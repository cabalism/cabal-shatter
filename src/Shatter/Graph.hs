{-# LANGUAGE ScopedTypeVariables #-}

-- | The dependency graph between a module's top-level declarations, and the
-- groups the module would split into.
--
-- The analysis is syntactic. A declaration depends on another when it
-- mentions a name the other binds. Nothing is renamed, so a local variable
-- that shadows a top-level name counts as a mention. That can only add
-- dependencies, which makes groups larger but never wrong.
module Shatter.Graph
  ( Node (..)
  , Item (..)
  , Kind (..)
  , Group (..)
  , declNodes
  , declGroups
  , fragmentNames
  , costed
  , pack
  , Used (..)
  , usedQualifiers
  , usedIn
  , rdr
  , wrap
  , collect
  ) where

import Data.Char (isAlphaNum, ord)
import Data.Data (Data, gmapQ)
import Data.Foldable (toList)
import Data.Graph (flattenSCC, stronglyConnComp)
import Data.List (nub, partition, sort, sortOn)
import Data.Map.Lazy qualified as Lazy
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe, maybeToList)
import Data.Ord (Down (..))
import Data.Set qualified as Set
import Data.Typeable (Typeable, cast)
import Numeric (showHex)
import GHC.Hs
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (RdrName (..), rdrNameOcc)
import GHC.Types.SrcLoc (GenLocated (..), SrcSpan (..), srcSpanEndLine, srcSpanStartLine, unLoc)

-- | What sort of declaration a node is.
data Kind
  = -- | A function or pattern binding.
    Value
  | -- | A data type, newtype, synonym, class or family.
    TypeOrClass
  | -- | A class instance or a standalone deriving declaration.
    Instance
  | -- | A signature, fixity declaration or pragma that names a binder.
    Attached
  | -- | Anything else that stays in the module, such as a rewrite rule.
    Other
  | -- | A top-level Template Haskell splice, which the splitter refuses.
    Splice
  deriving (Eq, Show)

-- | One top-level declaration.
data Node = Node
  { nodeId :: Int
  -- ^ Position among the module's declarations.
  , nodeKind :: Kind
  , nodeBinds :: [String]
  -- ^ The names it brings into scope at the top level.
  , nodeItems :: [Item]
  -- ^ The same names, with how another module would import each.
  , nodeWith :: [String]
  -- ^ Names whose declaration it has to share a module with: the function
  -- for a signature, the class or type for an instance.
  , nodeUses :: [String]
  -- ^ Every unqualified name it mentions.
  , nodeFirstLine :: Int
  , nodeLastLine :: Int
  , nodeOrphan :: Bool
  -- ^ An instance whose class and types all come from other modules.
  , nodeDerives :: Bool
  -- ^ Whether it is an instance or has a deriving clause. Derived code
  -- costs far more to compile than its line count suggests.
  }

-- | A name a declaration binds.
data Item = Item
  { itemName :: String
  , itemHead :: Maybe String
  -- ^ The name to write in an import list to bring it into scope: its own
  -- name for a function, the type or class for anything that belongs to
  -- one. 'Nothing' when it needs syntax this program doesn't write, such
  -- as a pattern synonym.
  , itemType :: Bool
  -- ^ Whether 'itemHead' is a type or class, which is imported with what
  -- belongs to it.
  , itemParent :: Maybe String
  -- ^ The type or class a constructor, field or method belongs to.
  , itemNewtype :: Bool
  -- ^ Whether it is a newtype. Coercing through a newtype needs its
  -- constructor in scope, and nothing in the source says so.
  }

-- | Declarations that have to be compiled together.
data Group = Group
  { groupId :: Int
  -- ^ The position of its first declaration, which identifies the group.
  , groupDeps :: [Int]
  -- ^ The other groups it mentions a name from.
  , groupOrphan :: [String]
  -- ^ For a group that is an orphan instance, the names in the instance
  -- head; otherwise empty.
  , groupNodes :: [Node]
  , groupCost :: Double
  -- ^ How long it takes to compile, in milliseconds; see 'costed'. Zero
  -- until then.
  , groupLines :: Int
  , groupNames :: [String]
  -- ^ The names the group binds, in source order.
  }

-- | One node per top-level declaration. Documentation comments and
-- @default@ declarations are left out: the first travel with the
-- declaration they describe and the second belongs in every fragment.
declNodes :: String -> [LHsDecl GhcPs] -> [Node]
declNodes modName = markOrphans . mapMaybe node . zip [0 ..]
  where
    node (i, L ann decl) = do
      (kind, items, with) <- classify decl
      let (first, final) = case locA ann of
            RealSrcSpan s _ -> (srcSpanStartLine s, srcSpanEndLine s)
            _ -> (0, 0)
      pure
        Node
          { nodeId = i
          , nodeKind = kind
          , nodeBinds = nub $ map itemName items
          , nodeItems = items
          , nodeWith = nub with
          , nodeUses = nub $ namesIn modName decl
          , nodeFirstLine = first
          , nodeLastLine = final
          , nodeOrphan = False
          , nodeDerives = kind == Instance || not (null (collect decl :: [HsDerivingClause GhcPs]))
          }

    classify :: HsDecl GhcPs -> Maybe (Kind, [Item], [String])
    classify = \case
      ValD _ bind@PatSynBind{} -> Just (Value, unimportable . rdr <$> collectHsBindBinders CollNoDictBinders bind, [])
      ValD _ bind -> Just (Value, value . rdr <$> collectHsBindBinders CollNoDictBinders bind, [])
      TyClD _ d -> Just (TypeOrClass, tyClItems d, [])
      InstD _ (ClsInstD _ inst) -> Just (Instance, [], instanceHead $ cid_poly_ty inst)
      InstD _ d@DataFamInstD{} -> Just (Instance, unimportable <$> conBinders d, [])
      InstD _ _ -> Just (Instance, [], [])
      DerivD _ d -> Just (Instance, [], instanceHead . hswc_body $ deriv_type d)
      SigD _ sig -> Just (Attached, [], sigSubjects sig)
      KindSigD _ (StandaloneKindSig _ n _) -> Just (Attached, [], [located n])
      RoleAnnotD _ (RoleAnnotDecl _ n _) -> Just (Attached, [], [located n])
      WarningD _ d -> Just (Attached, [], namesIn modName d)
      ForD _ d@ForeignImport{} -> Just (Value, [value . located $ fd_name d], [])
      ForD _ _ -> Just (Other, [], [])
      SpliceD{} -> Just (Splice, [], [])
      DefD{} -> Nothing
      DocD{} -> Nothing
      _ -> Just (Other, [], [])

    value n = Item n (Just n) False Nothing False
    unimportable n = Item n Nothing False Nothing False

    -- The class comes first, so that an instance joins its class when the
    -- module defines it and otherwise the first type in the head that the
    -- module defines. The context is left out: a constraint synonym in it
    -- is not somewhere the instance can live.
    instanceHead ty =
      (located <$> maybeToList (getLHsInstDeclClass_maybe ty)) ++ namesIn modName (getLHsInstDeclHead ty)

    -- An instance is an orphan when no name in its head is bound here.
    markOrphans ns =
      [ n{nodeOrphan = nodeKind n == Instance && null (nodeBinds n) && not (any (`Set.member` bound) (nodeWith n))}
      | n <- ns
      ]
      where
        bound = Set.fromList $ concatMap nodeBinds ns

-- | The names a type or class declaration binds: itself, its constructors
-- and fields, and for a class its methods and associated types. All of them
-- are imported through the type or class.
tyClItems :: TyClDecl GhcPs -> [Item]
tyClItems d = Item name entry True Nothing isNewtype : [Item c entry True (Just name) False | c <- conBinders d ++ methods]
  where
    name = located $ tyClDeclLName d
    entry
      | all isNameChar name = Just name
      | otherwise = Nothing
    isNewtype = case d of
      DataDecl{tcdDataDefn = HsDataDefn{dd_cons = NewTypeCon{}}} -> True
      _ -> False
    methods = case d of
      ClassDecl{tcdSigs = sigs, tcdATs = ats} ->
        concatMap (sigSubjects . unLoc) sigs ++ map (located . fdLName . unLoc) ats
      _ -> []

-- | A name as it is written in an import list.
wrap :: String -> String
wrap n
  | all isNameChar n = n
  | otherwise = "(" ++ n ++ ")"

isNameChar :: Char -> Bool
isNameChar c = isAlphaNum c || c == '_' || c == '\''

-- | Constructor and field names anywhere inside a declaration.
conBinders :: Data a => a -> [String]
conBinders d = concatMap con (collect d :: [ConDecl GhcPs])
  where
    con c = conNames c ++ map (located . foLabel) (collect c :: [FieldOcc GhcPs])
    conNames = \case
      ConDeclH98{con_name = n} -> [located n]
      ConDeclGADT{con_names = ns} -> located <$> toList ns

-- | The binders a signature or pragma is about.
sigSubjects :: Sig GhcPs -> [String]
sigSubjects = \case
  TypeSig _ _ ns _ -> located <$> ns
  PatSynSig _ ns _ -> located <$> ns
  ClassOpSig _ _ ns _ -> located <$> ns
  FixSig _ (FixitySig _ ns _) -> located <$> ns
  InlineSig _ n _ -> [located n]
  SpecSig _ n _ _ -> [located n]
  SCCFunSig _ n _ -> [located n]
  _ -> []

-- | Group the nodes into strongly connected components, largest first.
--
-- A node points at every node that binds a name it uses. A node and the
-- declaration it has to sit with point at each other, which forces them
-- into one group. An instance joins only the first declaration that binds a
-- name in 'nodeWith', so its class in preference to its type. A signature
-- or pragma joins all of them, since @f, g :: T@ belongs with both.
declGroups :: [Node] -> [Group]
declGroups ns = sortOn (Down . groupLines) . orphanDeps $ map group components
  where
    binders :: Map String [Int]
    binders = Map.fromListWith (flip (++)) [(b, [nodeId n]) | n <- ns, b <- nodeBinds n]

    lookupAll = concatMap (\name -> Map.findWithDefault [] name binders)

    host n = pick . filter (/= nodeId n) . lookupAll $ nodeWith n
      where
        pick = if nodeKind n == Instance then take 1 else nub

    back :: Map Int [Int]
    back = Map.fromListWith (++) [(h, [nodeId n]) | n <- ns, h <- host n]

    -- Orphan instances can use one another and nothing in the source says
    -- so. They point at each other, which puts them all in one group.
    orphans = [nodeId n | n <- ns, nodeOrphan n]

    graph =
      [ ( n
        , nodeId n
        , nub . filter (/= nodeId n) $
            lookupAll (nodeUses n)
              ++ host n
              ++ Map.findWithDefault [] (nodeId n) back
              ++ (if nodeOrphan n then orphans else [])
        )
      | n <- ns
      ]

    edges :: Map Int [Int]
    edges = Map.fromList [(k, out) | (_, k, out) <- graph]

    owner :: Map Int Int
    owner = Map.fromList [(nodeId n, minimum $ map nodeId members) | members <- components, n <- members]

    components = map flattenSCC $ stronglyConnComp graph

    group members =
      let sorted = sortOn nodeId members
          gid = nodeId $ minimum' sorted
          minimum' = foldr1 (\a b -> if nodeId a <= nodeId b then a else b)
       in Group
            { groupId = gid
            , groupDeps =
                nub
                  . filter (/= gid)
                  . mapMaybe (`Map.lookup` owner)
                  $ concatMap (\n -> Map.findWithDefault [] (nodeId n) edges) sorted
            , groupOrphan = concat [nodeWith n | n <- sorted, nodeOrphan n]
            , groupNodes = sorted
            , groupCost = 0
            , groupLines = sum [nodeLastLine n - nodeFirstLine n + 1 | n <- sorted]
            , groupNames = nub $ concatMap nodeBinds sorted
            }

-- | A module name for each group, from the least name it binds. Naming by
-- binder keeps the other fragments' names the same when a declaration is
-- added or moved.
fragmentNames :: String -> [Group] -> Map Int String
fragmentNames modName groups = Map.fromList $ concatMap number byName
  where
    byName = Map.toList $ Map.fromListWith (flip (++)) [(base g, [groupId g]) | g <- groups]
    number (name, [gid]) = [(gid, name)]
    number (name, gids) = [(gid, name ++ "_" ++ show n) | (gid, n) <- zip gids [1 :: Int ..]]
    base g = modName ++ ".S_" ++ case sort (groupNames g) of
      n : _
        | all isNameChar n -> n
        | otherwise -> "op_" ++ concatMap (flip showHex "" . ord) n
      []
        | not (null $ groupOrphan g) -> "orphans"
        | otherwise -> "line" ++ show (minimum . map nodeFirstLine $ groupNodes g)

-- | Give each group its compile time in milliseconds, and say how many
-- were measured.
--
-- The times come from an earlier build in which every group was its own
-- fragment, by fragment name. A group with no measurement gets an estimate
-- from its size: 0.4 ms a line, or 1.35 ms a line for a declaration that
-- is an instance or derives one. Those are the averages over cabal-install
-- at @-O0@, and are only a rough guide elsewhere.
costed :: Map String Double -> String -> [Group] -> ([Group], Int)
costed costs modName gs = (map fst priced, length $ filter snd priced)
  where
    names = fragmentNames modName gs
    priced = map price gs
    price g = case (`Map.lookup` costs) =<< Map.lookup (groupId g) names of
      Just ms -> (g{groupCost = ms}, True)
      Nothing -> (g{groupCost = sum $ map estimate (groupNodes g)}, False)
    estimate n = fromIntegral (nodeLastLine n - nodeFirstLine n + 1) * (if nodeDerives n then 1.35 else 0.4)

-- | Pack groups into fragments that take at least a given time to compile,
-- in milliseconds, so that a module doesn't turn into dozens of small ones.
-- Every module costs GHC something whether or not it is recompiled.
--
-- Groups are packed by level: a group's level is one more than the highest
-- level it depends on, so groups on one level don't depend on each other
-- and any of them can share a fragment. Whole levels can be joined too.
-- Either way the fragments can't end up depending on each other in a cycle.
--
-- * A level that takes at least that long is packed on its own: each group
--   that is expensive enough stays alone, and the rest fill fragments in
--   source order.
-- * Cheaper levels are joined to the levels above them until together they
--   take long enough, and become one fragment.
pack :: Double -> [Group] -> [Group]
pack ms gs
  | ms <= 0 = gs
  | otherwise = sortOn (Down . groupLines) $ map merge fragments
  where
    byId = Map.fromList [(groupId g, g) | g <- gs]
    -- Each level refers to the levels below it, so the map has to be lazy.
    level = Lazy.map (\g -> 1 + maximum (0 : mapMaybe (`Map.lookup` level) (groupDeps g))) byId :: Map Int Int
    levels = Map.elems $ Map.fromListWith (flip (++)) [(level Map.! groupId g, [g]) | g <- sortOn groupId gs]
    cost = sum . map groupCost

    fragments = bands [] levels

    bands held [] = [held | not (null held)]
    bands held (l : ls)
      | cost l >= ms = [held | not (null held)] ++ fill l ++ bands [] ls
      | cost (held ++ l) >= ms = (held ++ l) : bands [] ls
      | otherwise = bands (held ++ l) ls

    fill l =
      let (large, small) = partition ((>= ms) . groupCost) l
       in map pure large ++ bins [] small

    bins held [] = [held | not (null held)]
    bins held (g : rest)
      | cost (held ++ [g]) >= ms && cost rest >= ms = (held ++ [g]) : bins [] rest
      | otherwise = bins (held ++ [g]) rest

    owner = Map.fromList [(groupId g, minimum $ map groupId f) | f <- fragments, g <- f]

    merge f =
      let gid = minimum $ map groupId f
          members = sortOn groupId f
       in Group
            { groupId = gid
            , groupDeps = nub . filter (/= gid) . mapMaybe (`Map.lookup` owner) $ concatMap groupDeps members
            , groupOrphan = concatMap groupOrphan members
            , groupNodes = sortOn nodeId $ concatMap groupNodes members
            , groupCost = cost members
            , groupLines = sum $ map groupLines members
            , groupNames = concatMap groupNames members
            }

-- | Make an orphan instance visible to the rest of the module.
--
-- In the original module every declaration can see an orphan instance. No
-- name says which ones need it, so every group imports the orphan's group,
-- except the groups the orphan itself depends on, which can't without a
-- cycle. A module's orphans are all in one group, so there is one to add.
orphanDeps :: [Group] -> [Group]
orphanDeps gs = Map.elems $ foldl add (Map.fromList [(groupId g, g) | g <- gs]) orphans
  where
    orphans = [groupId g | g <- sortOn groupId gs, not . null $ groupOrphan g]
    add m o =
      let below = reach m Set.empty . groupDeps $ m Map.! o
          see g
            | groupId g == o || groupId g `Set.member` below = g
            | otherwise = g{groupDeps = o : groupDeps g}
       in Map.map see m
    reach _ seen [] = seen
    reach m seen (x : xs)
      | x `Set.member` seen = reach m seen xs
      | otherwise = reach m (Set.insert x seen) $ maybe [] groupDeps (Map.lookup x m) ++ xs

-- | Every name mentioned in a piece of syntax that could refer to a
-- top-level binder of this module: unqualified names, and names qualified
-- with the module's own name.
namesIn :: Data a => String -> a -> [String]
namesIn modName x = mapMaybe local (collect x :: [RdrName])
  where
    local = \case
      Unqual occ -> Just $ occNameString occ
      Qual m occ | moduleNameString m == modName -> Just $ occNameString occ
      _ -> Nothing

-- | The names a piece of syntax mentions, for deciding which imports it
-- could need.
data Used = Used
  { usedNames :: Set.Set String
  -- ^ Every name, without its qualifier if it has one.
  , usedUnqualified :: Set.Set String
  -- ^ The names written without a qualifier.
  , usedQualified :: Set.Set (String, String)
  -- ^ The names written with a qualifier, as qualifier and name.
  }

instance Semigroup Used where
  Used a b c <> Used d e f = Used (a <> d) (b <> e) (c <> f)

instance Monoid Used where
  mempty = Used mempty mempty mempty

-- | The qualifiers that are written.
usedQualifiers :: Used -> Set.Set String
usedQualifiers = Set.map fst . usedQualified

usedIn :: Data a => a -> Used
usedIn x = foldMap use (collect x :: [RdrName])
  where
    use = \case
      Unqual occ -> let n = occNameString occ in Used (Set.singleton n) (Set.singleton n) mempty
      Qual m occ -> let n = occNameString occ in Used (Set.singleton n) mempty (Set.singleton (moduleNameString m, n))
      _ -> mempty

located :: GenLocated l RdrName -> String
located = rdr . unLoc

rdr :: RdrName -> String
rdr = occNameString . rdrNameOcc

-- | Every value of one type inside a piece of syntax.
collect :: forall a b. (Data a, Typeable b) => a -> [b]
collect x = maybeToList (cast x) ++ concat (gmapQ collect x)
