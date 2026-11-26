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
  , Kind (..)
  , Group (..)
  , declNodes
  , declGroups
  , Used (..)
  , usedIn
  , collect
  ) where

import Data.Data (Data, gmapQ)
import Data.Foldable (toList)
import Data.Graph (flattenSCC, stronglyConnComp)
import Data.List (nub, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe, maybeToList)
import Data.Ord (Down (..))
import Data.Set qualified as Set
import Data.Typeable (Typeable, cast)
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
  , nodeWith :: [String]
  -- ^ Names whose declaration it has to share a module with: the function
  -- for a signature, the class or type for an instance.
  , nodeUses :: [String]
  -- ^ Every unqualified name it mentions.
  , nodeFirstLine :: Int
  , nodeLastLine :: Int
  , nodeOrphan :: Bool
  -- ^ An instance whose class and types all come from other modules.
  }

-- | Declarations that have to be compiled together.
data Group = Group
  { groupId :: Int
  -- ^ The position of its first declaration, which identifies the group.
  , groupDeps :: [Int]
  -- ^ The other groups it mentions a name from.
  , groupNodes :: [Node]
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
      (kind, binds, with) <- classify decl
      let (first, final) = case locA ann of
            RealSrcSpan s _ -> (srcSpanStartLine s, srcSpanEndLine s)
            _ -> (0, 0)
      pure
        Node
          { nodeId = i
          , nodeKind = kind
          , nodeBinds = nub binds
          , nodeWith = nub with
          , nodeUses = nub $ namesIn modName decl
          , nodeFirstLine = first
          , nodeLastLine = final
          , nodeOrphan = False
          }

    classify :: HsDecl GhcPs -> Maybe (Kind, [String], [String])
    classify = \case
      ValD _ bind -> Just (Value, rdr <$> collectHsBindBinders CollNoDictBinders bind, [])
      TyClD _ d -> Just (TypeOrClass, tyClBinders d, [])
      InstD _ (ClsInstD _ inst) -> Just (Instance, [], instanceHead $ cid_poly_ty inst)
      InstD _ d@DataFamInstD{} -> Just (Instance, conBinders d, [])
      InstD _ _ -> Just (Instance, [], [])
      DerivD _ d -> Just (Instance, [], instanceHead . hswc_body $ deriv_type d)
      SigD _ sig -> Just (Attached, [], sigSubjects sig)
      KindSigD _ (StandaloneKindSig _ n _) -> Just (Attached, [], [located n])
      RoleAnnotD _ (RoleAnnotDecl _ n _) -> Just (Attached, [], [located n])
      WarningD _ d -> Just (Attached, [], namesIn modName d)
      ForD _ d@ForeignImport{} -> Just (Value, [located $ fd_name d], [])
      ForD _ _ -> Just (Other, [], [])
      SpliceD{} -> Just (Splice, [], [])
      DefD{} -> Nothing
      DocD{} -> Nothing
      _ -> Just (Other, [], [])

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
-- and fields, and for a class its methods and associated types.
tyClBinders :: TyClDecl GhcPs -> [String]
tyClBinders d = located (tyClDeclLName d) : conBinders d ++ methods
  where
    methods = case d of
      ClassDecl{tcdSigs = sigs, tcdATs = ats} ->
        concatMap (sigSubjects . unLoc) sigs ++ map (located . fdLName . unLoc) ats
      _ -> []

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
declGroups ns = sortOn (Down . groupLines) $ map group components
  where
    binders :: Map String [Int]
    binders = Map.fromListWith (flip (++)) [(b, [nodeId n]) | n <- ns, b <- nodeBinds n]

    lookupAll = concatMap (\name -> Map.findWithDefault [] name binders)

    host n = pick . filter (/= nodeId n) . lookupAll $ nodeWith n
      where
        pick = if nodeKind n == Instance then take 1 else nub

    back :: Map Int [Int]
    back = Map.fromListWith (++) [(h, [nodeId n]) | n <- ns, h <- host n]

    graph =
      [ (n, nodeId n, nub . filter (/= nodeId n) $ lookupAll (nodeUses n) ++ host n ++ Map.findWithDefault [] (nodeId n) back)
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
            , groupNodes = sorted
            , groupLines = sum [nodeLastLine n - nodeFirstLine n + 1 | n <- sorted]
            , groupNames = nub $ concatMap nodeBinds sorted
            }

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
  -- ^ Names written without a qualifier.
  , usedQualifiers :: Set.Set String
  -- ^ The qualifiers of names written with one.
  }

instance Semigroup Used where
  Used a b <> Used c d = Used (a <> c) (b <> d)

instance Monoid Used where
  mempty = Used mempty mempty

usedIn :: Data a => a -> Used
usedIn x = foldMap use (collect x :: [RdrName])
  where
    use = \case
      Unqual occ -> Used (Set.singleton $ occNameString occ) mempty
      Qual m occ -> Used (Set.singleton $ occNameString occ) (Set.singleton $ moduleNameString m)
      _ -> mempty

located :: GenLocated l RdrName -> String
located = rdr . unLoc

rdr :: RdrName -> String
rdr = occNameString . rdrNameOcc

-- | Every value of one type inside a piece of syntax.
collect :: forall a b. (Data a, Typeable b) => a -> [b]
collect x = maybeToList (cast x) ++ concat (gmapQ collect x)
