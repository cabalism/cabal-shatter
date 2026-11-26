-- | What the modules of a package bind, export and import, so that a
-- fragment of one module can import the fragments of another directly.
--
-- Like the rest of the splitter this works from syntax alone. It can say
-- where a name comes from when the name is bound in the package. When a
-- name could come from outside the package it says so, and the caller keeps
-- the import as it was written.
module Shatter.Index
  ( Index
  , Entry (..)
  , Info (..)
  , Bound (..)
  , Exports (..)
  , ImportSpec (..)
  , ImportList (..)
  , Found (..)
  , Package (..)
  , package
  , newtypes
  , importSpec
  , exportsOf
  , allows
  , picks
  , find
  , orphanModules
  , textualEntry
  ) where

import Data.Binary (Binary)
import Data.Char (isAlphaNum, isSpace)
import Data.List (delete, isPrefixOf)
import Data.Map.Lazy qualified as Lazy
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Generics (Generic)
import GHC
  ( GhcPs
  , IE (..)
  , IEWildcard (..)
  , ImportDecl (..)
  , ImportDeclQualifiedStyle (..)
  , ImportListInterpretation (..)
  , IsBootInterface (..)
  , LIE
  , ieName
  , moduleNameString
  , unLoc
  )
import GHC.Types.Name.Reader (RdrName)
import Shatter.Graph (Used (..), collect, rdr, usedIn)

-- | The modules of the package, by name.
type Index = Map String Entry

data Entry
  = -- | A module that is split.
    Known Info
  | -- | A module that is left whole because the splitter couldn't read it.
    -- Only what a scan of its text shows is known.
    Unknown
      { unknownImports :: [String]
      , unknownInstances :: Bool
      -- ^ Whether it declares any instances, which may be orphans.
      }
  deriving (Eq, Generic)

instance Binary Entry

data Info = Info
  { infoBinders :: Map String [Bound]
  , infoExports :: Exports
  , infoImports :: [ImportSpec]
  , infoOrphans :: [String]
  -- ^ The fragments that hold its orphan instances.
  }
  deriving (Eq, Generic)

instance Binary Info

-- | Where a name is bound. The fields after the first are those of
-- 'Shatter.Graph.Item'.
data Bound = Bound
  { boundModule :: String
  -- ^ The fragment that binds it.
  , boundHead :: Maybe String
  , boundType :: Bool
  , boundParent :: Maybe String
  , boundNewtype :: Bool
  }
  deriving (Eq, Ord, Generic)

instance Binary Bound

data Exports = Exports
  { exportsLocal :: Bool
  -- ^ Whether everything the module binds is exported: there is no export
  -- list, or the list names the module itself.
  , exportsNames :: Set String
  -- ^ The names in the list, including the ones in parentheses after a
  -- type.
  , exportsMembers :: Set String
  -- ^ Only the names in parentheses after a type.
  , exportsWild :: Set String
  -- ^ The types and classes listed with @(..)@.
  , exportsModules :: Set String
  -- ^ The other modules, or import qualifiers, listed as @module M@.
  , exportsWith :: Set (String, String)
  -- ^ Each type or class with a name in parentheses after it.
  }
  deriving (Eq, Generic)

instance Binary Exports

data ImportList
  = -- | No list: everything the module exports.
    Open
  | -- | The names listed, and whether any is listed with @(..)@.
    Only (Set String) Bool
  | -- | The names hidden, and those of them hidden with @(..)@, which
    -- hides what belongs to them as well.
    Hiding (Set String) (Set String)
  deriving (Eq, Generic)

instance Binary ImportList

data ImportSpec = ImportSpec
  { importModule :: String
  , importQualifier :: String
  -- ^ What its names can be qualified with: the alias, or the module name.
  , importQualifiedOnly :: Bool
  , importSource :: Bool
  , importList :: ImportList
  }
  deriving (Eq, Generic)

instance Binary ImportSpec

importSpec :: ImportDecl GhcPs -> ImportSpec
importSpec d =
  ImportSpec
    { importModule = name
    , importQualifier = maybe name (moduleNameString . unLoc) $ ideclAs d
    , importQualifiedOnly = ideclQualified d /= NotQualified
    , importSource = ideclSource d == IsBoot
    , importList = case ideclImportList d of
        Nothing -> Open
        Just (Exactly, items) -> Only (usedNames $ usedIn items) (any wild $ unLoc items)
        Just (EverythingBut, items) ->
          Hiding (usedNames $ usedIn items) (Set.fromList [rdr . ieName $ unLoc i | i <- unLoc items, wild i])
    }
  where
    name = moduleNameString . unLoc $ ideclName d

-- | Whether an import or export item ends in @(..)@.
wild :: LIE GhcPs -> Bool
wild item = case unLoc item of
  IEThingAll{} -> True
  ie -> any (/= NoIEWildcard) (collect ie :: [IEWildcard])

exportsOf :: String -> Maybe [LIE GhcPs] -> Exports
exportsOf _ Nothing = Exports True mempty mempty mempty mempty mempty
exportsOf self (Just items) = foldMap one items
  where
    one item = case unLoc item of
      IEModuleContents _ m
        | moduleNameString (unLoc m) == self -> Exports True mempty mempty mempty mempty mempty
        | otherwise -> Exports False mempty mempty mempty (Set.singleton . moduleNameString $ unLoc m) mempty
      ie
        | named ie ->
            let names = map rdr (collect ie :: [RdrName])
                self' = rdr $ ieName ie
             in Exports
                  False
                  (Set.fromList names)
                  (Set.fromList $ delete self' names)
                  (if wild item then Set.singleton self' else mempty)
                  mempty
                  (Set.fromList [(self', m) | m <- delete self' names])
        | otherwise -> mempty
    named = \case
      IEVar{} -> True
      IEThingAbs{} -> True
      IEThingAll{} -> True
      IEThingWith{} -> True
      _ -> False

instance Semigroup Exports where
  Exports a b c d e k <> Exports f g h i j l = Exports (a || f) (b <> g) (c <> h) (d <> i) (e <> j) (k <> l)

instance Monoid Exports where
  mempty = Exports False mempty mempty mempty mempty mempty

-- | Whether an import could bring a name into scope, going by its list.
allows :: ImportList -> String -> Bool
allows Open _ = True
allows (Only names anyWild) x = anyWild || x `Set.member` names
allows (Hiding names _) x = x `Set.notMember` names

-- | Whether a list that names things picks out this binding: by its own
-- name, or by the type it belongs to.
picks :: ImportList -> String -> Bound -> Bool
picks (Only names _) x b = x `Set.member` names || maybe False (`Set.member` names) (boundParent b)
picks (Hiding names heads) x b = x `Set.notMember` names && maybe True (`Set.notMember` heads) (boundParent b)
picks Open _ _ = True

-- | The result of looking for a name that a module exports.
data Found = Found
  { foundBounds :: [Bound]
  -- ^ Where in the package it is bound.
  , foundUnsure :: Bool
  -- ^ Whether it could instead come from outside the package, or through
  -- a module the splitter couldn't read.
  }

-- | The index together with what each module exports, worked out once.
data Package = Package
  { packageIndex :: Index
  , packageExports :: Map String Env
  , packageChildren :: Map (String, String) [String]
  -- ^ For a fragment and a type or class it binds: the constructors,
  -- fields or methods that belong to the type or class.
  }

package :: Index -> Package
package ix = Package ix envs children
  where
    children =
      Map.fromListWith
        (++)
        [ ((boundModule b, parent), [x])
        | (name, Known info) <- Map.toList ix
        , (x, bounds) <- Map.toList $ infoBinders info
        , b <- bounds
        , Just parent <- [boundParent b]
        , -- A fragment exports everything. A module left whole has its
          -- own export list, which may keep some of a type's members in.
          boundModule b /= name || lets (infoExports info) x parent
        ]

    lets ex x parent =
      exportsLocal ex
        || parent `Set.member` exportsWild ex
        || x `Set.member` exportsMembers ex
        -- A field or method can be exported on its own. A constructor
        -- can't, so one named after its type is not what the list means.
        || (x /= parent && x `Set.member` exportsNames ex)

    -- Each module's exports refer lazily to those of the modules it
    -- imports. Source imports are not followed, so there are no cycles.
    envs = Lazy.map (environment envs ix) ix

-- | What a module exports.
data Env = Env
  { envBound :: Map String [Bound]
  -- ^ The exported names that are bound in the package.
  , envOutside :: Outside
  -- ^ The exported names that could come from outside it.
  }

-- | Which names might come from outside the package.
data Outside = None | These (Set String) | Any

instance Semigroup Outside where
  Any <> _ = Any
  _ <> Any = Any
  None <> o = o
  o <> None = o
  These a <> These b = These (a <> b)

instance Monoid Outside where
  mempty = None

-- | Where a name exported by a module is bound.
find :: Package -> String -> String -> Found
find pkg modName x = case Map.lookup modName (packageExports pkg) of
  Nothing -> Found [] False
  Just env -> case Map.lookup x (envBound env) of
    Just bounds@(_ : _) -> Found bounds False
    _ -> Found [] $ case envOutside env of
      None -> False
      These names -> x `Set.member` names
      Any -> True

-- | The newtypes a module exports, by name.
newtypes :: Package -> String -> [(String, Bound)]
newtypes pkg modName =
  [ (x, b)
  | env <- maybe [] pure . Map.lookup modName $ packageExports pkg
  , (x, bounds) <- Map.toList $ envBound env
  , b <- bounds
  , boundNewtype b
  ]

-- | A name the module binds is exported if the export list lets it out.
-- Otherwise the module may re-export a name: by name, as part of a type
-- listed with @(..)@, or through @module M@. Those are followed into the
-- modules it imports.
environment :: Map String Env -> Index -> Entry -> Env
environment _ _ Unknown{} = Env mempty Any
environment envs ix (Known info) = Env bound outside
  where
    ex = infoExports info
    binders = infoBinders info

    local = Map.mapWithKey bundle . Map.filter (not . null) $ Map.mapWithKey (filter . exported) binders
    exported x b =
      exportsLocal ex
        || x `Set.member` exportsNames ex
        || maybe False (`Set.member` exportsWild ex) (boundParent b)

    -- An export list can attach a pattern synonym and its fields to a type
    -- of the module, as in @T (P, field, ..)@. Importing the module's @T (..)@
    -- then brings them in, but importing @T (..)@ from the fragment that
    -- binds @T@ would not. Such a type is marked as one this program can't
    -- write an import for, so imports that mention it are kept as written.
    bundle x bs
      | x `Set.member` bundling = [b{boundHead = Nothing} | b <- bs]
      | otherwise = bs
    bundling =
      Set.fromList
        [ t
        | (t, m) <- Set.toList $ exportsWith ex
        , t `Map.member` binders
        , not . any ((== Just t) . boundParent) $ Map.findWithDefault [] m binders
        ]

    byModule i = any (`Set.member` exportsModules ex) [importQualifier i, importModule i]
    home i = importModule i `Map.member` ix
    followed = [i | i <- infoImports info, not (importSource i)]

    -- Types listed with (..) that the module doesn't bind itself.
    wildHeads = Set.filter (`Map.notMember` binders) $ exportsWild ex

    -- What comes through one import of a module in the package.
    through i = case Map.lookup (importModule i) envs of
      Nothing -> mempty
      Just env ->
        Map.filter (not . null) $
          Map.mapWithKey
            ( \x ->
                filter
                  ( \b ->
                      picks (importList i) x b
                        && ( byModule i
                              || x `Set.member` exportsNames ex
                              || maybe False (`Set.member` wildHeads) (boundParent b)
                           )
                  )
            )
            (envBound env)

    -- A qualified import puts no unqualified name in scope, so a name in
    -- the export list can't mean one of its. A name exported with its
    -- qualifier is then not found here and counts as possibly from outside.
    reexported = Map.unionsWith (++) $ map through (filter (\i -> home i && not (importQualifiedOnly i)) followed)
    bound = Map.unionWith (++) local $ Map.filterWithKey (\x _ -> x `Map.notMember` binders) reexported

    -- A type listed with (..) that isn't found in the package comes from
    -- outside it, and so might anything that belongs to it.
    outsideHeads = Set.filter (`Map.notMember` bound) wildHeads

    outside =
      -- Names exported by name that weren't found in the package.
      named (Set.filter (`Map.notMember` bound) $ exportsNames ex)
        <> foldMap viaImport followed

    named names
      | Set.null names = None
      | otherwise = These names

    viaImport i
      | home i = if byModule i then restrict i (maybe None envOutside $ Map.lookup (importModule i) envs) else None
      | byModule i = restrict i Any
      | any (allows $ importList i) outsideHeads = restrict i Any
      | otherwise = None

    -- An import that lists names without (..) can only bring those in.
    restrict i o = case (importList i, o) of
      (_, None) -> None
      (Only names False, Any) -> These names
      (Only names False, These others) -> These $ Set.intersection names others
      _ -> o

-- | The modules to import so that the orphan instances visible through a
-- module stay visible: the orphan fragments of every module it reaches, and
-- every module it reaches that couldn't be read and declares instances.
orphanModules :: Index -> String -> [String]
orphanModules ix = Set.toList . snd . go (Set.empty, Set.empty)
  where
    go (seen, acc) m
      | m `Set.member` seen = (seen, acc)
      | otherwise = case Map.lookup m ix of
          Nothing -> (seen, acc)
          Just (Known info) ->
            foldl go (Set.insert m seen, acc <> Set.fromList (infoOrphans info)) $
              map importModule (infoImports info)
          Just (Unknown imports instances) ->
            foldl go (Set.insert m seen, if instances then Set.insert m acc else acc) imports

-- | The name of a module and an 'Unknown' entry for it, from a scan of its
-- text.
textualEntry :: String -> Maybe (String, Entry)
textualEntry src = do
  name <- listToMaybe . mapMaybe (keyword "module") $ lines src
  pure
    ( name
    , Unknown
        { unknownImports = mapMaybe (keyword "import") $ lines src
        , unknownInstances = any (\l -> any (`isPrefixOf` l) ["instance ", "deriving instance "]) $ lines src
        }
    )
  where
    keyword k l = case words l of
      w : rest | w == k -> listToMaybe . filter (not . null) . map moduleName $ dropWhile skip rest
      _ -> Nothing
    skip w = w `elem` ["qualified", "safe", "{-#", "SOURCE", "#-}"] || "\"" `isPrefixOf` w
    moduleName = takeWhile (\c -> isAlphaNum c || c `elem` "._'") . dropWhile isSpace
