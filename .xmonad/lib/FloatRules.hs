{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- FloatRules.hs
--------------------------------------------------------------------------------
module FloatRules
  ( -- * Default YAML path (no-arg version)
    floatManageHook          -- :: ManageHook
    -- * Custom YAML path (arg version)
  , makeFloatManageHook      -- :: FilePath -> ManageHook
    -- * Default path constant, exported for external use
  , defaultRulesPath
    -- * Internal types, exported in case they are needed
  , RuleTable
  , FieldMap
  , kindMap
  , fieldMap
  ) where

import XMonad
import XMonad.Hooks.ManageHelpers
import qualified Data.Map as M
-- sudo dnf install ghc-yaml ghc-yaml-devel
import qualified Data.Yaml as Y
import System.Directory (doesFileExist, getHomeDirectory)
import System.IO.Unsafe (unsafePerformIO)
import System.FilePath ((</>))

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------
type FieldMap  = M.Map String [String]
type RuleTable = M.Map String FieldMap

--------------------------------------------------------------------------------
-- Default YAML path
--------------------------------------------------------------------------------
defaultRulesPath :: FilePath
defaultRulesPath = unsafePerformIO $ do
    home <- getHomeDirectory
    pure (home </> ".xmonad" </> "float-rules.yaml")
{-# NOINLINE defaultRulesPath #-}

--------------------------------------------------------------------------------
-- Two lookup tables
--------------------------------------------------------------------------------
kindMap :: M.Map String ManageHook
kindMap = M.fromList
  [ ("doCenterFloat", doCenterFloat)
  , ("doFloat",       doFloat)
  , ("doIgnore",      doIgnore)
  , ("doFullFloat",   doFullFloat)
  ]

fieldMap :: M.Map String (String -> Query Bool)
fieldMap = M.fromList
  [ ("className", (className =?))
  , ("appName",   (appName   =?))
  , ("title",     (title     =?))
  ]

lookupKind :: String -> ManageHook
lookupKind k = M.findWithDefault doFloat k kindMap

lookupField :: String -> (String -> Query Bool)
lookupField f = M.findWithDefault (className =?) f fieldMap

--------------------------------------------------------------------------------
-- Read YAML
--------------------------------------------------------------------------------
loadRules :: FilePath -> RuleTable
loadRules path = unsafePerformIO $ do
    ok <- doesFileExist path
    if not ok
      then pure M.empty
      else do
        r <- Y.decodeFileEither path
        pure $ case r of
          Right m -> m
          Left  _ -> M.empty
{-# NOINLINE loadRules #-}

--------------------------------------------------------------------------------
-- Assemble
--------------------------------------------------------------------------------

-- | Arg version: use the given YAML path.
makeFloatManageHook :: FilePath -> ManageHook
makeFloatManageHook path = composeAll
  [ lookupField fieldName pattern --> lookupKind kindName
  | (kindName, fm)  <- M.toList (loadRules path)
  , (fieldName, ps) <- M.toList fm
  , pattern         <- ps
  ]

-- | No-arg version: use the default path. Equivalent to makeFloatManageHook defaultRulesPath.
floatManageHook :: ManageHook
floatManageHook = makeFloatManageHook defaultRulesPath
