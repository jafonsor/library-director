{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Application
  ( App(..)
  , appName  -- generated lens
  , AppHandler
  , appInit
  ) where

import Control.Lens (makeLenses)
import Snap.Snaplet (Handler, makeSnaplet, addRoutes, SnapletInit)

import Site (routes)

------------------------------------------------------------------------------
-- | Application state type
--
-- This record holds:
--   1. Application-level state (config, caches, etc.)
--   2. Nested snaplets (each wrapped in 'Snaplet')
--
-- Example with snaplets:
--   data App = App
--     { _appConfig :: Config          -- your app state
--     , _heist     :: Snaplet (Heist App)
--     , _sess      :: Snaplet SessionManager
--     , _db        :: Snaplet Postgres
--     }
--
-- Handlers access state via lenses: `use appConfig`, `with db $ ...`
data App = App
  { _appName :: String
  }

makeLenses ''App


------------------------------------------------------------------------------
-- | Convenience type alias for handlers in this application
type AppHandler = Handler App App

------------------------------------------------------------------------------
-- | Initialize the application snaplet
appInit :: SnapletInit App App
appInit = makeSnaplet "library-director" "Library Director Application" Nothing $ do
  addRoutes routes
  return $ App "Library Director"
