{-# LANGUAGE OverloadedStrings #-}

-- | Development live-reload support
-- 
-- Include 'devReloadScript' in your HTML pages and add 'pingHandler'
-- to your routes. When the server restarts (after recompilation),
-- browsers will automatically reload.

module DevReload
  ( devReloadScript
  , pingHandler
  ) where

import Data.ByteString.Char8 (pack)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Lucid (Html, script_)
import Control.Monad.IO.Class (MonadIO)
import Snap.Core (MonadSnap, writeBS, modifyResponse, setContentType)
import System.IO.Unsafe (unsafePerformIO)

------------------------------------------------------------------------------
-- | Server start time (set once when module loads)
{-# NOINLINE serverStartTime #-}
serverStartTime :: Int
serverStartTime = unsafePerformIO $ round <$> getPOSIXTime

------------------------------------------------------------------------------
-- | Handler for /dev/ping - returns the server start timestamp
pingHandler :: (MonadSnap m, MonadIO m) => m ()
pingHandler = do
  modifyResponse $ setContentType "text/plain"
  writeBS $ pack $ show serverStartTime

------------------------------------------------------------------------------
-- | JavaScript to inject in HTML pages for auto-reload
--
-- How it works:
-- 1. Stores the server's start time on first poll
-- 2. Polls /dev/ping every second
-- 3. If start time changes (server restarted), reloads the page
-- 4. If server goes down and comes back, reloads the page
devReloadScript :: Html ()
devReloadScript = script_
  "(() => {\
  \  let startTime = null;\
  \  let wasDown = false;\
  \  setInterval(async () => {\
  \    try {\
  \      const r = await fetch('/dev/ping');\
  \      const t = await r.text();\
  \      if (startTime === null) {\
  \        startTime = t;\
  \        console.log('[dev] Connected, server time:', t);\
  \      } else if (startTime !== t || wasDown) {\
  \        console.log('[dev] Server restarted, reloading...');\
  \        location.reload();\
  \      }\
  \      wasDown = false;\
  \    } catch (e) {\
  \      if (!wasDown) console.log('[dev] Server down, waiting...');\
  \      wasDown = true;\
  \    }\
  \  }, 1000);\
  \})();"
