{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString.Char8 as BS
import Snap.Http.Server
  ( defaultConfig
  , setPort
  , setAccessLog
  , setErrorLog
  , ConfigLog(..)
  )
import Snap.Snaplet (serveSnaplet)

import Application (appInit)

------------------------------------------------------------------------------
-- | Main entry point
main :: IO ()
main = do
  putStrLn "📚 Library Director Server"
  putStrLn "Starting on http://localhost:8000"
  putStrLn "Press Ctrl+C to stop"
  putStrLn ""

  let config =
        setPort 8000 $
        setAccessLog (ConfigIoLog BS.putStrLn) $
        setErrorLog (ConfigIoLog BS.putStrLn) $
        defaultConfig

  serveSnaplet config appInit
