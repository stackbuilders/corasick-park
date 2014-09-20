{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString (ByteString)
import Control.Concurrent (newMVar)
import Control.Monad.Trans (liftIO)

import Snap (Handler, SnapletInit, serveSnaplet, defaultConfig, makeSnaplet,
             addRoutes)

import qualified Data.Map.Strict as Map

import Text.CorasickPark.Types
import Text.CorasickPark.Handler.Transform (transformHandler)
import Text.CorasickPark.Handler.Update (updateHandler)

main :: IO ()
main = do
  serveSnaplet defaultConfig app

app :: SnapletInit App App
app = makeSnaplet "app" "Corasick Park text substitution service" Nothing $ do
  addRoutes routes
  opMap <- liftIO $ newMVar Map.empty
  return $ App opMap

routes :: [(ByteString, Handler App App ())]
routes = [ ("/operations", updateHandler)
         , ("/transform",  transformHandler)]
