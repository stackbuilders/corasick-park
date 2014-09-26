{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString (ByteString)
import Control.Concurrent (newMVar)
import Control.Monad.Trans (liftIO)
import System.Environment (lookupEnv)

import Text.Read (readMaybe)

import Snap (Handler, SnapletInit, serveSnaplet, defaultConfig, makeSnaplet,
             addRoutes)

import qualified Data.Cache.LRU as L

import Text.CorasickPark.Types
import Text.CorasickPark.Handler.Transform (transformHandler)
import Text.CorasickPark.Handler.Update (updateHandler)

import Text.Parsec.Prim hiding ((<|>))
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec ()

import Control.Applicative ((<|>), (<*))

maxBucketParser :: Parsec String () (Maybe Integer)
maxBucketParser = do
  (string "unlimited" >> return Nothing) <|> do
    digitStr <- many1 digit <* eof
    return $ Just (read digitStr :: Integer)

main :: IO ()
main = do
  maxBuckets <- lookupEnv "MAX_BUCKETS"

  let bucketLimit =
        case maxBuckets of
          Nothing -> Just 1000
          Just bs -> do
            case parse maxBucketParser "MAX_BUCKETS Environment Variable" bs of
              Left _ -> Just 1000
              Right numBuckets -> numBuckets

  let strMaxBuckets = case bucketLimit of
        Nothing -> "unlimited"
        Just x -> show x

  putStrLn $ "Starting server with max buckets: " ++ strMaxBuckets

  serveSnaplet defaultConfig (app bucketLimit)

app :: Maybe Integer -> SnapletInit App App
app buckets = makeSnaplet "app" "Corasick Park text substitution service" Nothing $ do
  addRoutes routes
  opMap <- liftIO $ newMVar $ L.newLRU buckets
  return $ App opMap

routes :: [(ByteString, Handler App App ())]
routes = [ ("/operations", updateHandler)
         , ("/transform",  transformHandler)]
