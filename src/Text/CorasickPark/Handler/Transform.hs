{-# LANGUAGE OverloadedStrings #-}

module Text.CorasickPark.Handler.Transform (transformHandler) where

import Text.CorasickPark.Types

import Snap (Handler, Method(..), method, gets)

import Control.Concurrent (readMVar)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Control.Monad.Trans (liftIO)

import Snap.Core (modifyResponse, setResponseStatus)
import Snap.Extras.JSON (getJSON, writeJSON)
import Data.Aeson

import qualified Data.Map.Strict as Map


import Text.CorasickPark.Handler.Utils (errorMessage, successMessage)
import Text.CorasickPark.Types ()
import Text.CorasickPark.Algorithm (transform)

data TransformRequest = TransformRequest { operationGroup :: String
                                         , toTransform :: String
                                         } deriving (Show, Eq)

instance FromJSON TransformRequest where
  parseJSON (Object v) = TransformRequest <$>
                         v .: "name" <*>
                         v .: "toTransform"
  parseJSON _          = mzero

instance ToJSON TransformRequest where
  toJSON (TransformRequest nm toTransformString) =
    object [ "name"        .= nm
           , "toTransform" .= toTransformString
           ]


transformHandler :: Handler App App ()
transformHandler = method POST transformer
  where
    transformer :: Handler App App ()
    transformer = do
      res <- getJSON

      case res of
        Left msg -> do
          modifyResponse $ setResponseStatus 422 "Invalid request"
          writeJSON $ errorMessage $ "Invalid request JSON: " ++ msg

        Right transformReq -> do
          opmapvar <- gets _operations
          ops      <- liftIO $ readMVar opmapvar

          case Map.lookup (operationGroup transformReq) ops of
            Nothing -> do
              modifyResponse $ setResponseStatus 404 "Not found"
              writeJSON $ errorMessage "Requested operation bucket not found."

            Just machines -> do
              let newString = transform (toTransform transformReq) machines
              writeJSON $ successMessage newString
