{-# LANGUAGE OverloadedStrings #-}

module Text.CorasickPark.Handler.Update (updateHandler) where

import Snap (Handler, Method(..), method, gets)
import Snap.Core (modifyResponse, setResponseStatus)
import Control.Monad.Trans (liftIO)

import Snap.Extras.JSON (getJSON, writeJSON)

import Text.CorasickPark.Types
import Text.CorasickPark.Handler.Utils (errorMessage, successMessage)
import Text.CorasickPark.Algorithm (updateStateMachines)

updateHandler :: Handler App App ()
updateHandler = method POST setter
  where
    setter :: Handler App App ()
    setter = do
      res <- getJSON

      case res of
        Left err -> do
          modifyResponse $ setResponseStatus 422 "Invalid request"
          writeJSON $ errorMessage $ "Invalid request JSON: " ++ err

        Right newOpSet -> do
          opmapvar <- gets _operations
          _ <- liftIO $ updateStateMachines opmapvar newOpSet
          writeJSON $ successMessage "Operations inserted successfully."
          return ()
