module Text.CorasickPark.Handler.Utils (errorMessage, successMessage) where

import qualified Data.Map.Strict as Map

errorMessage :: String -> Map.Map String String
errorMessage msg = Map.fromList [("error", msg)]

successMessage :: String -> Map.Map String String
successMessage msg = Map.fromList [("result", msg)]
