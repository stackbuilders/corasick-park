{-# LANGUAGE OverloadedStrings #-}

module Text.CorasickPark.Algorithm
       ( OperationSet(..),
         updateStateMachines
       , transform
       ) where

import Control.Concurrent (MVar, takeMVar, putMVar)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)

import Data.Char (toLower)
import Data.List (partition)

import Data.Aeson

import qualified Data.Map.Strict as Map

import Text.AhoCorasick
import Text.CorasickPark.Types
import Text.CorasickPark.Parser (replace)

data OperationSet = OperationSet { setName       :: !String
                                 , allOperations :: ![Operation]
                                 } deriving (Show, Eq)

instance FromJSON OperationSet where
  parseJSON (Object v) = OperationSet <$>
                         v .: "name" <*>
                         v .: "operations"
  parseJSON _          = mzero

instance ToJSON OperationSet where
  toJSON (OperationSet nm allOps) =
    object [ "name"       .= nm
           , "operations" .= allOps
           ]

transform :: String
          -> (StateMachine Char Operation, StateMachine Char Operation)
          -> String
transform s stateMachines =
  let allOps = findOperations stateMachines s in
    foldr applyOperation s $ uncurry (++) allOps

findOperations :: (StateMachine Char Operation, StateMachine Char Operation)
               -> String
               -> ([Operation], [Operation])
findOperations (caseSensitiveSMs, nonCaseSensitiveSMs) toTarget =
  let (caseSensitiveOps, nonCaseSensitiveOps) =
        ( findAll caseSensitiveSMs toTarget
        , findAll nonCaseSensitiveSMs (map toLower toTarget)) in

      (map pVal caseSensitiveOps, map pVal nonCaseSensitiveOps)

updateStateMachines :: MVar OperationMachines -> OperationSet -> IO ()
updateStateMachines opmapvar
  (OperationSet { setName = sn, allOperations = ao }) = do

  let (caseSensitiveOps, nonCaseSensitiveOps) = partition isCaseSensitive ao


  existingOps <- takeMVar opmapvar
  _ <- putMVar opmapvar $ Map.insert sn
       ( generateStateMachine caseSensitiveOps True
       , generateStateMachine nonCaseSensitiveOps False )
       existingOps

  return ()

  where isCaseSensitive (Operation { matchType = MatchType {
                                        caseSensitive = True } }) = True
        isCaseSensitive (Operation { matchType = MatchType {
                                        caseSensitive = False } }) = False


applyOperation :: Operation -> String -> String
applyOperation (Operation { target = t
                          , replacement = r
                          , matchType = mt
                          }) s =

  replace s t r (leftBoundary mt, rightBoundary mt)
                (caseSensitive mt) (global mt)

-- | Given a list of operations, update the state machines to find patterns.
generateStateMachine :: [Operation] -> Bool -> StateMachine Char Operation
generateStateMachine ops isCaseSensitive =
  makeStateMachine $ map (opToState isCaseSensitive) ops

-- | Turns our Operation structure into a tuple that can be consumed by
-- the AhoCorasick library.
opToState :: Bool -> Operation -> (String, Operation)
opToState isCaseSensitive (Operation tpe toTarget toReplace) =
  (applyCase toTarget, Operation tpe toTarget toReplace)

  where applyCase str = if isCaseSensitive then str else map toLower str
