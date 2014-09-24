{-# LANGUAGE OverloadedStrings #-}

module Text.CorasickPark.Algorithm
       ( OperationSet(..)
       , findAndApplyTransformations
       , updateStateMachines
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

-- | Searches for all transformations in the given state machine
-- selectively applies the ones that match based on boundaries.
findAndApplyTransformations :: String
                            -> ( StateMachine Char Operation
                               , StateMachine Char Operation)
                            -> String
findAndApplyTransformations s stateMachines =
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
       ( generateStateMachine caseSensitiveOps
       , generateStateMachine nonCaseSensitiveOps )
       existingOps

  return ()

  where isCaseSensitive (Operation { target = Target {
                                        caseSensitive = True } }) = True
        isCaseSensitive (Operation { target = Target {
                                        caseSensitive = False } }) = False


applyOperation :: Operation -> String -> String
applyOperation
  (Operation { target =
                  Target { text          = targetText
                         , caseSensitive = isCaseSensitive
                         , leftBoundary  = lboundary
                         , rightBoundary = rboundary  }

             , global    = isGlobal
             , transform = Replace replacement
             }) input =

  replace input targetText replacement (lboundary, rboundary)
    isCaseSensitive isGlobal

-- | Given a list of operations, update the state machines to find patterns.
generateStateMachine :: [Operation] -> StateMachine Char Operation
generateStateMachine ops =
  makeStateMachine $ map opToState ops

-- | Turns our Operation structure into a tuple that can be consumed by
-- the AhoCorasick library.
opToState :: Operation -> (String, Operation)
opToState op =
  (applyCase ((text . target) op), op)

  where applyCase str = if ((caseSensitive . target) op)
                          then str
                          else map toLower str
