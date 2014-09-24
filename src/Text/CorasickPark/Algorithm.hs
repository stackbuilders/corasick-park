{-# LANGUAGE OverloadedStrings #-}

module Text.CorasickPark.Algorithm
       ( OperationSet(..)
       , findAndApplyTransformations
       , updateStateMachines
       ) where

import Control.Concurrent (MVar, takeMVar, putMVar)

import Data.Char (toLower, toUpper)
import Data.List (partition)

import qualified Data.Map.Strict as Map

import Text.AhoCorasick
import Text.CorasickPark.Types

import Text.Inflections.Parse.Types (Word(..))

import Text.CorasickPark.Parser (replace, transformWith, titleize)


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
  (Operation { target = tgt
             , transform = Replace replacement
             }) input = replace input tgt replacement

applyOperation
  (Operation { target = tgt
             , transform = Upcase
             }) input = transformWith input tgt (map toUpper)

applyOperation
  (Operation { target = tgt
             , transform = Downcase
             }) input = transformWith input tgt (map toLower)

applyOperation
  (Operation { target = tgt
             , transform = Titleize
             }) input = titleize input tgt

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
