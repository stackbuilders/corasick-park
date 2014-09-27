{-# LANGUAGE OverloadedStrings #-}

module Text.CorasickPark.Algorithm
       ( OperationSet(..)
       , findAndApplyTransformations

       , replace
       , titleize
       , truncateTrailing

       , updateStateMachines
       ) where

import Control.Concurrent (MVar, takeMVar, putMVar)
import Data.List (intercalate, partition)

import qualified Text.Inflections as I
import Text.Inflections.Parse.Types (Word(..))

import Data.Char (toLower, toUpper)

import qualified Data.Cache.LRU as L

import Text.AhoCorasick

import Text.CorasickPark.Types

import Text.CorasickPark.Parser ( parseToStrings
                                , parseToMatchSegments
                                )


-- | Searches for all transformations in the given state machine
-- selectively applies the ones that match based on boundaries.
findAndApplyTransformations :: String -> MachineSet -> String
findAndApplyTransformations s stateMachines =
  foldr applyOperation s $ findOperations stateMachines s

replace :: String -- ^ Input text string
        -> Target -- ^ Target to match
        -> String -- ^ The string to replace
        -> String -- ^ The Text with substitutions applied
replace input tgt replacement =
  intercalate replacement $ parseToStrings input tgt

transformWith :: String -- ^ Input text string
              -> Target -- ^ Target to match
              -> (String -> String) -- ^ Function to apply to String
              -> String -- ^ String with transformation function applied
transformWith input tgt fn =
  intercalate withFn $ parseToStrings input tgt

  where withFn = fn (text tgt)

-- | Capitalizes the first word in each matched instance.
titleize :: String -- ^ Input text string
         -> Target -- ^ Target to match
         -> String -- ^ String with matching terms titleized
titleize input tgt =
  intercalate titleized $ parseToStrings input tgt

  where titleized = I.titleize $ map Word $ words (text tgt)

-- | Removes the text trailing each instance of the match.
truncateTrailing :: String -- ^ Input text string
                 -> Target -- ^ Target to match
                 -> String -- ^ String with text trailing matches removed
truncateTrailing input tgt =
  concatMap segmentToString $ zip [0..] $ parseToMatchSegments input tgt

  where
    segmentToString :: (Integer, MatchSegment) -> String
    segmentToString (i, Match prefix match) =
      if i == 0
      then prefix ++ match
      else match

    segmentToString (i, Remaining str) =
      if i == 0
      then str
      else ""


findOperations :: MachineSet -> String -> [Operation]
findOperations (caseSensitiveSMs, nonCaseSensitiveSMs) toTarget =
  let (caseSensitiveOps, nonCaseSensitiveOps) =
        ( findAll caseSensitiveSMs toTarget
        , findAll nonCaseSensitiveSMs (map toLower toTarget)) in

      uncurry (++) (map pVal caseSensitiveOps, map pVal nonCaseSensitiveOps)

updateStateMachines :: MVar OperationMachines -> OperationSet -> IO ()
updateStateMachines opmapvar
  (OperationSet { setName = sn, allOperations = ao }) = do

  let (caseSensitiveOps, nonCaseSensitiveOps) =
        partition (caseSensitive . target) ao


  existingOps <- takeMVar opmapvar
  _ <- putMVar opmapvar $ L.insert sn
       ( generateStateMachine caseSensitiveOps
       , generateStateMachine nonCaseSensitiveOps )
       existingOps

  return ()


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

applyOperation
  (Operation { target = tgt
             , transform = TruncateTrailing
             }) input = truncateTrailing input tgt

-- | Given a list of operations, update the state machines to find patterns.
generateStateMachine :: [Operation] -> StateMachine Char Operation
generateStateMachine ops =
  makeStateMachine $ map opToState ops

-- | Turns our Operation structure into a tuple that can be consumed by
-- the AhoCorasick library.
opToState :: Operation -> (String, Operation)
opToState op =
  (applyCase ((text . target) op), op)

  where applyCase str = if (caseSensitive . target) op
                          then str
                          else map toLower str
