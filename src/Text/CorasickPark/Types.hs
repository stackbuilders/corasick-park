{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.CorasickPark.Types where

import Data.Aeson
import Control.Concurrent (MVar)
import Control.Monad (mzero)
import Control.Applicative ((<$>), (<*>))
import Text.AhoCorasick (StateMachine)
import Control.Lens.TH (makeLenses)
import Snap (Handler)

import qualified Data.Map.Strict as Map

data BoundaryType = NoBoundary
                  | WordBoundary
                  | LineBoundary
                  | InputBoundary
                  deriving (Show, Eq)

instance FromJSON BoundaryType where
  parseJSON (String "none")  = return NoBoundary
  parseJSON (String "word")  = return WordBoundary
  parseJSON (String "line")  = return LineBoundary
  parseJSON (String "input") = return InputBoundary
  parseJSON _                = mzero

instance ToJSON BoundaryType where
  toJSON NoBoundary    = "none"
  toJSON WordBoundary  = "word"
  toJSON LineBoundary  = "line"
  toJSON InputBoundary = "input"


type CaseSensitive = Bool

data MatchType = MatchType { caseSensitive :: !Bool
                           , global        :: !Bool
                           , leftBoundary  :: !BoundaryType
                           , rightBoundary :: !BoundaryType
                           } deriving (Show, Eq)

instance FromJSON MatchType where
  parseJSON (Object v) = MatchType <$>
                         v .: "isCaseSensitive" <*>
                         v .: "isGlobal" <*>
                         v .: "leftBoundaryType" <*>
                         v .: "rightBoundaryType"
  parseJSON _          = mzero

instance ToJSON MatchType where
     toJSON (MatchType caseSens isGlob lBoundary rBoundary) =
       object [ "isCaseSensitive"   .= caseSens
              , "isGlobal"          .= isGlob
              , "leftBoundaryType"  .= lBoundary
              , "rightBoundaryType" .= rBoundary
              ]

data Operation = Operation { matchType   :: !MatchType
                           , target      :: !String
                           , replacement :: !String
                           } deriving (Show, Eq)


instance FromJSON Operation where
  parseJSON (Object v) = Operation <$>
                         v .: "matchType" <*>
                         v .: "target" <*>
                         v .: "replacement"
  parseJSON _          = mzero


instance ToJSON Operation where
     toJSON (Operation mType toTarget toReplace) =
       object [ "matchType"   .= mType
              , "target"      .= toTarget
              , "replacement" .= toReplace
              ]

-- | Map of bucket names pointing to a tuple of case sensitive and non-case
-- sensitive state machines, respectively
type OperationMachines = Map.Map String ( (StateMachine Char Operation)
                                        , (StateMachine Char Operation) )


data App = App
    { _operations :: MVar OperationMachines
    }

makeLenses ''App

type AppHandler = Handler App App
