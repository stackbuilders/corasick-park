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
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Cache.LRU as L

data BoundaryType = NoBoundary
                  | WordBoundary
                  | LineBoundary
                  | InputBoundary
                  deriving (Show, Eq)

data Operation = Operation { target    :: !Target
                           , transform :: !Transform
                           } deriving (Show, Eq)

data Target = Target { text          :: !String
                     , caseSensitive :: !Bool
                     , leftBoundary  :: !BoundaryType
                     , rightBoundary :: !BoundaryType
                     , global        :: !Bool
                     } deriving (Show, Eq)

data Transform = Replace String
               | Upcase
               | Downcase
               | Titleize
               | TruncateTrailing
               deriving (Show, Eq)

data OperationSet = OperationSet { setName       :: !String
                                 , allOperations :: ![Operation]
                                 } deriving (Show, Eq)


instance FromJSON OperationSet where
  parseJSON (Object v) = OperationSet <$>
                         v .: "name" <*>
                         v .: "operations"
  parseJSON _          = mzero

instance FromJSON Operation where
  parseJSON (Object v) = Operation <$>
                         v .: "target" <*>
                         v .: "transform"
  parseJSON _          = mzero

instance FromJSON Target where
  parseJSON (Object v) = Target <$>
                         v .: "text" <*>
                         v .: "isCaseSensitive" <*>
                         v .: "leftBoundaryType" <*>
                         v .: "rightBoundaryType" <*>
                         v .: "isGlobal"
  parseJSON _          = mzero

instance FromJSON Transform where
  parseJSON (Object v) = do
    case HM.lookup (T.pack "type") v of
      Nothing -> mzero
      Just s -> do
        case s of
          String "replace" ->
            case HM.lookup (T.pack "replacement") v of
              Just (String repl) -> return $ Replace (T.unpack repl)
              _ -> mzero

          String "upcase"            -> return Upcase
          String "downcase"          -> return Downcase
          String "titleize"          -> return Titleize
          String "truncate trailing" -> return TruncateTrailing
          _                          -> mzero

  parseJSON _ = mzero

instance FromJSON BoundaryType where
  parseJSON (String "none")  = return NoBoundary
  parseJSON (String "word")  = return WordBoundary
  parseJSON (String "line")  = return LineBoundary
  parseJSON (String "input") = return InputBoundary
  parseJSON _                = mzero



-- | Map of bucket names pointing to a tuple of case sensitive and non-case
-- sensitive state machines, respectively
type OperationMachines = L.LRU String ( StateMachine Char Operation
                                      , StateMachine Char Operation )


data App = App
    { _operations :: MVar OperationMachines
    }

makeLenses ''App

type AppHandler = Handler App App
