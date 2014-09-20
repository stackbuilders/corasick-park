{-# LANGUAGE OverloadedStrings #-}

module Text.CorasickPark.Parser (replace) where

import Text.Parsec.Prim hiding ((<|>))
import Text.Parsec.Char
import Text.Parsec.Text ()
import Text.Parsec.Combinator

import qualified Data.Char as C
import qualified Data.Text as T

import Control.Applicative ((<|>))

import Text.CorasickPark.Types (BoundaryType(..))

-- | Text before and after the match is found.
type TextEnds = (T.Text, T.Text)

data Side = LeftSide | RightSide


-- | Returns a function that retrieves the nearest charactero to a match,
-- for examining the boundary of the match.
nearestCharFn :: Side -> (T.Text -> Char)
nearestCharFn RightSide = T.head
nearestCharFn LeftSide  = T.last


spliceText :: T.Text -- ^ Pattern matched
           -> T.Text -- ^ Text to substitute
           -> (BoundaryType, BoundaryType) -- ^ Boundaries we need to verify
           -> TextEnds -- ^ Text chunks broken by matches of pattern
           -> T.Text

spliceText pattern replacement (lboundary, rboundary) (lside, rside) =
  if (validBoundary lboundary LeftSide lside) &&
     (validBoundary rboundary RightSide rside) then

    lside `T.append` replacement
  else
    lside `T.append` pattern


replace :: T.Text -- ^ Input text string

        -> T.Text
        -- ^ Text of pattern from match that was found

        -> T.Text
        -- ^ Replacement

        -> (BoundaryType, BoundaryType)
        -- ^ (Left Boundary, Right Boundary) for matched pattern

        -> Bool
        -- ^ Whether the matched pattern is case-sensitive

        -> Bool
        -- ^ Whether to replace all occurrences

        -> T.Text
        -- ^ The Text with substitutions applied

replace input target replacement boundaries isCaseSensitive isGlobal =
  case parsed of
    Left _        -> input
    Right matches -> insertReplacements target replacement boundaries matches

  where parsed =
          parse (parser isCaseSensitive isGlobal target) "(input)" input


parser :: Bool -> Bool -> T.Text -> Parsec T.Text () [T.Text]
parser isCaseSensitive isGlobal target = do
  sections <- if isGlobal then
                many (try (matchingSection isCaseSensitive target))

              else
                do
                  ss <- try (matchingSection isCaseSensitive target)
                  return [ss]

  toEnd <- manyTill anyChar eof

  return $ map T.pack $ sections ++ [toEnd]

insertReplacements :: T.Text -- ^ Pattern matched
                  -> T.Text -- ^ Text to substitute
                  -> (BoundaryType, BoundaryType) -- ^ Boundaries to verify
                  -> [T.Text] -- ^ Text chunks broken by matches of pattern
                  -> T.Text
insertReplacements _ _ _ [] = T.pack ""
insertReplacements _ _ _ (x:[]) = x
insertReplacements pattern replacement (lboundary, rboundary) (x:y:zs) =
  splicedText `T.append` insertReplacements pattern replacement
                                            (lboundary, rboundary) (y : zs)

  where splicedText =
          spliceText pattern replacement (lboundary, rboundary) (x, y)


-- | Figures out if the given string is a valid boundary for each match type.
-- Note that we take care of null cases by looking up to see if it matches an
-- input boundary, which is basically just a null check on the matched text.
validBoundary :: BoundaryType -> Side -> T.Text -> Bool

validBoundary NoBoundary _ _ = True

validBoundary InputBoundary _ t = T.null t

validBoundary LineBoundary s t =
  validBoundary InputBoundary s t || (nearestC == '\n') || (nearestC == '\r')
  where nearestC = (nearestCharFn s) t

validBoundary WordBoundary s t =
  validBoundary LineBoundary s t
  || ((not . C.isAlpha) $ (nearestCharFn s) t)


matchingSection :: Bool -> T.Text -> Parsec T.Text () String
matchingSection isCaseSensitive target =
  manyTill anyChar (try (caseString isCaseSensitive target))


caseString :: Bool -> T.Text -> Parsec T.Text () String
caseString True target = string $ T.unpack target
caseString False target =
  do { walk $ T.unpack target; return $ T.unpack target }

  where
    walk []     = return ()
    walk (c:cs) = do { _ <- caseChar c <?> msg; walk cs }

    caseChar c  | C.isAlpha c  = char (C.toLower c) <|> char (C.toUpper c)
                | otherwise  = char c

    msg         = show target
