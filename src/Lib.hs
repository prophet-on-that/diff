module Lib
    ( Hunk (..)
    , Diff
    , diff
    ) where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Control.Monad.State

data Hunk
  = LeftInsert Int Int
  | RightInsert Int Int
  | Replacement Int Int Int Int
  deriving (Show)

type Diff = [Hunk]

-- * Inclusive start index
type StartIndex = Int
-- * Exclusive end index
type EndIndex = Int

diff :: Eq a => Vector a -> Vector a -> Diff
diff left right
  = diff' 0 (V.length left) 0 (V.length right)
  where
    -- End indices are exclusive
    diff' leftStartIndex leftEndIndex rightStartIndex rightEndIndex
      = case lcs slicedLeft slicedRight of
          Just (lcsLength, (+ leftStartIndex) -> lcsLeftStartIndex, (+ rightStartIndex) -> lcsRightStartIndex) ->
            diff' leftStartIndex lcsLeftStartIndex rightStartIndex lcsRightStartIndex ++
            diff' (lcsLeftStartIndex + lcsLength) leftEndIndex (lcsRightStartIndex + lcsLength) rightEndIndex
          Nothing ->
            if
              | isLeftEmpty && isRightEmpty -> []
              | isLeftEmpty -> [RightInsert rightStartIndex rightEndIndex]
              | isRightEmpty -> [LeftInsert leftStartIndex leftEndIndex]
              | otherwise -> [Replacement leftStartIndex leftEndIndex rightStartIndex rightEndIndex]
      where
        isLeftEmpty
          = leftEndIndex <= leftStartIndex
        isRightEmpty
          = rightEndIndex <= rightStartIndex
        slicedLeft
          = V.slice leftStartIndex (leftEndIndex - leftStartIndex) left
        slicedRight
          = V.slice rightStartIndex (rightEndIndex - rightStartIndex) right

data LCSDirection
  = DropLeft
  | DropRight
  | TakeBoth
  deriving (Show, Eq)

type LCSTable = Vector (Maybe (Int, LCSDirection))

lcs
  :: Eq a
  => Vector a
  -> Vector a
  -> Maybe (Int, Int, Int) -- Maybe (Positive length of LCS, left LCS start index, right LCS start index)
lcs left right = do
  = 
  where
    leftLength
      = V.length left
    rightLength
      = V.length right
    initialTable
      = V.replicate (leftLength * rightLength) Nothing
    ((lcsLength, lcsDirection
    
    lcs'
      :: StartIndex
      -> StartIndex
      -> State LCSTable (Int, LCSDirection)
    lcs' leftIndex rightIndex = do
      let
        tableIndex
          = rightIndex * leftLength + leftIndex
      tableVal <- get >>= flip V.indexM tableIndex
      case tableVal of
        Nothing -> do
          lcsResult <- if
            | leftIndex == leftLength || rightIndex == rightLength ->
                return (0, TakeBoth) -- Direction arbitrary here
            | left V.! leftIndex == right V.! rightIndex -> do
                (takeBothLength, _) <- lcs' (leftIndex + 1) (rightIndex + 1)
                return (takeBothLength + 1, TakeBoth)
            | otherwise -> do
                (dropLeftLength, _) <- lcs' (leftIndex + 1) rightIndex
                (dropRightLength, _) <- lcs' leftIndex (rightIndex + 1)
                return $
                  if dropLeftLength > dropRightLength
                    then
                      (dropLeftLength, DropLeft)
                    else
                      (dropRightLength, DropRight)
          modify' (V.// [(tableIndex, (Just lcsResult))])
          return lcsResult
        Just lcsResult ->
          return lcsResult
    
