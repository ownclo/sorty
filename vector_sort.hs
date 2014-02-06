{-# LANGUAGE BangPatterns #-}

import Data.Int
import Control.Monad.ST

import qualified Data.Vector as IVector
import qualified Data.Vector.Algorithms.Intro as Intro -- introsort

import qualified Data.ByteString.Lazy.Char8 as LS

data EventType = Start
               | Finish
               deriving (Show,Eq,Ord)

data Event = Event {
    _time    :: {-# UNPACK #-} !Int64,
    _evType  ::                !EventType,
    _taskNum :: {-# UNPACK #-} !Int64
    } deriving (Show,Eq,Ord)

-- Vector of *pointers* to events. Cannot be unboxed
-- because EventType itself can't.
type EventVector = IVector.Vector Event

makeStartQueue :: (EventType, Int64, Int64, LS.ByteString) -> EventVector
makeStartQueue = IVector.unfoldr step
  where step (Start, _, !num, !str) =
          case LS.readInt str of
              Nothing -> Nothing
              Just (!s, !rest) -> Just (start, (Finish, s', num, LS.tail rest))
                where start = Event s' Start num
                      s' = fromIntegral s

        step (Finish, !startTime, !num, !str) =
          case LS.readInt str of
              Nothing -> Nothing
              Just (!d, !rest) -> Just (finish, (Start, 0, num+1, LS.tail rest))
                where finish = Event stopTime Finish num
                      stopTime = startTime + duration
                      duration = fromIntegral d


-- |in-place sorting of an immutable vector. Beware!
-- NB: for a safer approach, use Intro.sort directly.
unsafeSort :: Ord a => IVector.Vector a -> IVector.Vector a
unsafeSort vec = runST $ do
    mutable <- IVector.unsafeThaw vec
    Intro.sort mutable
    IVector.unsafeFreeze mutable

main :: IO ()
main = do
    inData <- LS.readFile "input.txt"
    let sorted = unsafeSort $ makeStartQueue (Start, 0, 1, inData)
    print $ IVector.last sorted
