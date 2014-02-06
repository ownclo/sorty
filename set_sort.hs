{-# LANGUAGE BangPatterns #-}

import Data.Int
import Data.Maybe

import qualified Data.List as List
import qualified Data.Set as Set

import qualified Data.ByteString.Lazy.Char8 as LS

data EventType = Start
               | Finish
               deriving (Show,Eq,Ord)

data Event = Event {
    _time    :: {-# UNPACK #-} !Int64,
    _evType  ::                !EventType,
    _taskNum :: {-# UNPACK #-} !Int64
    } deriving (Show,Eq,Ord)

makeStartQueue :: [(Int64, LS.ByteString)] -> Set.Set Event
makeStartQueue = List.foldl' insert Set.empty
  where insert set (num, line) = Set.insert finish $ Set.insert start set
          where start = Event startTime Start num
                finish = Event finishTime Finish num

                [startTime, duration] = map readInt $ LS.words line
                finishTime = startTime + duration
                readInt = fromIntegral . fst . fromJust . LS.readInt  -- not for production

-- XXX: will fusion work here?
main :: IO()
main = do
    inData <- LS.lines `fmap` LS.readFile "input.txt"  
    let startQueue = makeStartQueue $ zip [1..] inData
    print $ Set.findMax startQueue
