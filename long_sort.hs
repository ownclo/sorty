{-# LANGUAGE BangPatterns #-}

import Data.List
import Data.Int
import Data.Maybe

import qualified Data.ByteString.Lazy.Char8 as BS

-- ------------------------------------------------
data TEventType = Start | Finish  -- XXX: Start < Finish
                deriving (Show,Eq,Ord)

data TEvent = TEvent {
    _time    :: {-# UNPACK #-} !Int64,
    _evType  ::                !TEventType,
    _taskNum :: {-# UNPACK #-} !Int64
    } deriving (Show,Eq,Ord)

-- ------------------------------------------------
makeStartQueue :: Int -> [BS.ByteString] -> [TEvent] -> [TEvent]
makeStartQueue _ [] q = q
makeStartQueue !num (str:ts) q = makeStartQueue (num+1) ts (e1:e2:q)
  where
    [!s,!d] = map readInt $ BS.words str
    !e1 = TEvent s Start num'
    !e2 = TEvent (s+d) Finish num'
    num' = fromIntegral num
    readInt = fromIntegral . fst . fromJust . BS.readInt  -- not for production

main :: IO()
main = do
    -- Входной файл - много строк по два целых числа в каждой: начало задания и длительность
    -- Вот тут задумался: а кто же тормозит - сортировка или преобразование входных данных
    inData <- BS.lines `fmap` BS.readFile "input.txt"  
    let startQueue = sort $ makeStartQueue 1 inData []
    print $ last startQueue
