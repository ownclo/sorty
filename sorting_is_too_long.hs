import System.IO
import Data.List
import Data.Int
  
-- ------------------------------------------------
data TEventType = Finish | Start 
  deriving (Show,Eq,Ord)
  
data TEvent = Event { time :: Int64, evType :: TEventType, taskNum :: Int64 }
  deriving (Show,Eq,Ord)

-- ------------------------------------------------
makeStartQueue :: [(Int64,String)] -> [TEvent] -> [TEvent]
makeStartQueue [] q = q
makeStartQueue ((num,str):ts) q = makeStartQueue ts (e1:e2:q)
  where
    s:d:_ = map read $ words str
    e1 = Event s Start num
    e2 = Event (s+d) Finish num

main :: IO()
main = do
-- Входной файл - много строк по два целых числа в каждой: начало задания и длительность
  inData <- readFile "input.txt"  
  let
    inList = zipWith (,) [1..] $ lines inData
-- Вот тут задумался: а кто же тормозит - сортировка или преобразование входных данных
    startQueue = sort $ makeStartQueue inList []
  print $ last startQueue
