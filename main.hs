module Main(main) where

import System.IO
import Data.Word
import qualified Data.ByteString.Lazy as S
import qualified Data.ByteString.Lazy.Char8 as C

main = do
  -- Turn off buffering as we should not have to do unbuffered IO
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin NoBuffering
  putStr "file> "
  file <- getLine
  handle <- openFile file ReadMode
  program <- hGetContents handle
  run program
  hClose handle

run :: String -> IO ()
run program = run' program 0 [0,0..] 0

run' :: String -> Int -> [Word8] -> Int -> IO ()
run' program insPointer mem memPointer
  | currentIns == '>' = run' program nextIP mem (memPointer + 1)
  | currentIns == '<' = run' program nextIP mem (memPointer - 1)
  | currentIns == '+' = run' program nextIP (applyFunctionAtIndex mem ((+) 1) memPointer) memPointer
  | currentIns == '-' = run' program nextIP (applyFunctionAtIndex mem (flip (-) 1) memPointer) memPointer
  | currentIns == '.' = do
    putChar $ (C.unpack . S.pack) mem !! memPointer
    run' program nextIP mem memPointer
  | currentIns == ',' = do
    input <- getChar >>= return . S.unpack . C.singleton
    run' program nextIP (applyFunctionAtIndex mem (\_ -> head input) memPointer) memPointer
  | currentIns == '[' = if (mem !! memPointer) == 0
                           then run' program (findEndOfLoop program insPointer 0) mem memPointer
                        else run' program nextIP mem memPointer
  | currentIns == ']' = if (mem !! memPointer) /= 0
                           then run' program (findBeginningOfLoop program insPointer 0) mem memPointer
                        else run' program nextIP mem memPointer
  | otherwise = run' program (insPointer + 1) mem memPointer
  where currentIns = program !! insPointer
        nextIP     = insPointer + 1

applyFunctionAtIndex :: [a] -> (a -> a) -> Int -> [a]
applyFunctionAtIndex (x:xs) f i
  | i == 0    = (f x):xs
  | otherwise = x:(applyFunctionAtIndex xs f (i - 1))

-- When calling find{End,Beginning}OfLoop always use the constant 0 as the last argument
findEndOfLoop :: String -> Int -> Int -> Int
findEndOfLoop program insPointer skip
  | currentIns == '[' = findEndOfLoop program nextIP (skip + 1)
  | currentIns == ']' = if skip > 1
                           then findEndOfLoop program nextIP (skip - 1)
                        else insPointer
  | otherwise         = findEndOfLoop program nextIP skip
  where currentIns = program !! insPointer
        nextIP     = insPointer + 1

findBeginningOfLoop :: String -> Int -> Int -> Int
findBeginningOfLoop program insPointer skip
  | currentIns == ']' = findBeginningOfLoop program prevIP (skip + 1)
  | currentIns == '[' = if skip > 1
                           then findBeginningOfLoop program prevIP (skip - 1)
                        else insPointer
  | otherwise         = findBeginningOfLoop program prevIP skip
  where currentIns = program !! insPointer
        prevIP     = insPointer - 1
-- Local Variables:
-- compile-command: "ghc --make main.hs -o bin/mlbf"
-- End:
