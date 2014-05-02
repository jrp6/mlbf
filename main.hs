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
  | currentIns == ',' = error "FIXME: Instruction not implemented: ,"
  | currentIns == '[' = error "FIXME: Instruction not implemented: ["
  | currentIns == ']' = error "FIXME: Instruction not implemented: ]"
  | otherwise = run' program (insPointer + 1) mem memPointer
  where currentIns = program !! insPointer
        currentData = mem !! memPointer
        nextIP = insPointer + 1

applyFunctionAtIndex :: [a] -> (a -> a) -> Int -> [a]
applyFunctionAtIndex (x:xs) f i
  | i == 0    = (f x):xs
  | otherwise = x:(applyFunctionAtIndex xs f (i - 1))

-- Local Variables:
-- compile-command: "ghc --make main.hs -o bin/mlbf"
-- End:
