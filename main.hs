module Main(main) where

import System.IO
import qualified Data.ByteString.Char8 as C

main = do
  putStr "file> "
  hFlush stdout
  file <- getLine
  handle <- openFile file ReadMode
  program <- hGetContents handle
  run program
  hClose handle

run :: String -> IO ()
run program = run' program 0 C.empty 0

run' :: String -> Int -> C.ByteString -> Int -> IO ()
run' program insPointer mem memPointer
  | currentIns == '>' = error "FIXME: Instruction not implemented: >"
  | currentIns == '<' = error "FIXME: Instruction not implemented: <"
  | currentIns == '+' = error "FIXME: Instruction not implemented: +"
  | currentIns == '-' = error "FIXME: Instruction not implemented: -"
  | currentIns == '.' = error "FIXME: Instruction not implemented: ."
  | currentIns == ',' = error "FIXME: Instruction not implemented: ,"
  | currentIns == '[' = error "FIXME: Instruction not implemented: ["
  | currentIns == ']' = error "FIXME: Instruction not implemented: ]"
  | otherwise = run' program (insPointer + 1) mem memPointer
  where currentIns = program !! insPointer

-- Local Variables:
-- compile-command: "ghc --make main.hs -o bin/mlbf"
-- End:
