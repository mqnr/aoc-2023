import Data.Char (digitToInt, isDigit)

main :: IO ()
main = readFile "input.txt" >>= print . sum . map (joinExtremes . digits) . lines
  where digits = map digitToInt . filter isDigit
        joinExtremes xs = read $ concatMap show [head xs, last xs]
