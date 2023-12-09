import Data.Char (digitToInt, isDigit)

stringToNumber :: String -> Int
stringToNumber s = case s of
  "one"   -> 1
  "two"   -> 2
  "three" -> 3
  "four"  -> 4
  "five"  -> 5
  "six"   -> 6
  "seven" -> 7
  "eight" -> 8
  "nine"  -> 9
  "1" -> 1
  "2" -> 2
  "3" -> 3
  "4" -> 4
  "5" -> 5
  "6" -> 6
  "7" -> 7
  "8" -> 8
  "9" -> 9
  _ -> -1

windows :: [a] -> [[a]]
windows xs = [take n (drop m xs) | m <- [0..length xs - 1], n <- [1..length xs - m]]

main :: IO ()
main = readFile "input.txt" >>= print . sum . map (joinExtremes . digits) . lines
  where digits = filter (-1 /=) . map stringToNumber . windows
        joinExtremes xs = read $ concatMap show [head xs, last xs]
