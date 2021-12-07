import Text.Read

inputFilePath :: String
inputFilePath = "puzzle-input/01.txt"

main :: IO ()
main = do
  putStr "Part 1: "
  part1
  putStr "Part 2: "
  part2

part1 :: IO ()
part1 = solve numberOfDecreases

part2 :: IO ()
part2 = solve (numberOfDecreases . threeMeasurementWindows)

readIntMaybe :: String -> Maybe Int
readIntMaybe = readMaybe

parseInput :: String -> Maybe [Int]
parseInput s = mapM readIntMaybe (words s)

solve :: ([Int] -> Int) -> IO ()
solve solver = do
    fileContent <- readFile inputFilePath
    print (solver <$> parseInput fileContent)

numberOfDecreases :: [Int] -> Int
numberOfDecreases xs = length (filter (< 0) (diffsToNext xs))

diffsToNext :: [Int] -> [Int]
diffsToNext []  = []
diffsToNext xs  = map (\(x, y) -> x - y) (zip (init xs) (tail xs))

threeMeasurementWindows :: [Int] -> [Int]
threeMeasurementWindows xs = map tripleSum (zip3 a b c)
    where
        a = dropR 2 xs
        b = drop 1 (dropR 1 xs)
        c = drop 2 xs

-- |Drop, but it removes the elements from the back instead
dropR :: Int -> [a] -> [a]
dropR n = reverse . drop n . reverse

tripleSum :: (Num a) => (a, a, a) -> a
tripleSum (a, b, c) = a + b + c
