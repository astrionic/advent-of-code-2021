import Text.Read

inputFilePath :: String
inputFilePath = "puzzle-input/01.txt"

main :: IO ()
main = do
  input <- getInput
  let part1 = numberOfDecreases <$> input
  putStrLn $ "Part 1: " ++ show part1
  let part2 = numberOfDecreases . threeMeasurementWindows <$> input
  putStrLn $ "Part 2: " ++ show part2

getInput :: IO (Maybe [Int])
getInput =  parseInput <$> readFile inputFilePath

parseInput :: String -> Maybe [Int]
parseInput s = mapM readMaybe (words s)

numberOfDecreases :: [Int] -> Int
numberOfDecreases xs = length $ filter (< 0) (diffsToNext xs)

diffsToNext :: [Int] -> [Int]
diffsToNext (x0 : x1 : xs) = x0 - x1 : diffsToNext (x1 : xs)
diffsToNext _ = []

threeMeasurementWindows :: [Int] -> [Int]
threeMeasurementWindows (x0 : x1 : x2 : xs) = x0 + x1 + x2 : threeMeasurementWindows (x1 : x2 : xs)
threeMeasurementWindows _ = []
