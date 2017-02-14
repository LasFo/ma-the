import System.Environment

main = do 
  args <- getArgs
  res <- parser $ head args
  let threads = head . tail $ args
      fileline = threads ++ " " ++ show res ++ "\n"
  appendFile (last args) fileline
  print res

parser fName = do
  file <- readFile fName
  let parseRes = parse file
      average = aver parseRes
  return average

aver :: [Double] -> Double
aver xs = (sum xs)/(fromIntegral (length xs))

parse :: String -> [Double]
parse str = map read $ map (cutter ':') $ lines str
  where cutter c (x:xs)
           | c == x    = xs
           | otherwise = cutter c xs 
