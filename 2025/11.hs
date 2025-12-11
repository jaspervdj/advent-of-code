{-# LANGUAGE DataKinds #-}
import           AdventOfCode.Main       (pureMain)
import qualified AdventOfCode.NanoParser as NP
import           Control.Applicative     (many)
import           Data.Foldable           (toList)
import qualified Data.Map                as M
import qualified Data.Set                as S

type Devices = M.Map String [String]

parseDevices :: NP.Parser Char Devices
parseDevices = M.fromList <$> many (parseDevice <* NP.spaces)
  where
    parseDevice = (,)
        <$> parseIdentifier <* NP.char ':' <* NP.horizontalSpaces
        <*> many (parseIdentifier <* NP.horizontalSpaces)

    parseIdentifier = toList <$> NP.many1 NP.alpha

countPaths :: Devices -> String -> String -> Int
countPaths devices start end = paths M.! end
  where
    nodes  = S.fromList [n | (d, ds) <- M.toList devices, n <- d : ds]
    inputs = M.fromListWith (<>) $ do
        (i, outs) <- M.toList devices
        out       <- outs
        pure (out, S.singleton i)

    paths = M.fromList $ do
        n <- toList nodes
        let ins = maybe [] toList $ M.lookup n inputs
        pure (n, if n == start then 1 else sum (map (paths M.!) ins))

main :: IO ()
main = pureMain $ \str -> do
    devices <- NP.runParser parseDevices str

    let part1 = countPaths devices "you" "out"
        part2 =
            countPaths (M.delete "fft" devices) "svr" "dac" *
            countPaths devices                  "dac" "fft" *
            countPaths (M.delete "dac" devices) "fft" "out" +

            countPaths (M.delete "dac" devices) "svr" "fft" *
            countPaths devices                  "fft" "dac" *
            countPaths (M.delete "fft" devices) "dac" "out"

    pure (pure part1, pure part2)
