{-# LANGUAGE BangPatterns #-}

import           Data.Char   (digitToInt)
import qualified Data.Vector as V

captcha :: (Int -> Int) -> V.Vector Int -> Int
captcha f vec = V.sum $ V.ifilter
    (\idx0 x ->
        let !idx1 = f idx0 `mod` V.length vec in
        x == vec V.! idx1)
    vec

main :: IO ()
main = do
    input <- V.map digitToInt . V.fromList <$> getLine :: IO (V.Vector Int)
    putStrLn $ "Simple captcha: " ++ show (captcha succ input)

    let halfway = (+ (V.length input `div` 2))
    putStrLn $ "Halfway round captcha: " ++ show (captcha halfway input)
