import qualified Data.List as L
import qualified Data.Set  as S
import qualified System.IO as IO

run :: [Int] -> [Int]
run = snd . L.mapAccumL (\acc c -> (acc + c, acc)) 0 . cycle

twice :: Ord a => [a] -> Maybe a
twice = go S.empty
  where
    go _ []       = Nothing
    go v (x : xs) = if x `S.member` v then Just x else go (S.insert x v) xs

parse :: IO.Handle -> IO [Int]
parse h = map read . lines . filter (/= '+') <$> IO.hGetContents h

main :: IO ()
main = do
    frequencies <- parse IO.stdin
    print $ sum frequencies
    putStrLn $ maybe "<no number was visited twice>" show $
        twice $ run frequencies
