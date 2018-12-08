import           Data.List  (group, sort)
import           Data.Maybe (fromMaybe, listToMaybe, mapMaybe)

diff1 :: Eq a => [a] -> [a] -> Maybe [a]
diff1 (x : xs) (y : ys)
    | x == y    = (x :) <$> diff1 xs ys
    | xs == ys  = Just xs
    | otherwise = Nothing
diff1 _ _ = Nothing

pairs :: [a] -> [(a, a)]
pairs []       = []
pairs (x : xs) = [(x, y) | y <- xs] ++ pairs xs

main :: IO ()
main = do
    boxIds <- lines <$> getContents
    let charCounts = map (map length . group . sort) boxIds
        appears n  = length $ filter (n `elem`) charCounts
    print $ appears 2 * appears 3

    putStrLn $ fromMaybe "<no match>" $ listToMaybe $
        mapMaybe (uncurry diff1) (pairs boxIds)
