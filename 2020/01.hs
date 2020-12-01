import           Control.Monad      (guard)
import           Data.List.Extended (select)
import           Data.Maybe         (listToMaybe)

main :: IO ()
main = do
    numbers <- map read . lines <$> getContents :: IO [Int]
    maybe (fail "no solution") print . listToMaybe $ do
        (x, numbers') <- select numbers
        (y, _) <- select numbers'
        guard $ x + y == 2020
        pure $ x * y
    maybe (fail "no solution") print . listToMaybe $ do
        (x, numbers') <- select numbers
        (y, numbers'') <- select numbers'
        guard $ x + y <= 2020
        (z, _) <- select numbers''
        guard $ x + y + z == 2020
        pure $ x * y * z
