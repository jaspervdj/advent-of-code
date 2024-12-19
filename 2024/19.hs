{-# LANGUAGE ScopedTypeVariables #-}
import           AdventOfCode.Main       (pureMain)
import qualified AdventOfCode.NanoParser as NP
import           Control.Applicative     (many)
import           Control.Monad           (guard)
import           Data.Foldable           (toList)
import           Data.Monoid             (Any (..), Sum (..))
import qualified Data.Vector             as V

type Pattern a = V.Vector a
type Design a = V.Vector a
data Input = Input [Pattern Char] [Design Char] deriving (Show)

parseInput :: NP.Parser Char Input
parseInput = Input
    <$> NP.sepBy vec (NP.string ", ") <* NP.spaces
    <*> many (vec <* NP.spaces)
  where
    vec = V.fromList . toList <$> NP.many1 NP.alpha

-- | Abstracted over the Monoid we use to count solutions.  This is faster than
-- checking if `count > 0` for the first part, since it allows for
-- short-circuiting over booleans.
make :: forall a m. (Eq a, Monoid m) => m -> [Pattern a] -> Design a -> m
make unit patterns design = v V.! (V.length design - 1)
  where
    v :: V.Vector m
    v = V.generate (V.length design) $ \i -> mconcat $ do
        pat <- patterns
        let start = i + 1 - V.length pat
        guard $ start >= 0
        guard $ V.slice start (V.length pat) design == pat
        pure $ if start == 0 then unit else v V.! (start - 1)

main :: IO ()
main = pureMain $ \str -> do
    Input patterns designs <- NP.runParser parseInput str
    let part1 = length $ filter (getAny . make (Any True) patterns) designs
        part2 = getSum $ foldMap (make (Sum 1 :: Sum Int) patterns) designs
    pure (pure part1, pure part2)
