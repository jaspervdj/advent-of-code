import qualified AdventOfCode.Grid       as G
import           AdventOfCode.Main
import qualified AdventOfCode.NanoParser as NP
import           AdventOfCode.V2         (V2 (..))
import           Control.Applicative     ((<|>))
import           Control.Monad           (when)
import           Control.Monad.Except    (throwError)
import           Data.Foldable           (toList)
import           Numeric                 (readHex)

data Dig = Dig G.Dir Int String deriving (Show)

parseDigs :: NP.Parser Char [Dig]
parseDigs = fmap toList $ NP.many1 $
    Dig <$> tok dir <*> tok NP.decimal <*> tok color
  where
    tok    p = (p <* NP.spaces)
    parens p = NP.char '(' *> p <* NP.char ')'

    color = fmap toList $ parens $
        NP.char '#' *> NP.many1 (NP.alpha <|> NP.digit)
    dir =
        (G.U <$ NP.char 'U') <|>
        (G.R <$ NP.char 'R') <|>
        (G.D <$ NP.char 'D') <|>
        (G.L <$ NP.char 'L')

area :: [Dig] -> Int
area digs =
    -- +++-
    -- +||#
    -- +||#
    -- ####
    --
    -- +, - and # form the trench.
    -- + and | indicate shoelace area.
    --
    -- We want the shoelace area and an additional "border" to the bottom and
    -- right.  This is half of the trench length plus one (the - at the top).
    shoelace + trench `div` 2 + 1
  where
    trench = sum [n | Dig _ n _ <- digs]
    points = V2 0 0 : go (V2 0 0) digs

    shoelace = abs . (`div` 2) . sum $ zipWith
        (\(V2 x1 y1) (V2 x2 y2) -> (x1 * y2) - (x2 * y1))
        points
        (drop 1 points ++ points)

    go _ []              = []
    go p (Dig d n _ : t) = let p' = G.move n d p in p' : go p' t

decodeHexDig :: Dig -> Either String Dig
decodeHexDig (Dig _ _ color) = do
    when (length color /= 6) $ throwError "bad color length"
    len <- case readHex (take 5 color) of
        [(n, "")] -> pure n
        _         -> throwError "could not read hex"
    dir <- case last color of
        '0' -> pure G.R
        '1' -> pure G.D
        '2' -> pure G.L
        '3' -> pure G.U
        _   -> throwError "could not read color"
    pure $ Dig dir len color

main :: IO ()
main = pureMain $ \str -> do
    digs <- NP.runParser parseDigs str
    pure (pure (area digs), area <$> traverse decodeHexDig digs)
