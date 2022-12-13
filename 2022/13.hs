import           AdventOfCode.Main
import qualified AdventOfCode.NanoParser as NP
import           Control.Applicative     (many, (<|>))
import           Control.Monad           (guard)
import           Data.List               (findIndices, sortBy)

type Pair = (Packet, Packet)
data Packet = I Int | L [Packet] deriving (Eq, Show)

parsePairs :: NP.Parser Char [Pair]
parsePairs = many $ (,) <$> parsePacket <*> parsePacket

parsePacket :: NP.Parser Char Packet
parsePacket = (L <$> list <|> I <$> NP.decimal) <* NP.spaces
  where
    list = NP.char '[' *> NP.sepBy parsePacket (NP.char ',') <* NP.char ']'

comparePackets :: Packet -> Packet -> Ordering
comparePackets (I x)        (I y)        = compare x y
comparePackets (L (x : xs)) (L (y : ys)) = case comparePackets x y of
    EQ -> comparePackets (L xs) (L ys)
    o  -> o
comparePackets (L [])       (L (_ : _))  = LT
comparePackets (L (_ : _))  (L [])       = GT
comparePackets (L [])       (L [])       = EQ
comparePackets (L x)        (I y)        = comparePackets (L x)   (L [I y])
comparePackets (I x)        (L y)        = comparePackets (L [I x]) (L y)

main :: IO ()
main = pureMain $ \input -> do
    pairs <- NP.runParser parsePairs input
    let part1 = sum $ do
            (idx, (l, r)) <- zip [1 :: Int ..] pairs
            guard $ comparePackets l r == LT
            pure idx
        dividers = [L [L [I 2]], L [L [I 6]]]
        ordered  = sortBy comparePackets $
            dividers ++ [p | (l, r) <- pairs, p <- [l, r]]
        part2    = product . map succ $ findIndices (`elem` dividers) ordered
    pure (pure part1, pure part2)
