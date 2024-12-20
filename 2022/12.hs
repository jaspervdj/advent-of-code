import qualified AdventOfCode.Bfs          as Bfs
import qualified AdventOfCode.Grid.Bounded as G
import           AdventOfCode.Main
import           Control.Monad             (guard)
import           Data.Char                 (ord)
import           Data.Maybe                (maybeToList)
import qualified Data.Set                  as S

data Hills = Hills
    { hillsGrid  :: G.Grid Int
    , hillsStart :: G.Pos
    , hillsEnd   :: G.Pos
    } deriving (Show)

parseHills :: String -> Either String Hills
parseHills input = do
    grid0 <- G.fromString input
    start <- find 'S' grid0
    end   <- find 'E' grid0
    grid1 <- traverse toHeight grid0
    pure $ Hills grid1 start end
  where
    toHeight c
        | c >= 'a' && c <= 'z' = Right $ ord c - ord 'a'
        | c == 'S'             = Right $ 0
        | c == 'E'             = Right $ 25
        | otherwise            = Left $ "Unknown height: " ++ show c
    find c g = case filter ((== c) . snd) $ G.toList g of
        (pos, _) : _ -> Right pos
        _            -> Left $ "No " ++ show c ++ " found"

shortest
    :: (Int -> Int -> Bool)
    -> (G.Pos -> Bool)
    -> G.Pos
    -> G.Grid Int
    -> Int
shortest canClimb isEnd start grid = maybe 0 snd $ Bfs.goal $
    Bfs.bfs Bfs.defaultOptions
        { Bfs.neighbours = neighbours
        , Bfs.find       = isEnd
        , Bfs.start      = S.singleton start
        }
  where
    neighbours pos = do
        let height = grid G.! pos
        nb <- G.neighbours pos
        x  <- maybeToList $ G.lookup nb grid
        guard $ canClimb height x
        pure nb

main :: IO ()
main = pureMain $ \input -> do
    hills <- parseHills input
    let part1 = shortest
            (\h x -> x <= h + 1)
            (== hillsEnd hills)
            (hillsStart hills)
            (hillsGrid hills)
        part2 = shortest
            (\h x -> x >= h - 1)
            ((== 0) . (hillsGrid hills G.!))
            (hillsEnd hills)
            (hillsGrid hills)
    pure (pure part1, pure part2)
