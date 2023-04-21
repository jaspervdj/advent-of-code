import qualified AdventOfCode.Grid   as G
import           AdventOfCode.Main
import           AdventOfCode.Stream (Stream (..))
import qualified AdventOfCode.Stream as Stream
import           AdventOfCode.V2     (V2 (..), (.+.))
import qualified AdventOfCode.V2     as V2
import           Data.Char           (isSpace)
import           Data.Foldable       (foldl')
import qualified Data.Map            as M
import qualified Data.Set            as S

type Shape = S.Set G.Pos

rocks :: [Shape]
rocks = fmap S.fromList $
    [ [V2 x 0 | x <- [0 .. 3]]
    , [V2 1 0, V2 0 1, V2 1 1, V2 2 1, V2 1 2]
    , [V2 2 2, V2 2 1, V2 0 0, V2 1 0, V2 2 0]
    , [V2 0 y | y <- [0 .. 3]]
    , [V2 0 0, V2 1 0, V2 0 1, V2 1 1]
    ]

shapeHeight :: Shape -> Int
shapeHeight = foldl' (\acc (V2 _ y) -> if y + 1 > acc then y + 1 else acc) 0

move :: V2 Int -> Shape -> Shape
move offset = S.map (.+. offset)

data World = World
    { wRocks   :: Stream Shape
    , wJets    :: Stream Jet
    , wWidth   :: Int
    , wRock    :: Maybe (S.Set G.Pos)
    , wFall    :: Bool
    , wGrid    :: S.Set G.Pos
    , wTop     :: Int
    , wStopped :: Int
    }

instance Show World where
    show w = unlines . reverse . lines . G.toString $
        M.fromSet (const '#') (wGrid w) <>
        maybe M.empty (M.fromSet (const '@')) (wRock w) <>
        M.fromList [(V2 x 0, '-') | x <- [0 .. wWidth w - 1]]

data Jet = L | R deriving (Eq, Show)

parseJets :: String -> [Jet]
parseJets = map toJet
  where
    toJet '<' = L
    toJet '>' = R
    toJet c   = error $ "unknown jet: " ++ show c

step :: World -> World
step w
    | Nothing <- wRock w =
        let shape = Stream.head (wRocks w)
            y     = wTop w + 3
            rock  = move (V2 2 y) shape in
        w
            { wRocks = Stream.tail (wRocks w)
            , wRock  = Just rock
            }

    | Just rock0 <- wRock w, wFall w =
        let rock1 = move (V2 0 (-1)) rock0
            stuck = any ((<= 0) . V2.v2Y) rock1 ||
                not (S.null . S.intersection rock1 $ wGrid w) in
        if not stuck then w
            { wFall = not (wFall w)
            , wRock = Just rock1
            }
        else w
            { wRock    = Nothing
            , wGrid    = wGrid w <> rock0
            , wFall    = not (wFall w)
            , wTop     = maximum $ wTop w : map V2.v2Y (S.toList rock0)
            , wStopped = wStopped w + 1
            }

    | Just rock0 <- wRock w =
        let (jet, jets) = Stream.uncons (wJets w)
            offset = case jet of
                L -> V2 (-1) 0
                R -> V2 1 0
            rock1 = move offset rock0
            stuck =
                any ((>= wWidth w) . V2.v2X) rock1 ||
                any ((< 0) . V2.v2X) rock1 ||
                not (S.null . S.intersection rock1 $ wGrid w) in
        if not stuck then w
            { wJets = jets
            , wRock = Just rock1
            , wFall = not (wFall w)
            }
        else w
            { wJets = jets
            , wFall = not (wFall w)
            }

main :: IO ()
main = simpleMain $ \input ->
    let jets   = parseJets $ filter (not . isSpace) input
        world0 = World (Stream.fromListCycle rocks) (Stream.fromListCycle jets)
            10 Nothing False S.empty 0 0
        world2022 = head . dropWhile ((< 2022) . wStopped) $ iterate step world0
        viz = unlines $ fmap show $ take 20 $ iterate step world0 in
    (viz, wTop world2022)
