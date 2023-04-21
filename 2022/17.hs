import qualified AdventOfCode.Grid   as G
import qualified AdventOfCode.Loop   as Loop
import           AdventOfCode.Main
import           AdventOfCode.V2     (V2 (..), (.+.), (.-.))
import qualified AdventOfCode.V2     as V2
import           Control.Monad       (guard)
import           Data.Char           (isSpace)
import           Data.Foldable       (foldl')
import qualified Data.Map            as M
import qualified Data.Set            as S
import qualified Data.Vector         as V

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
    { wRocks    :: V.Vector Shape
    , wRocksIdx :: Int
    , wJets     :: V.Vector Jet
    , wJetsIdx  :: Int
    , wWidth    :: Int
    , wRock     :: Maybe (S.Set G.Pos)
    , wFall     :: Bool
    , wGrid     :: S.Set G.Pos
    , wTop      :: Int
    , wBottom   :: Int
    , wStopped  :: Int
    }

instance Show World where
    show w = unlines . reverse . lines . G.toString $
        M.fromSet (const '#') (wGrid w) <>
        maybe M.empty (M.fromSet (const '@')) (wRock w) <>
        M.fromList [(V2 x 0, '-') | x <- [0 .. wWidth w - 1]]

emptyWorld :: World
emptyWorld = World
    { wRocks    = V.empty
    , wRocksIdx = 0
    , wJets     = V.empty
    , wJetsIdx  = 0
    , wWidth    = 7
    , wRock     = Nothing
    , wFall     = False
    , wGrid     = S.empty
    , wTop      = 0
    , wBottom   = 0
    , wStopped  = 0
    }

trim :: World -> World
trim w = w
    { wGrid   = S.map (.-. V2 0 offset) relevant
    , wRock   = S.map (.-. V2 0 offset) <$> wRock w
    , wTop    = wTop w - offset
    , wBottom = wBottom w + offset
    }
  where
    offset = pred . minimum . fmap v2Y $ S.toList relevant

    relevant = S.intersection (wGrid w) . go S.empty $ S.fromList
        [(V2 x (wTop w + 1), False) | x <- [0 .. wWidth w - 1]]

    go visited queue0 = case S.minView queue0 of
        Nothing -> visited
        Just ((q, True), queue1) -> go (S.insert q visited) queue1
        Just ((q, False), queue1) ->
            let new = do
                    p@(V2 x y) <- G.neighbours q
                    guard $ x >= 0 && x < wWidth w
                    guard $ y >= 0 && y <= wTop w
                    guard . not $ p `S.member` visited
                    pure (p, p `S.member` wGrid w) in
            go (S.insert q visited) (queue1 <> S.fromList new)

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
        let shape = wRocks w V.! wRocksIdx w
            y     = wTop w + 4
            rock  = move (V2 2 y) shape in
        w
            { wRocksIdx = succ (wRocksIdx w) `mod` V.length (wRocks w)
            , wRock     = Just rock
            }

    | Just rock0 <- wRock w, wFall w =
        let rock1 = move (V2 0 (-1)) rock0
            stuck = any ((<= 0) . V2.v2Y) rock1 ||
                not (S.null . S.intersection rock1 $ wGrid w) in
        if not stuck then w
            { wFall = not (wFall w)
            , wRock = Just rock1
            }
        else trim $ w
            { wRock    = Nothing
            , wGrid    = wGrid w <> rock0
            , wFall    = not (wFall w)
            , wTop     = maximum $ wTop w : map V2.v2Y (S.toList rock0)
            , wStopped = wStopped w + 1
            }

    | Just rock0 <- wRock w =
        let jet    = wJets w V.! wJetsIdx w
            offset = case jet of
                L -> V2 (-1) 0
                R -> V2 1 0
            rock1 = move offset rock0
            stuck =
                any ((>= wWidth w) . V2.v2X) rock1 ||
                any ((< 0) . V2.v2X) rock1 ||
                not (S.null . S.intersection rock1 $ wGrid w) in
        if not stuck then w
            { wJetsIdx = succ (wJetsIdx w) `mod` V.length (wJets w)
            , wRock    = Just rock1
            , wFall    = not (wFall w)
            }
        else w
            { wJetsIdx = succ (wJetsIdx w) `mod` V.length (wJets w)
            , wFall    = not (wFall w)
            }

wHeight :: World -> Int
wHeight w = wTop w + wBottom w

main :: IO ()
main = simpleMain $ \input ->
    let jets   = parseJets $ filter (not . isSpace) input
        world0 = emptyWorld
            { wRocks = V.fromList rocks
            , wJets  = V.fromList jets
            }
        part1 = head . dropWhile ((< 2022) . wStopped) $ iterate step world0

        -- Find loop in the input
        key w  = (wRocksIdx w, wJetsIdx w, wGrid w)
        stepToNextDrop w0 =
            let w1 = step w0 in
            if wRock w1 == Nothing then w1 else stepToNextDrop w1
        Just loop = Loop.findLoop key stepToNextDrop world0

        -- Determine bottom and stopped blocks in the loop.
        stoppedAtFirst = wStopped $ Loop.lFirst loop
        bottomAtFirst = wBottom $ Loop.lFirst loop
        stoppedAtSecond = wStopped $ Loop.lSecond loop
        bottomAtSecond = wBottom $ Loop.lSecond loop
        stoppedPerCycle = stoppedAtSecond - stoppedAtFirst
        bottomDeltaPerCycle = bottomAtSecond - bottomAtFirst

        -- Skip using cycles.
        stoppedTarget  = 1000000000000
        numCycles      = (stoppedTarget - stoppedAtFirst) `div` stoppedPerCycle
        fast           = (Loop.lFirst loop)
            { wStopped = stoppedAtFirst + numCycles * stoppedPerCycle
            , wBottom  = bottomAtFirst  + numCycles * bottomDeltaPerCycle
            }

        -- Now perform the slow steps as usual, with a higher wBottom.
        part2 = head . dropWhile ((< stoppedTarget) . wStopped) $
            iterate step fast in
    (wHeight part1, wHeight part2)
