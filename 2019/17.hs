{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}
import qualified AdventOfCode.Grid       as G
import           AdventOfCode.IntCode
import qualified AdventOfCode.NanoParser as NP
import qualified AdventOfCode.V2         as V2
import           Control.Applicative     ((<|>))
import           Control.Monad           (guard)
import           Data.Char               (ord)
import           Data.Char               (chr)
import qualified Data.Map                as Map
import           Data.Maybe              (mapMaybe)
import qualified System.IO               as IO

type Scaffolding = G.Grid ()
type Robot = (G.Pos, G.Dir)

intersections :: Scaffolding -> Scaffolding
intersections g =
    Map.filterWithKey (\p _ -> all (`Map.member` g) (G.neighbours p)) g

alignmentParameters :: Scaffolding -> G.Grid Int
alignmentParameters = Map.mapWithKey (\(V2.V2 x y) () -> x * y)

charToDir :: Char -> Maybe G.Dir
charToDir = \case
    '^' -> Just G.U
    '>' -> Just G.R
    'v' -> Just G.D
    '<' -> Just G.L
    _   -> Nothing

data Turn = L | R deriving (Eq, Ord, Show)

turn :: Turn -> G.Dir -> G.Dir
turn L = G.turnLeft
turn R = G.turnRight

traceScaffolding :: Scaffolding -> Robot -> [(Turn, Int)]
traceScaffolding scaffolding (pos, dir) =
    case turnAndWalk L <|> turnAndWalk R of
        Nothing            -> []
        Just (t, s, robot) -> (t, s) : traceScaffolding scaffolding robot
  where
    turnAndWalk :: Turn -> Maybe (Turn, Int, Robot)
    turnAndWalk tc = do
        let dir'          = turn tc dir
            (steps, pos') = go 0 pos
            go !i p       =
                let p' = G.move dir' p in
                if Map.member p' scaffolding then go (i + 1) p' else (i, p)

        guard $ steps > 0
        pure (tc, steps, (pos', dir'))

-- | Hard manual labor
manual :: [Int]
manual = concatMap (map ord . (++ "\n"))
    [ "A,A,B,C,A,C,B,C,A,B"
    , "L,4,L,10,L,6"
    , "L,6,L,4,R,8,R,8"
    , "L,6,R,8,L,10,L,8,L,8"
    , "n"
    ]

main :: IO ()
main = do
    prog <- NP.hRunParser IO.stdin parseProgram
    let cgrid       = G.fromString . map chr . evalMachine $ initMachine [] prog
        scaffolding = Map.mapMaybe (\c -> guard $ c == '#') cgrid
    robot <- case mapMaybe (traverse charToDir) $ Map.toList cgrid of
        [(xy, d)] -> pure (xy, d)
        _         -> fail "robot not found"

    G.printGrid IO.stderr cgrid
    IO.hPutStrLn IO.stderr . show $ traceScaffolding scaffolding robot
    print . sum . alignmentParameters $ intersections scaffolding
    print . last . evalMachine . initMachine manual $ makeProgram [2] <> prog
