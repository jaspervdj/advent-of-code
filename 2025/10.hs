{-# LANGUAGE DataKinds #-}
import qualified AdventOfCode.Dijkstra   as Dijkstra
import           AdventOfCode.Main       (ioMain)
import qualified AdventOfCode.NanoParser as NP
import qualified AdventOfCode.Z3         as Z3
import           Control.Applicative     (many, (<|>))
import           Data.Bits               (setBit, xor)
import           Data.List               (foldl')
import qualified Data.Map                as M
import qualified Data.Set                as S
import           Data.Traversable        (for)

data MachineSpec = MachineSpec
    { msIndicators :: [Bool]
    , msButtons    :: [[Int]]
    , msJoltage    :: [Int]
    } deriving (Show)

parseMachineSpecs :: NP.Parser Char [MachineSpec]
parseMachineSpecs = many parseMachineSpec
  where
    section open close p = NP.char open *> p <* NP.char close <* NP.spaces
    commaSep p           = NP.sepBy p (NP.char ',')

    parseIndicators = section '[' ']' $ many $
        (False <$ NP.char '.') <|> (True <$ NP.char '#')
    parseButtons = many $ section '(' ')' $ commaSep NP.decimal
    parseJoltage = section '{' '}' $ commaSep NP.decimal

    parseMachineSpec = MachineSpec
        <$> parseIndicators
        <*> parseButtons
        <*> parseJoltage

turnOn :: MachineSpec -> Maybe Int
turnOn ms = M.lookup goal $ Dijkstra.distances result
  where
    goal = foldl'
        (\acc (i, b) -> if b then setBit acc i else acc)
        0
        (zip [0 .. ] (msIndicators ms)) :: Int

    buttons = map (foldl' setBit 0) (msButtons ms)
    neighbours bits = zip (repeat 1) $ map (xor bits) buttons

    result = Dijkstra.dijkstra Dijkstra.defaultOptions
        { Dijkstra.start = S.singleton 0
        , Dijkstra.neighbours = neighbours
        , Dijkstra.find = Dijkstra.FindOne (== goal)
        }

joltage :: MachineSpec -> Z3.Program
joltage ms =
    foldMap (Z3.declareConst . fst) buttons  <>
    mconcat [Z3.assert (Z3.var b Z3.>= Z3.int 0) | (b, _) <- buttons] <>
    Z3.declareConstEq presses (Z3.add (map (Z3.var . fst) buttons)) <>
    constraints <>
    Z3.minimize (Z3.var presses) <>
    Z3.checkSat <>
    Z3.eval (Z3.var presses)
  where
    presses :: Z3.Var 'Z3.IntSort
    presses = Z3.mkVar "presses"

    buttons :: [(Z3.Var 'Z3.IntSort, [Int])]
    buttons = do
        (i, button) <- zip [0 :: Int ..] $ msButtons ms
        pure (Z3.mkVar ("button_" ++ show i), button)

    buttonsIncreasing :: Int -> [Z3.Var 'Z3.IntSort]
    buttonsIncreasing joltageIdx =
        [bv | (bv, button) <- buttons, joltageIdx `elem` button]

    constraints :: Z3.Program
    constraints = concat $ do
        (joltageIdx, joltageVal) <- zip [0 ..] $ msJoltage ms
        pure $ Z3.assert $ Z3.int joltageVal Z3.==
            Z3.add [Z3.var b | b <- buttonsIncreasing joltageIdx]

main :: IO ()
main = ioMain $ \str -> do
    machines <- either fail pure $ NP.runParser parseMachineSpecs str
    let part1 = maybe (fail "cant turn this on") (pure . sum) $
            traverse turnOn machines
        part2 = fmap sum $ for machines $ \machine ->
            read <$> Z3.run "part2" (joltage machine) :: IO Int
    pure (part1, part2)
