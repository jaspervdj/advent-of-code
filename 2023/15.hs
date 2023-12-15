import           AdventOfCode.Main
import qualified AdventOfCode.NanoParser as NP
import           Control.Applicative     ((<|>))
import           Data.Char               (ord)
import qualified Data.IntMap             as IM
import           Data.List               (foldl')

hash :: String -> Int
hash = foldl' (\acc c -> (acc + ord c) * 17 `mod` 256) 0

data Step      = Step String Operation deriving (Show)
data Operation = Del | Ins Int deriving (Show)

parseOperations :: NP.Parser Char [Step]
parseOperations = NP.sepBy1 parseOperation (NP.char ',')
  where
    parseOperation = Step
        <$> NP.many1 NP.alpha
        <*> ((Del <$ NP.char '-') <|> (NP.char '=' *> (Ins <$> NP.decimal)))

type Boxes = IM.IntMap [(String, Int)]

step :: Step -> Boxes -> Boxes
step (Step label Del) = IM.adjust
    (\lenses -> filter ((/= label) . fst) lenses)
    (hash label)
step (Step label (Ins focus)) = IM.alter
    (\mbLenses -> Just $ case mbLenses of
        Nothing -> [(label, focus)]
        Just lenses -> case break ((== label) . fst) lenses of
            (_, [])         -> lenses ++ [(label, focus)]
            (pre, _ : post) -> pre ++ [(label, focus)] ++ post)
    (hash label)

focusPower :: Boxes -> Int
focusPower boxes = sum $ do
    (boxNum, lenses) <- IM.toList boxes
    (slot, (_, focus)) <- zip [0 ..] lenses
    pure $ (boxNum + 1) * (slot + 1) * focus

main :: IO ()
main = pureMain $ \str -> do
    let part1 = map hash . words $ map (\c -> if c == ',' then ' ' else c) str
    steps <- NP.runParser parseOperations str
    let part2 = foldl' (\acc s -> step s acc) IM.empty steps
    pure (pure (sum part1), pure (focusPower part2))
