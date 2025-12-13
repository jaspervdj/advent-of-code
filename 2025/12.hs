import           AdventOfCode.Main       (pureMain)
import qualified AdventOfCode.NanoParser as NP
import           Control.Applicative     (many, (<|>))
import           Data.Foldable           (toList)
import           Data.Maybe              (maybeToList)

type ShapeID = Int

data Shape = Shape [[Bool]] deriving (Show)

data Region = Region (Int, Int) [(ShapeID, Int)] deriving (Show)

data Problem = Problem
    { pShapes  :: [(ShapeID, Shape)]
    , pRegions :: [Region]
    } deriving (Show)

parseProblem :: NP.Parser Char Problem
parseProblem = Problem
    <$> many ((,) <$> NP.decimal <* tok (NP.char ':') <*> tok parseShape)
    <*> many (parseRegion <* NP.spaces)
  where
    tok p = p <* NP.spaces

    parseShape :: NP.Parser Char Shape
    parseShape = Shape <$> NP.sepBy (toList <$> NP.many1 parseTile) NP.spaces
    parseTile = (True <$ NP.char '#') <|> (False <$ NP.char '.')

    parseRegion = Region <$> parseSize <* tok (NP.char ':') <*> parseQuantities
    parseSize = (,) <$> NP.decimal <* NP.char 'x' <*> NP.decimal
    parseQuantities = zip [0 ..] <$> many (NP.decimal <* NP.horizontalSpaces)

solve :: [(ShapeID, Shape)] -> Region -> Bool
solve shapes (Region (width, height) quantities) = do
    width * height >= sum
        [ q * shapeSize shape
        | (sid, q) <- quantities
        , shape <- maybeToList (lookup sid shapes)
        ]
  where
    shapeSize (Shape rows) = length $ filter id $ concat rows

main :: IO ()
main = pureMain $ \str -> do
    problem <- NP.runParser parseProblem str
    let part1 = length $ filter (solve (pShapes problem)) (pRegions problem)
    pure (pure part1, pure "fin")
