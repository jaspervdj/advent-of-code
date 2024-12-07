{-# LANGUAGE ScopedTypeVariables #-}
import           AdventOfCode.Main
import qualified AdventOfCode.NanoParser as NP
import           Control.Applicative     ((<|>))
import           Data.Foldable           (toList)
import           Data.List               (foldl', transpose)
import qualified Data.Map                as M
import           Data.Maybe              (catMaybes)

type Stacks stack item = M.Map stack [item]

apply
    :: forall stack item. Ord stack
    => [(Int, stack, stack)] -> Stacks stack item -> Stacks stack item
apply moves0 stack0 =
    foldl' (\acc (n, from, to) -> move n from to acc) stack0 moves0
  where
    move :: Int -> stack -> stack -> Stacks stack item -> Stacks stack item
    move n from to stacks = case M.lookup from stacks of
        Just stack ->
            let (taken, remain) = splitAt n stack in
            M.insertWith (++) to taken $ M.insert from remain stacks
        _ -> stacks

data Input stack item = Input
    { inputStacks :: Stacks stack item
    , inputMoves  :: [(Int, stack, stack)]
    } deriving (Show)

parseInput :: NP.Parser Char (Input Int Char)
parseInput = makeInput
    <$> parseRows <*> parseLabels <* NP.newline
    <*> parseMoves
  where
    makeInput rows labels moves =
        let columns = map catMaybes $ transpose rows in
        Input (M.fromList $ zip labels columns) moves

    space = NP.char ' '

    parseRows = fmap toList $ NP.many1 $
        parseRow <* NP.horizontalSpaces <* NP.newline
    parseRow  = NP.sepBy1 parseItem space
    parseItem =
        (Nothing <$ space <* space <* space) <|>
        (Just <$> (NP.char '[' *> NP.anyChar <* NP.char ']'))

    parseLabels =
        NP.sepBy1 (space *> NP.decimal <* space) space <* NP.newline

    parseMoves = NP.sepBy1 parseMove NP.newline
    parseMove  = (,,)
        <$> (NP.string "move " *> NP.decimal)
        <*> (NP.string " from " *> NP.decimal)
        <*> (NP.string " to " *> NP.decimal)

main :: IO ()
main = pureMain $ \str -> do
    input <- NP.runParser parseInput str
    let oneByOne = do
            (n, from, to) <- inputMoves input
            replicate n $ (1, from, to)
        part1 = apply oneByOne (inputStacks input)
        part2 = apply (inputMoves input) (inputStacks input)
        tops = map (head . snd) . M.toAscList
    pure (pure (tops part1), pure (tops part2))
