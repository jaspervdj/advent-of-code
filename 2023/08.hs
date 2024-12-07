{-# LANGUAGE BangPatterns #-}
import           AdventOfCode.Main
import qualified AdventOfCode.NanoParser as NP
import           Control.Applicative     ((<|>))
import           Data.Foldable           (toList)
import           Data.List               (findIndex)
import qualified Data.Map                as M
import           Data.Maybe              (fromJust)

type Instr a = (a, a) -> a
type Node  a = (a, a)
data Input a = Input [Instr a] (M.Map a (Node a))

parseInput :: NP.Parser Char (Input String)
parseInput = Input
    <$> (toList <$> NP.many1 instruction <* NP.spaces)
    <*> (M.fromList . toList <$> NP.many1 node)
  where
    instruction = (fst <$ NP.char 'L') <|> (snd <$ NP.char 'R')
    ident       = toList <$> NP.many1 NP.alpha <* NP.spaces
    tok c       = NP.char c *> NP.spaces
    node        = (,)
        <$> ident <* tok '=' <* tok '('
        <*> ((,) <$> ident <* tok ',' <*> ident <* tok ')')

walk :: Ord a => Input a -> a -> [a]
walk (Input instrs0 nodes) = go (cycle instrs0)
  where
    go instrs node = node : go (tail instrs) (head instrs (nodes M.! node))

main :: IO ()
main = pureMain $ \instr -> do
    input@(Input _ nodes) <- NP.runParser parseInput instr
    let part1 = fromJust . findIndex (== "ZZZ") $ walk input "AAA"
        part2 = map (fromJust . findIndex ((== 'Z') . last) . walk input) $
            filter ((== 'A') . last) $ M.keys nodes
    pure (pure part1, pure (foldl1 lcm part2))
