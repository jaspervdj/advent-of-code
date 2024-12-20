import qualified AdventOfCode.Dijkstra   as Dijkstra
import qualified AdventOfCode.Grid       as G
import qualified AdventOfCode.NanoParser as NP
import           AdventOfCode.V2         (zero)
import           Control.Applicative     (many, (<|>))
import           Control.Monad           (guard)
import qualified Data.List               as L
import qualified Data.Map                as M
import           Data.Maybe              (isJust, maybeToList)
import qualified Data.Set                as S

--------------------------------------------------------------------------------
-- Regex its parser

type Regex a = [Node a]
data Node a = Atom a | Choice [Regex a] deriving (Show)

parseRegex :: NP.Parser Char a -> NP.Parser Char (Regex a)
parseRegex parseAtom = between '^' '$' (many node)
  where
    between l r p = NP.char l *> p <* NP.char r
    node          =
        (Choice <$> between '(' ')' (NP.sepBy (many node) (NP.char '|'))) <|>
        (Atom <$> parseAtom)

parseDir :: NP.Parser Char G.Dir
parseDir =
    (NP.char 'N' *> pure G.U) <|> (NP.char 'E' *> pure G.R) <|>
    (NP.char 'S' *> pure G.D) <|> (NP.char 'W' *> pure G.L)

--------------------------------------------------------------------------------
-- We construct all the doors the walks for a regex passes through.

type Door = (G.Pos, G.Dir)

walks :: Regex G.Dir -> [Door]
walks = fst . go zero []
  where
    go pos acc []               = (acc, [pos])
    go pos acc (Atom x : xs)    = go (G.move 1 x pos) ((pos, x) : acc) xs
    go pos acc (Choice cs : xs) =
        -- Here, the essential part is the 'L.nub' on the end positions of the
        -- different branches, which allows us to rule out a lot of duplication.
        let branches = map (go pos []) cs
            bends    = L.nub $ concatMap snd branches
            conts    = [go end [] xs | end <- bends]
            doors    = acc ++ concatMap fst branches ++ concatMap fst conts
            ends     = concatMap snd conts in
        (doors, ends)

--------------------------------------------------------------------------------
-- If we have all the doors, we can construct a grid with proper edges

gridFromDoors :: [Door] -> G.Grid (S.Set G.Dir)
gridFromDoors = L.foldl' insertDoor M.empty
  where
    -- We need to add two "edges" for every door.
    insertDoor grid (p, d) =
        M.insertWith S.union p (S.singleton d) $
        M.insertWith S.union (G.move 1 d p) (S.singleton $ G.turnAround d) $
        grid

--------------------------------------------------------------------------------
-- Main...

main :: IO ()
main = do
    Right regex <- NP.runParser (parseRegex parseDir) <$> getContents
    let grid      = gridFromDoors $ walks regex
        distances = fmap fst $ Dijkstra.back $ Dijkstra.dijkstra
            Dijkstra.defaultOptions
                { Dijkstra.start = S.singleton zero
                , Dijkstra.neighbours = \p -> do
                    ds <- maybeToList $ M.lookup p grid
                    d <- S.toList ds
                    let q = G.move 1 d p
                    guard $ isJust $ M.lookup q grid
                    pure (1, q)
                }

    -- Part 1
    print $ L.maximum $ map snd $ M.toList distances

    -- Part 2
    print $ length $ filter (>= 1000) $ map snd $ M.toList distances
