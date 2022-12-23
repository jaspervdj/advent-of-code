{-# LANGUAGE BangPatterns #-}
import qualified AdventOfCode.Grid   as G
import           AdventOfCode.Main   (simpleMain)
import           AdventOfCode.V2     (V2 (..))
import qualified AdventOfCode.V2.Box as Box
import           Data.Foldable       (find, foldl')
import qualified Data.Map            as M
import           Data.Maybe          (mapMaybe)
import qualified Data.Set            as S

parseGrid :: String -> S.Set G.Pos
parseGrid = M.keysSet . M.filter (== '#') . G.fromString

data PrimaryDir = N | E | S | W
    deriving (Bounded, Enum, Eq, Show)

primaryDirs :: [PrimaryDir]
primaryDirs = [N, E, S, W]

data Dir = Primary PrimaryDir | NE | SE | SW | NW
    deriving (Eq, Show)

dirs :: [Dir]
dirs = fmap Primary primaryDirs ++ [NE, SE, SW, NW]

diagonals :: PrimaryDir -> [Dir]
diagonals N = [NW, NE]
diagonals E = [NE, SE]
diagonals S = [SE, SW]
diagonals W = [SW, NW]

moveCompass :: Dir -> G.Pos -> G.Pos
moveCompass dir (V2 x y) = case dir of
    Primary N -> V2  x      (y - 1)
    NE        -> V2 (x + 1) (y - 1)
    Primary E -> V2 (x + 1)  y
    SE        -> V2 (x + 1) (y + 1)
    Primary S -> V2  x      (y + 1)
    SW        -> V2 (x - 1) (y + 1)
    Primary W -> V2 (x - 1)  y
    NW        -> V2 (x - 1) (y - 1)

proposeElves :: [PrimaryDir] -> S.Set G.Pos -> [(G.Pos, G.Pos)]
proposeElves order0 original = mapMaybe (proposeElf order0) $ S.toList original
  where
    free pos = all ((`S.notMember` original) . flip moveCompass pos)

    proposeElf order pos
        | free pos dirs = Nothing
        | otherwise     = do
            prim <- find (\d -> free pos (Primary d : diagonals d)) order
            pure (pos, moveCompass (Primary prim) pos)

moveElves :: [(G.Pos, G.Pos)] -> S.Set G.Pos -> S.Set G.Pos
moveElves proposals original = foldl' move original proposals
  where
    contended =
        M.keysSet . M.filter (>= 2) $
        M.fromListWith (+) [(dst, 1 :: Int) | (_, dst) <- proposals]

    move acc (src, dst)
        | dst `S.member` contended = acc
        | otherwise                = S.insert dst $ S.delete src acc

data State = State (S.Set G.Pos) [PrimaryDir]

instance Show State where
    show (State pos dir) =
        G.toString (M.fromSet (const '#') pos) ++
        "next: " ++ show dir

stepElves :: State -> State
stepElves (State pos dir) =
    let proposals = proposeElves dir pos in
    State (moveElves proposals pos) (tail dir ++ [head dir])

elvesArea :: State -> Int
elvesArea (State pos _) =
    let Just box = foldMap (Just . Box.fromV2) pos in
    Box.area box - S.size pos

main :: IO ()
main = simpleMain $ \input ->
    let elves0  = State (parseGrid input) [N, S, W, E]
        elves10 = iterate stepElves elves0 !! 10

        stable !i s@(State ps _) =
            let n@(State pn _) = stepElves s in
            if pn == ps then i else stable (i + 1) n in

    (elvesArea elves10, stable (1 :: Int) elves0)
