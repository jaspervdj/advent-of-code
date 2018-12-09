{-# LANGUAGE BangPatterns #-}
import qualified Data.List     as L
import qualified Data.Map      as M
import qualified Data.Sequence as Seq

--------------------------------------------------------------------------------
-- Ring of marbles and simple operations on it

data Ring a = Ring
    { rSeq     :: !(Seq.Seq a)
    , rSize    :: !Int
    , rCurrent :: !Int
    } deriving (Show)

initial :: Ring Int
initial = Ring (Seq.singleton 0) 1 0

clockwise :: Int -> Ring a -> Ring a
clockwise n r = r {rCurrent = (rCurrent r + n) `mod` rSize r}

counterclockwise :: Int -> Ring a -> Ring a
counterclockwise = clockwise . negate

insert :: a -> Ring a -> Ring a
insert x r =
    r {rSize = rSize r + 1, rSeq = Seq.insertAt (rCurrent r) x (rSeq r)}

pop :: Ring a -> Maybe (a, Ring a)
pop r =
    let (ls, rs) = Seq.splitAt (rCurrent r) (rSeq r) in
    case Seq.viewl rs of
        Seq.EmptyL   -> Nothing
        x Seq.:< rs' -> Just (x, r {rSize = rSize r - 1, rSeq = ls <> rs'})

--------------------------------------------------------------------------------
-- Playing the game.

type Player = Int

play :: Int -> Int -> M.Map Player Int
play numPlayers numMarbles = go M.empty initial 0 1
  where
    go !scores !ring !player !marble
        | marble > numMarbles = scores
        | marble `mod` 23 == 0 =
            let Just (m, ring') = pop (counterclockwise 7 ring) in
            go
                (M.insertWith (+) player (m + marble) scores)
                ring' (next player) (marble + 1)
        | otherwise =
            go
                scores
                (insert marble (clockwise 2 ring))
                (next player)
                (marble + 1)

    next player = (player + 1) `mod` numPlayers

--------------------------------------------------------------------------------
-- Main

main :: IO ()
main = do
    ws <- words <$> getContents
    let numPlayers = read (ws !! 0)
        numMarbles = read (ws !! 6)
    print $ L.maximum $ map snd $ M.toList $ play numPlayers numMarbles
    print $ L.maximum $ map snd $ M.toList $ play numPlayers (numMarbles * 100)
