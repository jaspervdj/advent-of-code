{-# LANGUAGE RecordWildCards #-}
import           AdventOfCode.Main
import           AdventOfCode.Parsing (ints)
import qualified Data.IntMap          as IM

type Turn = Int

data Game = Game
    { gameRecent :: !(IM.IntMap Turn)  -- ^ Most recent occurences
    , gameTurn   :: !Turn              -- ^ The current turn
    , gameNumber :: !Int               -- ^ Number said at the current turn
    } deriving (Show)

start :: [Int] -> Game
start numbers = Game
    { gameRecent = IM.fromList $ zip numbers [1 ..]
    , gameTurn   = length numbers
    , gameNumber = last numbers
    }

step :: Game -> Game
step Game {..} = Game
    { gameRecent = IM.insert gameNumber gameTurn gameRecent
    , gameTurn   = gameTurn + 1
    , gameNumber = maybe 0 (gameTurn -) $ IM.lookup gameNumber gameRecent
    }

main :: IO ()
main = simpleMain $ \input ->
    let gameStates = iterate step . start $ ints input in
    ( gameNumber . head $ dropWhile ((< 2020)     . gameTurn) gameStates
    , gameNumber . head $ dropWhile ((< 30000000) . gameTurn) gameStates
    )
