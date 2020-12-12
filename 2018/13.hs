import qualified AdventOfCode.Grid as G
import           AdventOfCode.V2
import           Control.Monad     (guard)
import qualified Data.List         as L
import qualified Data.Map          as M
import           Data.Maybe        (listToMaybe, mapMaybe)
import           Data.Ord          (comparing)
import qualified System.IO         as IO

data Rail = Horizontal | Vertical | Intersection | Slash | Backslash
    deriving (Eq, Show)

data Cart = Cart !G.Dir !Int | Collision
    deriving (Eq, Show)

parseInput :: IO.Handle -> IO (G.Grid Rail, G.Grid Cart)
parseInput h = do
    grid0 <- G.readGrid parseChar h
    let rails = M.mapMaybe fst grid0
        carts = M.mapMaybe (\(_, d) -> Cart <$> d <*> pure 0) grid0
    return (rails, carts)
  where
    parseChar '-'  = return (Just Horizontal, Nothing)
    parseChar '|'  = return (Just Vertical, Nothing)
    parseChar '+'  = return (Just Intersection, Nothing)
    parseChar '/'  = return (Just Slash, Nothing)
    parseChar '\\' = return (Just Backslash, Nothing)
    parseChar '>'  = return (Just Horizontal, Just G.R)
    parseChar '<'  = return (Just Horizontal, Just G.L)
    parseChar '^'  = return (Just Vertical, Just G.U)
    parseChar 'v'  = return (Just Vertical, Just G.D)
    parseChar ' '  = return (Nothing, Nothing)
    parseChar c    = fail $ "Unknown characted: " ++ show c

_printState :: IO.Handle -> (G.Grid Rail, G.Grid Cart) -> IO ()
_printState h (rails, carts) = G.printGrid h $ M.mapWithKey
    (\pos rail -> case (M.lookup pos carts, rail) of
        (Just Collision, _)    -> 'X'
        (Just (Cart G.U _), _) -> '^'
        (Just (Cart G.R _), _) -> '>'
        (Just (Cart G.D _), _) -> 'v'
        (Just (Cart G.L _), _) -> '<'
        (_, Horizontal)        -> '-'
        (_, Vertical)          -> '|'
        (_, Intersection)      -> '+'
        (_, Slash)             -> '/'
        (_, Backslash)         -> '\\')
    rails

tick
    :: Bool
    -> G.Grid Rail
    -> G.Grid Cart
    -> G.Grid Cart
tick removeCollisions rails carts0 =
    L.foldl' (\acc pos -> moveCartAt pos acc) carts0 queue
  where
    -- Positions of carts in the order they need to move.
    queue =
        L.sortBy (comparing (\(V2 x y) -> (y, x))) $
        M.keys carts0

    -- Move a single cart
    moveCartAt pos carts = case M.lookup pos carts of
        -- Nothing to do.
        Nothing         -> carts
        Just Collision  -> carts
        -- Actually move
        Just (Cart dir t) ->
            let pos' = G.move 1 dir pos
                turn = case t `mod` 3 of
                    0 -> G.turnLeft dir
                    1 -> dir
                    _ -> G.turnRight dir
                cart' = case M.lookup pos' rails of
                    _ | pos' `M.member` carts   -> Collision
                    Just Vertical               -> Cart dir t
                    Just Horizontal             -> Cart dir t
                    Just Intersection           -> Cart turn (t + 1)
                    Just Slash     | G.U <- dir -> Cart G.R t
                    Just Slash     | G.R <- dir -> Cart G.U t
                    Just Slash     | G.D <- dir -> Cart G.L t
                    Just Slash     | G.L <- dir -> Cart G.D t
                    Just Backslash | G.U <- dir -> Cart G.L t
                    Just Backslash | G.R <- dir -> Cart G.D t
                    Just Backslash | G.D <- dir -> Cart G.R t
                    Just Backslash | G.L <- dir -> Cart G.U t
                    other                       ->
                        error ("Unexecpected: " ++ show other) in
            if removeCollisions && cart' == Collision then
                M.delete pos' (M.delete pos carts)
            else
                M.insert pos' cart' (M.delete pos carts)

firstCollision :: G.Grid Cart -> Maybe G.Pos
firstCollision carts = do
    ((pos, _), _) <- M.minViewWithKey (M.filter (== Collision) carts)
    return pos

lastCart :: G.Grid Cart -> Maybe G.Pos
lastCart carts = do
    guard $ M.size carts == 1
    ((pos, _), _) <- M.minViewWithKey carts
    return pos

showPos :: G.Pos -> String
showPos (V2 x y) = show x ++ "," ++ show y

main :: IO ()
main = do
    (rails, carts) <- parseInput IO.stdin
    putStrLn $ maybe "<no first collision>" showPos $
        listToMaybe $ mapMaybe firstCollision $ iterate (tick False rails) carts
    putStrLn $ maybe "<no last cart>" showPos $
        listToMaybe $ mapMaybe lastCart $ iterate (tick True rails) carts
