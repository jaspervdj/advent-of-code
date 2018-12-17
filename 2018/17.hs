module Main where

import qualified AdventOfCode.Grid   as G
import           AdventOfCode.V2
import qualified AdventOfCode.V2.Box as Box
import qualified Data.List           as L
import qualified Data.Map            as M
import           Data.Maybe          (mapMaybe)
import qualified System.IO           as IO
import           Text.Read           (readMaybe)

--------------------------------------------------------------------------------
-- Parsing

parseCompressedGrid :: IO.Handle -> IO (G.Grid ())
parseCompressedGrid h = do
    coords <- IO.hGetContents h >>= mapM parseLine . lines
    return $ M.fromList $ zip (concat coords) (repeat ())
  where
    parseLine s0
        | xyl : '=' : s1 <- s0
        , (num0, ',' : ' ' : s2) <- break (== ',') s1
        , xyr : '=' : s3 <- s2
        , (num1, '.' : '.' : s4) <- break (== '.') s3
        , num2 <- s4
        , xyl `elem` "xy"
        , xyr `elem` "xy" && xyl /= xyr
        , [l, rlo, rhi] <- mapMaybe readMaybe [num0, num1, num2] = case xyl of
            'x' -> return [V2 l y | y <- [rlo .. rhi]]
            _   -> return [V2 x l | x <- [rlo .. rhi]]

    parseLine other = fail $ "Could not parse line: " ++ show other

--------------------------------------------------------------------------------
-- Dropping

data Soil = Clay | Sand | FlowingWater | RestingWater deriving (Eq, Ord, Show)

showSoil :: Soil -> Char
showSoil Clay         = '#'
showSoil Sand         = '.'
showSoil FlowingWater = '|'
showSoil RestingWater = '~'

pass :: Soil -> Bool
pass soil = soil == Sand || soil == FlowingWater

inserts :: [G.Pos] -> a -> G.Grid a -> G.Grid a
inserts pos x grid = L.foldl' (\g p -> M.insert p x g) grid pos

flow
    :: Int          -- ^ Max Y
    -> G.Pos        -- ^ Starting position
    -> G.Grid Soil  -- ^ Initial grid
    -> G.Grid Soil  -- ^ Wet cells, final grid
flow maxY =
    \pos0 grid0 -> case fall pos0 (M.insert pos0 FlowingWater grid0) of
        -- Water went of the grid, this means we can stop.
        Left grid1 -> grid1
        -- Water "bounced" on something.
        Right (pos1, grid1) ->
            -- We search both left and right. grid1
            let lsearch = search grid1 lx [pos1] pos1
                rsearch = search grid1 rx [pos1] pos1 in
            case (lsearch, rsearch) of
                -- Water was stopped in both branches.
                (Left lw, Left rw) ->
                    let grid2 = inserts (lw ++ rw) RestingWater grid1 in
                    -- If the water was stopped, we can try going one "up"
                    -- and restarting the flow from there.
                    if vY pos1 > 0 then
                        flow maxY (pos1 .-. V2 0 1) grid2
                    else
                        grid2

                -- Water flows to the right.
                (Left lw, Right (down, rw)) ->
                    let grid2 = inserts (lw ++ rw) FlowingWater grid1 in
                    flow maxY down grid2

                -- Water flows to the left.
                (Right (down, lw), Left rw) ->
                    let grid2 = inserts (lw ++ rw) FlowingWater grid1 in
                    flow maxY down grid2

                -- Water flows to both sides.
                (Right (ldown, lw), Right (rdown, rw)) ->
                    let grid2 = inserts (lw ++ rw) FlowingWater grid1
                        grid3 = flow maxY ldown grid2
                        grid4 = flow maxY rdown grid3 in
                    grid4
  where
    passable p = maybe True pass . M.lookup p

    fall pos0 grid0
        -- Bail out because we've fallen of the grid.
        | vY down > maxY                           = Left grid0
        -- Bail out because we've been here before.
        | Just FlowingWater <- M.lookup down grid0 = Left grid0
        -- We can fall further down.
        | passable down grid0                      = fall down grid1
        -- We need to stop here because we hit resting water or clay.
        | otherwise                                = Right (pos0, grid0)
      where
        grid1 = M.insert down FlowingWater grid0
        down  = pos0 .+. V2 0 1

    search g dir acc pos0
        | not (passable pos1 g) = Left acc
        | passable pos1down g   = Right (pos1, pos1 : acc)
        | otherwise             = search g dir (pos1 : acc) pos1
      where
        pos1     = pos0 .+. dir
        pos1down = pos1 .+. V2 0 1

    lx = V2 (-1) 0
    rx = V2 1 0

--------------------------------------------------------------------------------
-- Main

main :: IO ()
main = do
    grid <- fmap (const Clay) <$> parseCompressedGrid IO.stdin

    -- Part 1
    let Just (Box.Box (V2 _ minY) (V2 _ maxY)) = G.box grid
        final = flow maxY (V2 500 minY) grid
    G.printGrid IO.stderr $ fmap showSoil final
    print $ length $ filter (`elem` [RestingWater, FlowingWater]) $
        map snd $ M.toList final

    -- Part 2
    print $ length $ filter (== RestingWater) $ map snd $ M.toList final
