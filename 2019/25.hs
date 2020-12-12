{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
import           AdventOfCode.IntCode
import           AdventOfCode.Main
import           AdventOfCode.Dijkstra (bfs, bfsGoal)
import qualified AdventOfCode.NanoParser as NP
import           Control.Applicative     (Alternative (..))
import           Control.Monad           (join, guard)
import           Control.Monad.State     (StateT, get, gets, modify, runStateT)
import           Control.Monad.Trans     (lift)
import           Data.Foldable           (for_)
import qualified Data.List.Extended      as L
import qualified Data.Map                as Map
import           Debug.Trace
import qualified System.IO               as IO

--------------------------------------------------------------------------------

data Bot a
    = Err String
    | Ok a
    | Output String (Bot a)
    | Await (String -> (Maybe String, Bot a))
    deriving (Functor)

instance Applicative Bot where
    pure = Ok
    f <*> x = f >>= (<$> x)

instance Monad Bot where
    Err err    >>= _ = Err err
    Ok x       >>= f = f x
    Output o b >>= f = Output o (b >>= f)
    Await n >>= f    = Await $ \i -> let (o, step) = n i in (o, step >>= f)

instance MonadFail Bot where
    fail = Err

readInput :: Bot String
readInput = Await $ \i -> (Nothing, Ok i)

readUntil :: String -> Bot [String]
readUntil marker = do
    str <- readInput
    if str == marker then pure [] else (str :) <$> readUntil marker

writeOutput :: String -> Bot ()
writeOutput str = Output str (Ok ())

--------------------------------------------------------------------------------

runBot :: Bot a -> Program -> Either String a
runBot step0 program =
    let (inputs, result) = go step0 $ filter (not . null) outputs
        outputs = lines $ runAsciiMachine (unlines inputs) program
        go (Ok x) _ = ([], Right x)
        go (Err e) _ = ([], Left e)
        go (Output o b) is = let (os, r) = go b is in (o : os, r)
        go (Await _) [] = ([], Left "Ran out of output!")
        go (Await f) (i : is) = trace ("< " ++ i) $ case f i of
           (Nothing, step) -> go step is
           (Just o, step) -> trace ("> " ++ o) $
               let (os, r) = go step is in (o : os, r) in
   result

--------------------------------------------------------------------------------

newtype RoomName = RoomName String deriving (Eq, Ord, Show)
newtype Door = Door String deriving (Eq, Ord, Show)

data Room = Room
    { roomName        :: RoomName
    , roomDescription :: String
    , roomDoors       :: [Door]
    , roomItems       :: [String]
    } deriving (Show)

readRoom :: Bot Room
readRoom = do
    roomName <- readName
    roomDescription <- readInput
    info <- readUntil "Command?"

    let roomDoors = case break (== "Doors here lead:") info of
            (_, _ : remainder) -> map (Door . drop 2) $
                takeWhile ("- " `L.isPrefixOf`) remainder
            _ -> []
        roomItems = case break (== "Items here:") info of
            (_, _ : remainder) -> map (drop 2) $
                takeWhile ("- " `L.isPrefixOf`) remainder
            _ -> []

    pure Room {..}
  where
    readName = do
        str <- readInput
        case L.stripPrefix "== " str >>= L.stripSuffix " ==" of
            Nothing -> fail "No room name"
            Just x  -> pure $ RoomName x

data Explorer = Explorer
    { explorerRooms :: Map.Map RoomName Room
    , explorerEdges :: Map.Map (RoomName, Door) (Maybe RoomName)
    } deriving (Show)

emptyExplorer :: Explorer
emptyExplorer = Explorer Map.empty Map.empty

data Ann i a = Ann i a deriving (Show)

instance Eq a => Eq (Ann i a) where
    Ann _ x == Ann _ y = x == y

instance Ord a => Ord (Ann i a) where
    compare (Ann _ x) (Ann _ y) = compare x y

data Path = Path (Maybe Door) RoomName deriving (Show)

instance Eq Path where
    Path _ x == Path _ y = x == y

instance Ord Path where
    compare (Path _ x) (Path _ y) = compare x y

findPath :: RoomName -> RoomName -> StateT Explorer Bot (Maybe [Door])
findPath src dst = do
    Explorer {..} <- get
    pure $ do
        path <- fmap snd $ bfsGoal $ bfs
            (\(Path _ r) -> do
                ((r', d), Just nb) <- Map.toList explorerEdges
                guard $ r == r'
                pure $ Path (Just d) nb)
            (\(Path _ r) -> r == dst)
            (Path Nothing src)
        traverse (\(Path d _) -> d) . drop 1 $ reverse path

walkPath :: [(RoomName, Door)] -> StateT Explorer Bot ()
walkPath [] = pure ()
walkPath ((_, Door x) : xs) = do
    lift $ writeOutput x
    case xs of
        [] -> pure ()
        _ : _ -> do
            _ <- lift readRoom
            walkPath xs

explore :: Maybe (RoomName, Door) -> StateT Explorer Bot ()
explore mbPrev = do
    room <- lift readRoom
    traceM $ show room

    -- Record edge we took.
    for_ mbPrev $ \edge -> modify $ \e@Explorer {..} ->
        e {explorerEdges = Map.insert edge (Just $ roomName room) explorerEdges}

    -- Fill in info about room.
    modify $ \e@Explorer {..} -> e
        { explorerRooms = Map.insert (roomName room) room explorerRooms
        , explorerEdges = Map.unionWith const explorerEdges $ Map.fromList
             [ ((roomName room, door), Nothing)
             | door <- roomDoors room
             ]
        }

    -- Find a new room to explore.
    edges <- gets explorerEdges
    let pathToUnexplored = do
            path <- bfsGoal $ bfs
                (\case
                    Ann _ Nothing  -> []
                    Ann _ (Just r) ->
                        [ Ann (Just (r', d)) nb
                        | ((r', d), nb) <- Map.toList edges
                        , r' == r
                        ])
                (\case
                    Ann _ Nothing -> True
                    Ann _ (Just _) -> False)
                (Ann Nothing . Just $ roomName room)
            traverse (\(Ann p _) -> p) . drop 1 . reverse $ snd path

    case pathToUnexplored of
        Nothing -> pure ()
        Just path -> do
            walkPath path
            traceM $ show path
            explore $ L.safeLast path

main :: IO ()
main = defaultMain $ \problem -> do
    program <- NP.hRunParser problem parseProgram
    case runBot (runStateT (explore Nothing) emptyExplorer) program of
        Left err            -> fail err
        Right (_, explorer) -> print explorer
    runAsciiMachineIO (IO.stdin, IO.stdout) program
    pure (pure (), pure ())
