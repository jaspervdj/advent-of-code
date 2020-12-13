{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
import           AdventOfCode.Dijkstra   (bfs, bfsGoal)
import           AdventOfCode.IntCode
import           AdventOfCode.Main
import qualified AdventOfCode.NanoParser as NP
import           Control.Monad           (guard, msum)
import           Control.Monad.State     (StateT, gets, modify, runStateT)
import           Control.Monad.Trans     (lift)
import           Data.Foldable           (for_)
import qualified Data.List.Extended      as L
import qualified Data.Map                as Map
import           Data.Maybe              (fromMaybe, isNothing)
import qualified Data.Set                as Set
import           Debug.Trace

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

newtype RoomName = RoomName {unRoomName :: String} deriving (Eq, Ord, Show)
newtype Door = Door String deriving (Eq, Ord, Show)

data Room = Room
    { roomName        :: RoomName
    , roomDescription :: String
    , roomDoors       :: [Door]
    , roomItems       :: [String]
    , roomEjectedTo   :: Maybe RoomName
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

        roomEjectedTo = msum $ map parseName info

    pure Room {..}
  where
    parseName x = L.stripPrefix "== " x >>= fmap RoomName . L.stripSuffix " =="
    readName = readInput >>= maybe (fail "No room name") pure . parseName

data Explorer = Explorer
    { explorerRooms :: Map.Map RoomName Room
    , explorerEdges :: Map.Map (RoomName, Door) (Maybe RoomName)
    , explorerItems :: Set.Set String
    } deriving (Show)

emptyExplorer :: Explorer
emptyExplorer = Explorer Map.empty Map.empty Set.empty

prettyExplorer :: Explorer -> String
prettyExplorer Explorer {..} = unlines $ concat $
    [ prettyRoom room | (_, room) <- Map.toList explorerRooms
    ] ++
    ["Inventory" : ["- " ++ i | i <- Set.toList explorerItems]]
  where
    prettyRoom Room {..} = do
        let header = unRoomName roomName ++
                (if isNothing roomEjectedTo then "" else " XXX") ++
                " (" ++ L.intercalate ", " roomItems ++ ")"
        header : do
            ((src, Door d), dst) <- Map.toList explorerEdges
            guard $ src == roomName
            pure $ "- " ++ d ++ " -> " ++ maybe "?" unRoomName dst

data Ann i a = Ann i a deriving (Show)

instance Eq a => Eq (Ann i a) where
    Ann _ x == Ann _ y = x == y

instance Ord a => Ord (Ann i a) where
    compare (Ann _ x) (Ann _ y) = compare x y

findPath
    :: RoomName -> (Maybe RoomName -> Bool)
    -> StateT Explorer Bot (Maybe [(RoomName, Door)])
findPath start goal = do
    edges <- gets explorerEdges
    pure $ do
        path <- bfsGoal $ bfs
            (\case
                Ann _ Nothing  -> []
                Ann _ (Just r) ->
                    [ Ann (Just (r', d)) nb
                    | ((r', d), nb) <- Map.toList edges
                    , r' == r
                    ])
            (\(Ann _ rn) -> goal rn)
            (Ann Nothing $ Just start)
        traverse (\(Ann p _) -> p) . drop 1 . reverse $ snd path

walkPath :: [(RoomName, Door)] -> StateT Explorer Bot ()
walkPath [] = pure ()
walkPath ((_, Door x) : xs) = do
    lift $ writeOutput x
    case xs of
        [] -> pure ()
        _ : _ -> do
            _ <- lift readRoom
            walkPath xs

forbidden :: Set.Set String
forbidden = Set.fromList
    [ "escape pod"
    , "infinite loop"
    , "molten lava"
    , "giant electromagnet"
    , "photons"
    ]

explore :: Maybe (RoomName, Door) -> StateT Explorer Bot RoomName
explore mbPrev = do
    -- Record current location.
    room <- lift readRoom
    let location = fromMaybe (roomName room) $ roomEjectedTo room

    -- Record edge we took.
    for_ mbPrev $ \edge -> modify $ \e@Explorer {..} ->
        e {explorerEdges = Map.insert edge (Just $ roomName room) explorerEdges}

    -- Fill in info about room.
    modify $ \e@Explorer {..} -> e
        { explorerRooms = Map.insert (roomName room) room explorerRooms
        , explorerEdges = Map.unionWith const explorerEdges $ Map.fromList
             [ ((roomName room, door), Nothing)
             | isNothing $ roomEjectedTo room  -- Don't add that edge.
             , door <- roomDoors room
             ]
        }

    -- Pick up all items.
    for_ (filter (not . (`Set.member` forbidden)) $ roomItems room) $ \item -> do
        lift . writeOutput $ "take " <> item
        _ <- lift $ readUntil "Command?"
        modify $ \e -> e {explorerItems = Set.insert item $ explorerItems e}
        pure ()

    -- Find a new room to explore.
    pathToUnexplored <- findPath location isNothing
    case pathToUnexplored of
        Nothing -> pure location
        Just path -> do
            walkPath path
            explore $ L.safeLast path

solve :: RoomName -> StateT Explorer Bot ()
solve start = do
    Just toSec <- findPath start (== Just (RoomName "Security Checkpoint"))
    walkPath toSec
    _ <- lift readRoom
    items <- gets $ Set.toList . explorerItems
    for_ (L.powerset items) $ \combination -> do
        for_ combination $ \item -> do
            lift . writeOutput $ "drop " <> item
            _ <- lift $ readUntil "Command?"
            pure ()
        lift $ writeOutput "west"
        _ <- lift $ readUntil "Command?"
        for_ combination $ \item -> do
            lift . writeOutput $ "take " <> item
            _ <- lift $ readUntil "Command?"
            pure ()

main :: IO ()
main = defaultMain $ \problem -> do
    program <- NP.hRunParser problem parseProgram
    case runBot (runStateT (explore Nothing >>= solve) emptyExplorer) program of
        Left err            -> putStrLn err
        Right (_, explorer) -> putStrLn $ prettyExplorer explorer
    pure (pure (), pure ())
