import           AdventOfCode.Main
import qualified AdventOfCode.NanoParser as NP
import qualified AdventOfCode.Queue      as Q
import           Control.Applicative     ((<|>))
import           Control.Monad           (when)
import           Control.Monad.Except    (throwError)
import           Data.List               (foldl', foldl1')
import qualified Data.Map                as M

type Identifier = String
data ModuleType = Broadcast | FlipFlop | Conjunction deriving (Show)
type Spec = M.Map Identifier (ModuleType, [Identifier])

parseSpec :: NP.Parser Char Spec
parseSpec = fmap M.fromList $ NP.many1 $
    (\((t, i), cs) -> (i, (t, cs))) <$> modul
  where
    ident = NP.many1 NP.alpha
    tok p = p <* NP.spaces

    modul = (,)
        <$> (tok header <* tok (NP.string "->"))
        <*> NP.sepBy1 (tok ident) (tok (NP.char ','))

    header =
        ((Broadcast, "broadcaster") <$ NP.string "broadcaster") <|>
        ((,) FlipFlop <$> (NP.char '%' *> ident)) <|>
        ((,) Conjunction <$> (NP.char '&' *> ident))

data Pulse = Low | High deriving (Eq, Show)
newtype Module = Module ((Identifier, Pulse) -> (Module, Maybe Pulse))
type Modules = M.Map Identifier Module
type Event = (Identifier, Pulse, Identifier)

inputs :: Spec -> Identifier -> [Identifier]
inputs spec ident = [i | (i, (_, os)) <- M.toList spec, ident `elem` os]

makeModules :: Spec -> Modules
makeModules spec = M.mapWithKey makeModule spec
  where
    makeModule _ (Broadcast, _) =
        let m = Module (\(_, pulse) -> (m, Just pulse)) in m
    makeModule _ (FlipFlop, _) =
        let m st = Module $ \(_, pulse) -> case pulse of
                High -> (m st, Nothing)
                Low  -> (m (not st), Just $ if st then Low else High) in
        m False
    makeModule ident (Conjunction, _) =
        let m st = Module $ \(input, pulse) ->
                let st' = M.insert input pulse st in
                (m st', Just $ if all (== High) st' then Low else High) in
        m . M.fromList $ zip (inputs spec ident) (repeat Low)

data State w = State
    { stateSpec     :: !Spec
    , stateModules  :: !Modules
    , stateQueue    :: !(Q.Queue Event)
    , stateLogEvent :: !(Event -> w)  -- Logs a single event
    , stateLog      :: !w             -- Accumulated log
    }

step :: Spec -> Event -> Modules -> (Modules, [Event])
step spec (src, pulse, dst) modules = case M.lookup dst modules of
    Nothing -> (modules, [])
    Just (Module module0) ->
        let (module1, mbOut) = module0 (src, pulse)
            outs             = case mbOut of
                Nothing -> []
                Just o  -> [(dst, o, n) | n <- snd $ spec M.! dst] in
        (M.insert dst module1 modules, outs)

simulate :: Monoid w => State w -> State w
simulate state = case Q.pop (stateQueue state) of
    Nothing           -> state
    Just (ev, queue0) ->
        let (mods, evs) = step (stateSpec state) ev (stateModules state) in
        simulate $ state
            { stateQueue   = foldl' (\q e -> Q.push e q) queue0 evs
            , stateModules = mods
            , stateLog     = stateLog state <> stateLogEvent state ev
            }

send :: Monoid w => Event -> State w -> State w
send event state = simulate $ state
    { stateQueue = Q.push event (stateQueue state)
    }

initState :: Monoid w => Spec -> (Event -> w) -> State w
initState spec f = State spec (makeModules spec) Q.empty f mempty

button :: Event
button = ("button", Low, "broadcaster")

-- Remembers high and lows
data LogP1 = LogP1 !Int !Int

instance Semigroup LogP1 where
    LogP1 lo0 hi0 <> LogP1 lo1 hi1 = LogP1 (lo0 + lo1) (hi0 + hi1)

instance Monoid LogP1 where
    mempty = LogP1 0 0

logP1 :: Event -> LogP1
logP1 (_, Low,  _) = LogP1 1 0
logP1 (_, High, _) = LogP1 0 1

part1 :: Spec -> Int
part1 spec = lo1 * hi1
  where
    LogP1 lo1 hi1 = stateLog $
        iterate (send button) (initState spec logP1) !! 1000

-- Remembers the first high for each module, and the number of button presses
data LogP2 = LogP2 !(M.Map Identifier Int) !Int deriving (Show)

instance Semigroup LogP2 where
    LogP2 hi0 but0 <> LogP2 hi1 but1 = LogP2
        (M.unionWith min hi0 $ fmap (+ but0) hi1) (but0 + but1)

instance Monoid LogP2 where
    mempty = LogP2 mempty 0

logP2 :: Event -> LogP2
logP2 ("button", Low,  _) = LogP2 mempty 1
logP2 (src,      High, _) = LogP2 (M.singleton src 0) 0
logP2 _                   = LogP2 mempty 0

part2 :: Spec -> Either String Int
part2 spec = do
    con <- case [k | (k, (Conjunction, ns)) <- M.toList spec, "rx" `elem` ns] of
        [k] -> pure k
        _   -> throwError "no single con"
    let prevs = [k | (k, (_, ns)) <- M.toList spec, con `elem` ns]
    when (null prevs) $ throwError "no prevs found"
    -- Iterate until we find a first high for each of the inputs to con.
    let (LogP2 his _) = head $
            dropWhile (\(LogP2 found _) ->
                not $ all (`M.member` found) prevs) $
            map stateLog $ iterate (send button) (initState spec logP2)
    pure $ foldl1' lcm $ map (his M.!) prevs

main :: IO ()
main = pureMain $ \str -> do
    spec <- NP.runParser parseSpec str
    pure (pure (part1 spec), part2 spec)
