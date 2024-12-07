import           AdventOfCode.Main
import qualified AdventOfCode.NanoParser as NP
import qualified AdventOfCode.Queue      as Q
import           Control.Applicative     ((<|>))
import           Control.Monad           (when)
import           Control.Monad.Except    (throwError)
import           Data.Foldable           (toList)
import           Data.List               (foldl', foldl1')
import           Data.List.NonEmpty      (NonEmpty)
import qualified Data.Map                as M

type Identifier = String
data ModuleType = Broadcast | FlipFlop | Conjunction deriving (Show)
type Spec = M.Map Identifier (ModuleType, NonEmpty Identifier)

parseSpec :: NP.Parser Char Spec
parseSpec = fmap (M.fromList . toList) $ NP.many1 $
    (\((t, i), cs) -> (i, (t, cs))) <$> modul
  where
    ident = toList <$> NP.many1 NP.alpha
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

send
    :: Monoid w => Spec -> (Event -> w) -> Event -> (Modules, w) -> (Modules, w)
send spec logEvent event = go (Q.singleton event)
  where
    go queue0 (modules0, writer) = case Q.pop queue0 of
        Nothing           -> (modules0, writer)
        Just (ev, queue1) ->
            let (modules1, evs) = deliver ev modules0
                queue2 = foldl' (\q e -> Q.push e q) queue1 evs in
            go queue2 (modules1, writer <> logEvent ev)

    deliver (src, pulse, dst) modules = case M.lookup dst modules of
        Nothing -> (modules, [])
        Just (Module module0) ->
            let (module1, mbOut) = module0 (src, pulse)
                outs             = case mbOut of
                    Nothing -> []
                    Just o  ->
                        [(dst, o, n) | n <- toList . snd $ spec M.! dst] in
            (M.insert dst module1 modules, outs)

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
    (_, LogP1 lo1 hi1) =
        iterate (send spec logP1 button) (makeModules spec, mempty) !! 1000

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
    prevs <- case inputs spec "rx" of
        [k] -> pure $ inputs spec k
        _   -> throwError "no single con"
    when (null prevs) $ throwError "no prevs found"
    -- Iterate until we find a first high for each of the inputs to con.
    let (LogP2 his _) = head $
            dropWhile (\(LogP2 found _) ->
                not $ all (`M.member` found) prevs) $
            map snd $ iterate (send spec logP2 button) (makeModules spec, mempty)
    pure $ foldl1' lcm $ map (his M.!) prevs

main :: IO ()
main = pureMain $ \str -> do
    spec <- NP.runParser parseSpec str
    pure (pure (part1 spec), part2 spec)
