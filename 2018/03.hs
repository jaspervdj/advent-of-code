import qualified AdventOfCode.Grid    as G
import qualified AdventOfCode.Parsing as Parsing
import           AdventOfCode.V2
import qualified Data.List            as L
import qualified Data.Map             as M
import qualified Data.Set             as S
import qualified System.IO            as IO

type Elf = Int

data Claim = Claim Elf G.Pos G.Pos deriving (Show)

parseClaim :: String -> IO Claim
parseClaim input = case Parsing.ints input of
    [i, l, t, w, h] -> return $
        Claim i (V2 l t) (V2 (l + w - 1) (t + h - 1))
    _ -> fail $ "Could not parse claim: " ++ input

parseClaims :: IO.Handle -> IO [Claim]
parseClaims h = IO.hGetContents h >>= mapM parseClaim . lines

type Fabric = G.Grid [Elf]

claim :: Claim -> Fabric -> Fabric
claim (Claim elf (V2 l t) (V2 r b)) fabric = L.foldl'
    (\acc pos -> M.insertWith (++) pos [elf] acc)
    fabric
    positions
  where
    positions = [V2 x y | x <- [l .. r], y <- [t .. b]]

contested :: Fabric -> [(G.Pos, [Elf])]
contested fabric = [(pos, elfs) | (pos, elfs@(_ : _ : _)) <- M.toList fabric]

main :: IO ()
main = do
    claims <- parseClaims IO.stdin
    let fabric     = L.foldl' (\acc c -> claim c acc) M.empty claims
        contested' = contested fabric
    print $ length contested'

    let badElfs  = S.fromList $ concatMap snd contested'
        goodElfs = [elf | Claim elf _ _ <- claims, not (elf `S.member` badElfs)]

    case goodElfs of
        (e : _) -> print e
        _       -> putStrLn "<no good elf>"
