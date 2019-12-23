module Main
    ( main
    ) where

import           AdventOfCode.IntCode
import qualified AdventOfCode.NanoParser as NP
import           Control.Monad           (forM_)
import           Control.Monad.State     (State, modify, runState, state)
import qualified Data.IntMap             as IM
import           Data.Maybe              (fromMaybe, listToMaybe, mapMaybe)
import qualified System.IO               as IO

type MQueue  = IM.IntMap [(Int, Int)]
type Network = (IM.IntMap Machine, MQueue)

stepNetwork :: Network -> Network
stepNetwork = \(nodes, queue) ->
    runState (IM.traverseWithKey step nodes) queue
  where
    step :: Int -> Machine -> State MQueue Machine
    step address machine0 = do
        -- Pop input packets.
        inPacket <- state $ \queue -> case IM.lookup address queue of
            Just (p : ps) -> (Just p, IM.insert address ps queue)
            _             -> (Nothing, queue)

        -- Run machine with inputs.
        let (outputs, _, machine1) = runMachine machine0 {mInputs = inputs}
            fixEmptyInput ls       = if null ls then [-1] else ls
            inputs                 = fixEmptyInput $
                mInputs machine0 ++
                (case inPacket of
                    Nothing     -> []
                    Just (x, y) -> [x, y])

        -- Send outputs.
        forM_ (triples outputs) $ \(dst, x, y) ->
            modify $ IM.insertWith (++) dst [(x, y)]

        pure machine1

data Nat = Nat Int (Int, Int) (Maybe (Int, Int))

mkNat :: Int -> Nat
mkNat address = Nat address (0, 0) Nothing

stepNetworkNat :: (Network, Nat) -> (Network, Nat)
stepNetworkNat (network0, Nat naddress mem0 _) =
    let -- Step network.
        (nodes1, queue1) = stepNetwork network0

        -- Update NAT.
        (natPackets, queue2) = case IM.lookup naddress queue1 of
            Nothing -> ([], queue1)
            Just ps -> (ps, IM.insert naddress [] queue1)
        mem1 = fromMaybe mem0 . listToMaybe . reverse $ natPackets

        -- Sent packet if all stay idle.
        sent    = if all null queue1 then Just mem1 else Nothing
        queue3  = case sent of
            Nothing     -> queue2
            Just (x, y) -> IM.insertWith (++) 0 [(x, y)] queue2 in

    ((nodes1, queue3), Nat naddress mem1 sent)

triples :: [a] -> [(a, a, a)]
triples (x : y : z : ls) = (x, y, z) : triples ls
triples _                = []

twice :: Eq a => [a] -> Maybe a
twice (x : y : ls) = if x == y then Just x else twice (y : ls)
twice _            = Nothing

main :: IO ()
main = do
    prog <- NP.hRunParser IO.stdin parseProgram
    let nodes = IM.fromList [(n, initMachine [n] prog) | n <- [0 .. 49]]
        y255  = \(_, queue) -> snd . head <$> IM.lookup 255 queue
    print . head . mapMaybe y255 $ iterate stepNetwork (nodes, IM.empty)
    print . fromMaybe 0 . twice .
        mapMaybe (\(_, Nat _ _ out) -> snd <$> out) $
        iterate stepNetworkNat ((nodes, IM.empty), mkNat 255)
