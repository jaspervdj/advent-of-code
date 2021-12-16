{-# LANGUAGE DeriveFoldable   #-}
{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLists  #-}

module Main where

import           AdventOfCode.Main          (pureMain)
import           Control.Monad              (replicateM)
import           Control.Monad.Except       (throwError)
import           Control.Monad.Reader       (ReaderT, ask, runReaderT)
import           Control.Monad.State.Strict (StateT, evalStateT, get, put)
import           Data.Functor.Fix
import           Data.List.NonEmpty         (NonEmpty (..))
import qualified Data.Vector.Unboxed        as VU

--------------------------------------------------------------------------------

type Bin = VU.Vector Bool

fromHex :: Char -> Bin
fromHex = \case
    '0' -> [False, False, False, False]
    '1' -> [False, False, False, True ]
    '2' -> [False, False, True , False]
    '3' -> [False, False, True , True ]
    '4' -> [False, True , False, False]
    '5' -> [False, True , False, True ]
    '6' -> [False, True , True , False]
    '7' -> [False, True , True , True ]
    '8' -> [True , False, False, False]
    '9' -> [True , False, False, True ]
    'A' -> [True , False, True , False]
    'B' -> [True , False, True , True ]
    'C' -> [True , True , False, False]
    'D' -> [True , True , False, True ]
    'E' -> [True , True , True , False]
    'F' -> [True , True , True , True ]
    _   -> []

fromHexString :: String -> Bin
fromHexString = VU.concatMap fromHex . VU.fromList

toInt :: Bin -> Int
toInt = VU.foldl' (\acc x -> acc * 2 + if x then 1 else 0) 0

--------------------------------------------------------------------------------

type Parser a = ReaderT Bin (StateT Int (Either String)) a

runParser :: Parser a -> Bin -> Either String a
runParser p bin = evalStateT (runReaderT p bin) 0

parseBits :: Int -> Parser Bin
parseBits n = do
    bin <- ask
    offset <- get
    if offset + n >= VU.length bin
        then throwError "not enough bits"
        else put (offset + n) >> pure (VU.take n $ VU.drop offset bin)

--------------------------------------------------------------------------------

data Packet a = Packet Int Int (Body a) deriving (Functor, Show)
data Body a
    = Literal Int
    | Sum     (NonEmpty a)
    | Product (NonEmpty a)
    | Minimum (NonEmpty a)
    | Maximum (NonEmpty a)
    | Greater a a
    | Less    a a
    | Equal   a a
    deriving (Functor, Foldable, Show)

parseLiteral :: Parser Int
parseLiteral = toInt . VU.concat <$> go
  where
    go = do
        g <- parseBits 5
        if not (VU.head g) then pure [VU.tail g] else (VU.tail g :) <$> go

parseHeader :: Parser (Int, Int)
parseHeader = (,) <$> (toInt <$> parseBits 3) <*> (toInt <$> parseBits 3)

parseUntil :: Int -> Parser a -> Parser [a]
parseUntil 0 _ = pure []
parseUntil n p = do
    offset0 <- get
    x <- p
    offset1 <- get
    let remainder = n - (offset1 - offset0)
    if remainder <= 0 then pure [x] else (x :) <$> parseUntil remainder p

parsePacket :: Parser (Fix Packet)
parsePacket = do
    (ver, typ) <- parseHeader
    fmap (Fix . Packet ver typ) $ if typ == 4
        then Literal <$> parseLiteral
        else do
            lengthType <- VU.head <$> parseBits 1
            case lengthType of
                False -> do
                    numBits <- toInt <$> parseBits 15
                    op typ =<< parseUntil numBits parsePacket
                True  -> do
                    len <- toInt <$> parseBits 11
                    op typ =<< replicateM len parsePacket
  where
    op 0 (x : xs) = pure $ Sum     (x :| xs)
    op 1 (x : xs) = pure $ Product (x :| xs)
    op 2 (x : xs) = pure $ Minimum (x :| xs)
    op 3 (x : xs) = pure $ Maximum (x :| xs)
    op 5 [x, y]   = pure $ Greater x y
    op 6 [x, y]   = pure $ Less    x y
    op 7 [x, y]   = pure $ Equal   x y
    op o xs       = throwError $ "unknown operator " ++ show (o, length xs)

--------------------------------------------------------------------------------

versionSum :: Fix Packet -> Int
versionSum = cata $ \(Packet ver _ vers) -> ver + sum vers

eval :: Fix Packet  -> Int
eval = cata $ \(Packet _ _ body) -> case body of
    Literal n   -> n
    Sum     xs  -> sum xs
    Product xs  -> product xs
    Minimum xs  -> minimum xs
    Maximum xs  -> maximum xs
    Greater x y -> if x >  y then 1 else 0
    Less    x y -> if x <  y then 1 else 0
    Equal   x y -> if x == y then 1 else 0

--------------------------------------------------------------------------------

main :: IO ()
main = pureMain $ \inputStr -> do
    packet <- runParser parsePacket $ fromHexString inputStr
    pure (pure (versionSum packet), pure (eval packet))
