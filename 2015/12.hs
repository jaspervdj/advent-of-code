{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified AdventOfCode.NanoParser as NP
import           Control.Applicative     (many, optional, (<|>))
import           Control.Monad           (guard)
import           Data.Char               (isDigit)
import           Data.Functor            (($>))
import           Data.Maybe              (catMaybes, mapMaybe)
import           Data.Maybe              (fromMaybe)

newtype Fix f = Fix {unFix :: f (Fix f)}

cata :: Functor f => (f a -> a) -> Fix f -> a
cata f = f . fmap (cata f) . unFix

data Json a
    = Int Int
    | String String
    | Array [a]
    | Object [(String, a)]
    deriving (Eq, Functor, Show)

parseJson :: NP.Parser Char (Fix Json)
parseJson = fmap Fix $
    (String <$> string) <|>
    (Int <$> int) <|>
    (Array <$> array parseJson) <|>
    (Object <$> object parseJson)
  where
    string   = NP.char '"' *> many notQuote <* NP.char '"'
    int      = sign <*> (read <$> NP.many1 digit)
    array  p = NP.char '[' *> NP.sepBy p (NP.char ',') <* NP.char ']'
    object p = NP.char '{' *> NP.sepBy (item p) (NP.char ',') <* NP.char '}'
    item   p = (,) <$> string <* NP.char ':' <*> p
    notQuote = NP.satisfy "non-quote" (/= '"')
    sign     = fromMaybe id <$> optional (NP.char '-' $> negate)
    digit    = NP.satisfy "digit" isDigit

jsonSum :: Fix Json -> Int
jsonSum = cata $ \case
    Int    n   -> n
    String _   -> 0
    Array  arr -> sum arr
    Object obj -> sum (map snd obj)

removeRed :: Fix Json -> Maybe (Fix Json)
removeRed = cata $ fmap Fix . \case
    Int n      -> Just $ Int n
    String s   -> Just $ String s
    Array  arr -> Just $ Array (catMaybes arr)
    Object obj ->
        let obj' = mapMaybe (\(k, v) -> (,) k <$> v) obj in
        guard (not $ any (isRed . snd) obj') $> Object obj'
  where
    isRed (Fix (String "red")) = True
    isRed _                    = False

main :: IO ()
main = do
    errOrJson <- NP.runParser parseJson <$> getContents
    either fail (print . jsonSum) errOrJson
    either fail (print . maybe 0 jsonSum . removeRed) errOrJson
