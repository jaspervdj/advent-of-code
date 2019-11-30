{-# LANGUAGE LambdaCase #-}
module Main where

import           Data.Char     (chr)
import           Numeric       (readHex)

parse :: String -> Either String String
parse = \case
    '"' : rest -> go rest
    _          -> Left "expected \" at start"
  where
    go = \case
        []                        -> Left "expected \" at end"
        "\""                      -> Right ""
        '\\' : '\\' : rest        -> (:) '\\' <$> go rest
        '\\' : '"' : rest         -> (:) '"' <$> go rest
        '\\' : 'x' : x : y : rest -> case readHex [x, y] of
            [(hex, "")] -> (:) (chr hex) <$> go rest
            _           -> Left $ "unexpected hex " ++ [x, y]
        x : rest                  -> (:) x <$> go rest

unparse :: String -> String
unparse = \str -> "\"" ++ go str ++ "\""
  where
    go = \case
        []          -> []
        '"'  : rest -> "\\\"" ++ go rest
        '\\' : rest -> "\\\\" ++ go rest
        x : rest    -> x : go rest

main :: IO ()
main = do
    strings <- lines <$> getContents
    print $
        sum (map length strings) -
        sum (map (either error length . parse) strings)
    print $
        sum (map (length . unparse) strings) -
        sum (map length strings)
