module AdventOfCode.NanoTest
    ( (@?=)
    ) where

(@?=) :: (Eq a, Show a) => a -> a -> IO ()
actual @?= expected
    | actual == expected = pure ()
    | otherwise          = fail $
        "AdventOfCode.NanoTest:  Expected " ++
        show expected ++ ", got " ++ show actual
