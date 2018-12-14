module Control.Monad.Extended
    ( module Control.Monad
    , ifM
    ) where

import           Control.Monad

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM mp mx my = mp >>= \p -> if p then mx else my
