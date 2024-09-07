module Control.Monad.Logic.Do
  ( (>>=)
  , (>>)
  ) where

import qualified Prelude as P
import Control.Applicative (empty)
import Data.Maybe (maybe)
import Control.Monad.Logic.Class

(>>=) :: MonadLogic m => m a -> (a -> m b) -> m b
(>>=) = (>>-)

(>>) :: MonadLogic m => m () -> m a -> m a
m >> k = msplit m P.>>= maybe empty (\(_, m') -> interleave k (m' >> k))
