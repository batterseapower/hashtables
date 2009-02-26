module Control.Monad.Ref (
        MonadRef(..)
    ) where

import Control.Monad.ST

import Data.IORef
import Data.STRef


class Monad m => MonadRef m where
    type Ref m :: * -> *
    
    -- | Create a new reference
    newRef    :: a -> m (Ref m a)
    
    -- | Read a reference
    readRef   :: Ref m a -> m a
    
    -- | Write a reference
    writeRef  :: Ref m a -> a -> m ()
    
    -- | Modify a reference
    modifyRef :: Ref m a -> (a -> a) -> m ()
    modifyRef ref f = readRef ref >>= \x -> writeRef ref (f x)
    
    -- | Modify a reference strictly, evaluating the function application immediately
    modifyRef' :: Ref m a -> (a -> a) -> m ()
    modifyRef' ref f = readRef ref >>= \x -> let x' = f x in x' `seq` writeRef ref x'

instance MonadRef IO where
    type Ref IO = IORef
    
    newRef    = newIORef
    readRef   = readIORef
    writeRef  = writeIORef
    modifyRef = modifyIORef

instance MonadRef (ST s) where
    type Ref (ST s) = STRef s
    
    newRef    = newSTRef
    readRef   = readSTRef
    writeRef  = writeSTRef
    modifyRef = modifySTRef