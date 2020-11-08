

newtype State s a = State { runState :: (s -> (a,s)) }

instance Monad (State s) where
  return a        = State $ \s -> (a,s)
  (State x) >>= f = State $ \s ->
    let (v,s') = x s
    in runState (f v) s'

newtype StateT s m a = StateT { runStateT :: (s -> m (a,s)) }

instance (Monad m) => Monad (StateT s m) where
  return a         = StateT $ \s -> return (a,s)
  (StateT x) >>= f = StateT $ \s -> do
    (v,s') <- x s
    runStateT (f v) s' 


