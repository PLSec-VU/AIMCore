{-# LANGUAGE UndecidableInstances #-}

module Util
  ( CircuitSim (..),
    run1,
    watch,
    cmpIO,
    pageIO,
    try,
    ifM,
  )
where

import Clash.Prelude hiding (Log, Ordering (..), Word, def, init, lift, log)
import Control.Monad
import Control.Monad.Trans.Maybe
import Prelude hiding (Ordering (..), Word, init, iterate, log, map, not, repeat, replicate, take, undefined, (!!), (&&), (++), (||))

data CircuitSim m i s o = CircuitSim
  { circuitInput :: i,
    circuitState :: s,
    circuitStep :: i -> s -> m (s, o),
    circuitNext :: o -> m (Maybe i)
  }

run1 :: (Monad m) => CircuitSim m i s o -> m (s, o, Maybe i)
run1 (CircuitSim i s step next) = do
  (s', o) <- step i s
  mi' <- next o
  pure (s', o, mi')

watch :: (Monad m) => CircuitSim m i s o -> m [(s, o, Maybe i)]
watch c = do
  (s', o, mi') <- run1 c
  case mi' of
    Nothing -> pure [(s', o, mi')]
    Just i' -> do
      rest <- watch $ c {circuitInput = i', circuitState = s'}
      pure $ (s', o, mi') : rest

cmpIO :: (Show a, Show b) => [(a, b)] -> IO ()
cmpIO = mapM_ $ \(a, b) -> do
  print a
  putStrLn ""
  putStrLn "*************************"
  putStrLn ""
  print b
  putStrLn ""
  putStrLn "------------------------"
  putStrLn "Press Enter to continue."
  putStrLn "------------------------"
  void getLine

pageIO :: (Show a) => [a] -> IO ()
pageIO = mapM_ $ \a -> do
  print a
  putStrLn ""
  putStrLn "------------------------"
  putStrLn "Press Enter to continue."
  putStrLn "------------------------"
  void getLine

try :: (Monad m) => MaybeT m () -> m ()
try m = runMaybeT m >>= maybe (pure ()) pure

ifM :: (Monad m) => m Bool -> m a -> m a -> m a
ifM mb mt mf = do
  b <- mb
  if b then mt else mf
