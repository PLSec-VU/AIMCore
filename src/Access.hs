{-# LANGUAGE ConstraintKinds #-}

module Access (Access, extract) where

import Control.Comonad (Comonad (extract))

type Access f = (Applicative f, Comonad f)
