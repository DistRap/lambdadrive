{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module ODrive.Control.Transform where

import Ivory.Language
import Ivory.Stdlib

import ODrive.Types

clarke :: IFloat -> IFloat -> (IFloat, IFloat)
clarke pb pc = (alpha, beta)
  where
    alpha = -pb - pc
    beta  = (1.0 / sqrt 3) * (pb - pc)

parke :: IFloat -> IFloat -> IFloat -> (IFloat, IFloat)
parke alpha beta theta = (d, q)
  where
    d = c*alpha + s*beta
    q = c*beta  + s*alpha
    c = cos theta
    s = sin theta

iparke :: IFloat -> IFloat -> IFloat -> (IFloat, IFloat)
iparke d q theta = (alpha, beta)
  where
    alpha = c*d - s*q
    beta  = c*q + s*d
    -- xXX: we can re-use this from parke to save some cycles
    c = cos theta
    s = sin theta
