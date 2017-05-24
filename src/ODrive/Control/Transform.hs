{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module ODrive.Control.Transform where

import Ivory.Language

clarke :: IFloat -> IFloat -> (IFloat, IFloat)
clarke pb pc = (alpha, beta)
  where
    alpha = -pb - pc
    beta  = (pb - pc) / (sqrt 3)

park :: IFloat -> IFloat -> IFloat -> (IFloat, IFloat)
park alpha beta theta = (d, q)
  where
    d = c*alpha + s*beta
    q = c*beta  - s*alpha
    c = cos theta
    s = sin theta

ipark :: IFloat -> IFloat -> IFloat -> (IFloat, IFloat)
ipark d q theta = (alpha, beta)
  where
    alpha = c*d - s*q
    beta  = c*q + s*d
    -- xXX: we can re-use this from parke to save some cycles
    c = cos theta
    s = sin theta
