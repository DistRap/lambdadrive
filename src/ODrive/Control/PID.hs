{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ODrive.Control.PID where

import Ivory.Language

[ivory|
struct PID
  { pid_mv  :: Stored IFloat
  ; pid_i   :: Stored IFloat
  ; pid_err :: Stored IFloat
  }
|]

type SP   = IFloat -- Set point
type PV   = IFloat -- Process (measured) value
type Time = IFloat

pidUpdate :: Def ('[ Ref s ('Struct "PID")
                   , IFloat
                   , IFloat
                   , IFloat
                   , SP
                   , PV
                   , Time ]
                  ':-> IFloat)
pidUpdate = proc "pid_update" $
  \ pid kp ki kd sp pv dt -> body $ do
    err     <- assign (sp - pv)
    i       <- deref $ pid ~> pid_i
    i'      <- assign  $ ki * (i + err*dt)
    prevErr <- deref $ pid ~> pid_err
    d       <- assign  $ (err - prevErr) / dt
    store (pid ~> pid_i)   i'
    store (pid ~> pid_mv)  (kp*err + i' + kd*d)
    store (pid ~> pid_err) err
    ret err
