{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ODrive.Types
  ( fconstrain
  , odriveTypes
  , odriveTowerDeps
  , uartTestTypes
  , UARTBuffer
  , PWMInput
  , PWMOutput
  ) where

import Ivory.Language
import Ivory.HW
import Ivory.Tower
import Ivory.Serialize

import ODrive.Ivory.Types

[ivory| string struct UARTBuffer 128 |]

-- XXX: this is supported by the backend correctly generating
-- fmodf for .% on IFloats, suggest to upstream
instance IvoryIntegral IFloat

uartTestTypes :: Module
uartTestTypes = package "uartTestTypes" $ do
  defStringType (Proxy :: Proxy UARTBuffer)

type PWMArray = 'Array 3 ('Stored Uint16)
type PWMInput  = ChanInput PWMArray
type PWMOutput = ChanOutput PWMArray

-- from SMACCMPilot.Flight.Control.PID
-- | Constrain a floating point value to the range [xmin..xmax].
fconstrain :: Def ('[IFloat, IFloat, IFloat] ':-> IFloat)
fconstrain = proc "fconstrain" $ \xmin xmax x -> body $
  (ifte_ (x <? xmin)
    (ret xmin)
    (ifte_ (x >? xmax)
      (ret xmax)
      (ret x)))

odriveTypes :: Module
odriveTypes = package "odrive_types" $ do
  incl fconstrain
  hw_moduledef

  defStringType (Proxy :: Proxy UARTBuffer)

  depend serializeModule
  mapM_ depend typeModules


odriveTowerDeps :: Tower e ()
odriveTowerDeps = do
  towerDepends odriveTypes
  towerModule odriveTypes

  mapM_ towerDepends typeModules
  mapM_ towerModule typeModules

  towerDepends serializeModule
  towerModule  serializeModule
  mapM_ towerArtifact serializeArtifacts
