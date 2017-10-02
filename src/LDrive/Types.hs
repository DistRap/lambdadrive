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

module LDrive.Types
  ( fconstrain
  , ldriveTypes
  , ldriveTowerDeps
  , uartTestTypes
  , UARTBuffer
  , PWMInput
  , PWMOutput
  ) where

import Ivory.Language
import Ivory.HW
import Ivory.Tower
import Ivory.Serialize

import LDrive.Control.PID

import CANOpen.Ivory.Types as COTypes
import LDrive.Ivory.Types as LDTypes

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

ldriveTypes :: Module
ldriveTypes = package "ldrive_types" $ do
  incl fconstrain
  incl pidUpdate
  hw_moduledef

  defStringType (Proxy :: Proxy UARTBuffer)

  defStruct (Proxy :: Proxy "PID")

  depend serializeModule
  mapM_ depend LDTypes.typeModules
  mapM_ depend COTypes.typeModules


ldriveTowerDeps :: Tower e ()
ldriveTowerDeps = do
  towerDepends ldriveTypes
  towerModule ldriveTypes

  mapM_ towerDepends LDTypes.typeModules
  mapM_ towerModule LDTypes.typeModules

  mapM_ towerDepends COTypes.typeModules
  mapM_ towerModule COTypes.typeModules

  towerDepends serializeModule
  towerModule  serializeModule
  mapM_ towerArtifact serializeArtifacts
