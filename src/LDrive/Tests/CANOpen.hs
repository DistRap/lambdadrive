{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module LDrive.Tests.CANOpen where

import Ivory.Language
import Ivory.Tower
import Ivory.Tower.HAL.Bus.CAN
import Ivory.Tower.HAL.Bus.Interface
import Ivory.HW.Module

import Ivory.BSP.STM32.ClockConfig
import Ivory.BSP.STM32.Driver.CAN
import Ivory.BSP.STM32.Peripheral.CAN.Filter

import LDrive.Platforms
import LDrive.LED
import LDrive.Types

import CANOpen.Ivory.Types
import CANOpen.Tower
import CANOpen.Tower.Types
import CANOpen.Tower.Interface.Cia402.Dict

app :: (e -> ClockConfig)
    -> (e -> TestCAN)
    -> (e -> ColoredLEDs)
    -> Tower e ()
app tocc totestcan toleds = do
  can  <- fmap totestcan getEnv
  leds <- fmap toleds    getEnv

  ldriveTowerDeps

  (res, req, _, _) <- canTower tocc (testCAN can) 1000000 (testCANRX can) (testCANTX can)

  attrs@Cia402Attrs{..} <- towerCia402Attrs initCia402Attrs
  od@ObjDict{..} <- objDictTower attrs
  canopenTower res req (canOpenLEDs leds) od

  periodic <- period (Milliseconds 250)

  monitor "simplecontroller" $ do
    handler systemInit "init" $ do
      callback $ const $ do
        let emptyID = CANFilterID32 (fromRep 0) (fromRep 0) False False
        canFilterInit (testCANFilters can)
                      [CANFilterBank CANFIFO0 CANFilterMask $ CANFilter32 emptyID emptyID]
                      []
        ledSetup $ greenLED leds
        ledSetup $ redLED leds
        ledOn    $ redLED leds


  where canOpenLEDs leds =
          CANOpenLEDs
            { leds_init = ledSetup (greenLED leds) >> ledSetup (redLED leds)
            , leds_module = hw_moduledef
            , leds_err_on = ledOn $ redLED leds
            , leds_err_off = ledOff $ redLED leds
            , leds_run_on = ledOn $ greenLED leds
            , leds_run_off = ledOff $ greenLED leds
            }
