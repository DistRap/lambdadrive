{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module ODrive.ADC where

import Ivory.Language
import Ivory.Stdlib
import Ivory.HW
import Ivory.Tower
import Ivory.BSP.STM32.Interrupt
import Ivory.BSP.STM32.Peripheral.ADC
import Ivory.BSP.STM32.Peripheral.GPIOF4

import ODrive.Platforms
import ODrive.Types
import ODrive.Utils
import ODrive.Ivory.Types.Adc
import ODrive.Ivory.Types.Dccal

-- handles concurrent ADC measurements and DRV8301 DC_CAL pin toggling
-- ADC1 vbus
-- ADC2 phase B
-- ADC3 phase C
adcMultiTower :: (ADC, ADC, ADC)
              -> GPIOPin
              -> IFloat
              -> Tower e (ChanOutput ('Struct "adc"), ChanOutput ('Struct "dccal"))
adcMultiTower (
    a1@ADC {adcPeriph=adcp1, adcChan=chan1, adcInjChan=ichan1, adcInt=int}
  , a2@ADC {adcPeriph=adcp2, adcChan=chan2, adcInjChan=ichan2, adcInt=_}
  , a3@ADC {adcPeriph=adcp3, adcChan=chan3, adcInjChan=ichan3, adcInt=_})
  drv8301_dc_cal meas_period = do

  odriveTowerDeps

  adc_chan <- channel -- ADC readings (adc struct)
  adc_dc_chan <- channel -- DC calibration measurements (dccal struct)

  isr <- signalUnsafe
            (Interrupt int)
            (Microseconds 250)
            (interrupt_disable int)

  monitor "adc_multi" $ do
    monitorModuleDef $ hw_moduledef

    adc_last_regular <- stateInit "adc_last_regular" (ival (0 :: Uint16))
    adc_last_injected <- stateInit "adc_last_injected" (ival (0 :: Uint16))

    -- isr source
    -- adc_src_none M1 dccal
    -- adc_src_t1cc4 M0 c
    -- adc_src_t1trgo M0 dccal
    -- adc_src_t8cc4 M1 c

    -- store raw values for debugging
    adc_vbus    <- stateInit "adc_vbus" (ival (0 :: IFloat))
    adc_phase_b <- stateInit "adc_phase_b" (ival (0 :: IFloat))
    adc_phase_c <- stateInit "adc_phase_c" (ival (0 :: IFloat))

    dccal_phase_b <- stateInit "dccal_phase_b" (ival (0 :: IFloat))
    dccal_phase_c <- stateInit "dccal_phase_c" (ival (0 :: IFloat))

    phase_b_done <- stateInit "phase_b_done" (ival false)
    phase_c_done <- stateInit "phase_c_done" (ival false)

    dccal_phase_b_done <- stateInit "dccal_phase_b_done" (ival false)
    dccal_phase_c_done <- stateInit "dccal_phase_c_done" (ival false)

    handler isr "adc_capture" $ do
      e <- emitter (fst adc_chan) 1
      edc <- emitter (fst adc_dc_chan) 1

      callback $ const $ do
        -- handle transaction safety manually, might be removed
        -- as it seems to hold that ADC2 isr comes before ADC3
        store phase_b_done false
        store phase_c_done false

        store dccal_phase_b_done false
        store dccal_phase_c_done false
        -- current and dc cal isrs should arrive in order ADC2 -> ADC3
        -- both phases are read at the same time
        --
        -- if source is CC4 we're measuring current at SVM vector 0
        -- if source is TRGO we're measuring DC_CAL at SVM vector 7
        --
        -- populate adc_meas struct with current values
        let mkMeas :: forall s eff . (GetAlloc eff ~ 'Scope s) => Ivory eff (ConstRef ('Stack s) ('Struct "adc"))
            mkMeas = do
              bus <- deref adc_vbus
              pb <- deref adc_phase_b
              pc <- deref adc_phase_c

              meas <- local $ istruct
                [ vbus .= ival bus
                , phase_b .= ival pb
                , phase_c .= ival pc ]

              return $ constRef meas

        let mkDCCalMeas :: forall s eff . (GetAlloc eff ~ 'Scope s) => Ivory eff (ConstRef ('Stack s) ('Struct "dccal"))
            mkDCCalMeas = do
              pb <- deref dccal_phase_b
              pc <- deref dccal_phase_c

              meas <- local $ istruct
                [ dccal_b .= ival pb
                , dccal_c .= ival pc ]

              return $ constRef meas

        let calibFilterTau = 0.2 :: IFloat
        let calibFilterK = meas_period / calibFilterTau

        -- handle adc reading from ADC, toggle between t1cc4 and t1trgo
        let handleADCInjected ADC{..} = do
              val <- deref adc_last_injected

              cr2 <- getReg $ adcRegCR2 adcPeriph
              let trgSrc = cr2 #. adc_cr2_jextsel
              let trgSrcT1CC4 = trgSrc ==? adc_jext_t1cc4
              let trgSrcT1TRGO = trgSrc ==? adc_jext_t1trgo
              let isDCCal = trgSrcT1TRGO
              let i = adcId

              assert (trgSrcT1CC4 .|| trgSrcT1TRGO)
              -- flip between t1cc4 and t1trgo for adc2 and 3, toggling
              -- between phase current measurements and DC calibration measurements
              when (i ==? 2 .|| i ==? 3) $ do
                cond_
                  [ trgSrcT1CC4  ==> do
                      -- next on this ADC is DC Cal
                      pinHigh drv8301_dc_cal
                      setJExt adcPeriph adc_jext_t1trgo

                  , trgSrcT1TRGO ==> do
                      -- next on this ADC is current
                      pinLow drv8301_dc_cal
                      setJExt adcPeriph adc_jext_t1cc4 ]

              cond_
                [ (i ==? 1) ==> do
                    store (adc_vbus) (vbusVoltageFromADC val)

                , (i ==? 2) ==> do
                    ifte_ isDCCal
                          (do
                              store dccal_phase_b_done true

                              lastdc <- deref dccal_phase_b
                              let delta = ((phaseCurrentFromADC val) - lastdc) * calibFilterK
                              store dccal_phase_b (lastdc + delta)
                              )
                          (do
                              store phase_b_done true
                              lastdc <- deref dccal_phase_b
                              store adc_phase_b ((phaseCurrentFromADC val) - lastdc)
                              )

                , (i ==? 3) ==> do
                    ifte_ isDCCal
                          (do store dccal_phase_c_done true

                              lastdc <- deref dccal_phase_c
                              let delta = ((phaseCurrentFromADC val) - lastdc) * calibFilterK
                              store dccal_phase_c (lastdc + delta)
                              )
                          (do
                              store phase_c_done true
                              lastdc <- deref dccal_phase_c
                              store adc_phase_c ((phaseCurrentFromADC val) - lastdc)
                              )
                ]

              -- only emit phase measurements after both were taken
              pbd <- deref phase_b_done
              pcd <- deref phase_c_done
              when (pbd .&& pcd) $ do
                pinPulse gpio3
                (mkMeas >>= emit e)

              dpbd <- deref dccal_phase_b_done
              dpcd <- deref dccal_phase_c_done
              when (dpbd .&& dpcd) $ do
                pinPulse gpio4
                assert isDCCal
                (mkDCCalMeas >>= emit edc)

        -- check which ADC fired
        let checkADC adc = do
              let ADCPeriph{..} = adcPeriph adc

              sr <- getReg adcRegSR

              comment "regular conversion"
              when (bitToBool (sr #. adc_sr_eoc)) $ do
                dr <- getReg adcRegDR

                store adc_last_regular (toRep (dr #. adc_dr_data))

                --XXX: we don't handle any regular conversions for now
                --handleADCRegular adc

              comment "injected conversion"
              when (bitToBool (sr #. adc_sr_jeoc)) $ do
                jdr1 <- getReg adcRegJDR1

                store adc_last_injected (toRep (jdr1 #. adc_jdr1_data))

                handleADCInjected adc

        let clearADC adc = do
              let ADCPeriph{..} = adcPeriph adc

              comment "clear interrupt flags in status register"
              modifyReg adcRegSR $ do
                clearBit adc_sr_strt
                clearBit adc_sr_eoc
                clearBit adc_sr_jstrt
                clearBit adc_sr_jeoc

        mapM_ checkADC [a1, a2, a3]
        mapM_ clearADC [a1, a2, a3]

        interrupt_enable int

    handler systemInit "init" $ do
      callback $ const $ do

        interrupt_set_priority int 5

        pinOut drv8301_dc_cal
        pinOut gpio3
        pinOut gpio4

        mapM_ adc_in_pin $ map snd [chan1, chan2, chan3, ichan1, ichan2, ichan3]

        adcInit adcp1 adc_12bit false
        adcInit adcp2 adc_12bit false
        adcInit adcp3 adc_12bit false

        let adcSetup ADCPeriph{..}
                     exten extsel regChan
                     jexten jextsel injChan = do

              comment "prescaler PCLK2 DIV2 for all adcs"
              modifyReg adcRegCCR $ setField adc_ccr_adcpre adc_pclk2_div2

              comment "enable interrupts"
              modifyReg adcRegCR1 $ do
                setBit adc_cr1_eocie
                setBit adc_cr1_jeocie

              modifyReg adcRegCR2 $ do
                setBit adc_cr2_eocs
                setField adc_cr2_exten exten
                setField adc_cr2_extsel extsel

                setField adc_cr2_jexten jexten
                setField adc_cr2_jextsel jextsel

              comment "regular conversion sequence, sqr3 rank 1"
              modifyReg adcRegSQR3 $ setField adc_sqr3_sq1 (fromRep regChan)

              comment "injected channel sequence, rank 1"
              modifyReg adcRegJSQR $ do
                --setField adc_jsq1 (fromRep 0)
                -- !!! only jsqr4 is read if jsqr_jl is 0, or not ???
                -- jsr and jdr correspondence? will better see this on another channel then 0
                setField adc_jsqr4 (fromRep injChan)

        -- quality jsqr obfuscation
        -- _CHANNELNB_ << (5U * ((_RANKNB_ + 3U) - (_JSQR_JL_)))
        adcSetup adcp1
          adc_exten_none adc_ext_t8trgo   (fst chan1)
          adc_exten_falling adc_jext_t1cc4 (fst ichan1)

        adcSetup adcp2
          adc_exten_none adc_ext_t8trgo (fst chan2)
          adc_exten_rising adc_jext_t1cc4 (fst ichan2)

        adcSetup adcp3
          adc_exten_rising adc_ext_t8trgo (fst chan3)
          adc_exten_rising adc_jext_t1cc4 (fst ichan3)

        interrupt_enable int

  -- return output side of measurement channels
  return (snd adc_chan, snd adc_dc_chan)

-- set pin to analog input mode
adc_in_pin :: GPIOPin -> Ivory eff ()
adc_in_pin p = do
  pinEnable  p
  pinSetMode p gpio_mode_analog

-- set external trigger source for ADCPeriph
setJExt :: ADCPeriph -> ADCJExtSel -> Ivory eff ()
setJExt ADCPeriph{..} jextsel = modifyReg adcRegCR2 $ setField adc_cr2_jextsel jextsel

-- convert ADC value to phase current
phaseCurrentFromADC :: Uint16 -> IFloat
phaseCurrentFromADC val = current $ shunt_volt $ amp_out_volt $ adcval_bal val
  where
    adcval_bal :: Uint16 -> IFloat
    adcval_bal x = safeCast $ ((safeCast :: Uint16 -> Sint32) x) - 2048

    amp_out_volt :: IFloat -> IFloat
    amp_out_volt x = 3.3 / 4096 * x

    -- XXX: drv_gain_40, static for now
    rev_gain :: IFloat
    rev_gain = 1/40

    shunt_conductance = 1/0.0005  -- [S]

    shunt_volt x = x * rev_gain

    current x = x * shunt_conductance

-- convert ADC value to VBUS voltage
vbusVoltageFromADC :: Uint16 -> IFloat
vbusVoltageFromADC val = (safeCast val) * 3.3 * 11 / 4096
