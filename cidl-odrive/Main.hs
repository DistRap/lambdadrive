import Cidl

main = runCidl [ odriveIface ]

odriveIface = withTypes ts $ depend cia402Dict $ dict "odrive" $ do
  at 0x7777 $ field "extended" uint32
  where
    ts =
      [ adc
      , dccal
      , calEnc
      , encoder
      , adcEncSample
      , calR
      , calI
      , calError
      , calibration
      , svm
      , cc
      , heartbeat
      , drvFault
      ]

adc = record "adc"
  [ field "vbus" float
  , field "phase_b" float
  , field "phase_c" float
  ]

dccal = record "dccal"
  [ field "dccal_b" float
  , field "dccal_c" float
  ]

encoder = record "encoder"
  [ field "count" sint32
  , field "dir" bool
  , field "phase" float
  , field "pll_pos" float
  , field" pll_vel" float
  ]

adcEncSample = record "adc_enc_sample"
  [ field "adc_sample" adc
  , field "enc_sample" encoder
  ]

calR = record "cal_r"
  [ field "resistance" float
  , field "maxVoltage" float
  , field "integrator" float
  , field "seconds" float
  ]

calI = record "cal_i"
  [ field "inductance" float
  , field "testVoltageLow" float
  , field "testVoltageHigh" float
  , field "alphasLow" float
  , field "alphasHigh" float
  , field "cycles" uint16
  ]

calError = enum "cal_error"
  [ ens 0 "ok"
  , ens 1 "noEncoderResponse"
  ]

calEnc = record "cal_enc"
  [ field "offset" float
  , field "direction" sint8
  , field "scanRange" float
  , field "steps" uint16
  ]

calibration = record "calibration"
  [ field "testCurrent" float
  , field "voltageMag" float
  , field "calR" calR
  , field "calI" calI
  , field "calEnc" calEnc
  , field "calErr" calError
  ]

cc = record "current_control"
  [ field "alpha" float
  , field "beta" float
  , field "d" float
  , field "q" float
  , field "d_in" float
  , field "q_in" float
  , field "d_err" float
  , field "q_err" float
  , field "mod_d" float
  , field "mod_q" float
  , field "v_d" float
  , field "v_q" float
  , field "p_gain" float
  , field "i_gain" float
  , field "vfact" float
  , field "mod_scale" float
  , field "current_i_d" float
  , field "current_i_q" float
  , field "scaled" bool
  , field "mod_alpha" float
  , field "mod_beta" float
  , field "ibus" float
  ]

svm = record "svm"
  [ field "svm_a" float
  , field "svm_b" float
  , field "svm_c" float
  , field "sextant" float
  ]

drvFault = record "drv_fault"
  [ field "fault" bool
  , field "gvdd_undervoltage" bool
  , field "gvdd_overvoltage" bool
  , field "pvdd_undervoltage" bool
  , field "overtemp_shutdown" bool
  , field "overtemp_warning" bool
  , field "fet_hi_a_overcurr" bool
  , field "fet_lo_a_overcurr" bool
  , field "fet_hi_b_overcurr" bool
  , field "fet_lo_b_overcurr" bool
  , field "fet_hi_c_overcurr" bool
  , field "fet_lo_c_overcurr" bool
  ]

heartbeat = record "heartbeat"
  [ field "adc_enc" adcEncSample
  , field "dccal" dccal
  , field "svm" svm
  ]
