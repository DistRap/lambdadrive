{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ODrive.Control.SVM where

import Ivory.Language
import Ivory.Stdlib

import ODrive.Ivory.Types.Svm

-- space vector modulation
-- generates phase modulations from alpha and beta
svm :: forall s eff . (GetAlloc eff ~ 'Scope s)
    => IFloat
    -> IFloat
    -> Ivory eff (ConstRef ('Stack s) ('Struct "svm"))
svm alpha beta = do
  let obs3 = 1 / sqrt 3
  let tbs3 = 2 / sqrt 3
  let beta1sq3 = (1 / sqrt 3) * beta

  out <- local $ istruct []

  let storesvm aval bval cval = do
        store (out ~> a) aval
        store (out ~> b) bval
        store (out ~> c) cval

  ifte_ (beta >=? 0)
    (ifte_ (alpha >=? 0)
      (ifte_ (beta1sq3 >? alpha)
        (store (out ~> sextant) 2)
        (store (out ~> sextant) 1)
      )
      (ifte_ (-beta1sq3 >? alpha)
        (store (out ~> sextant) 3)
        (store (out ~> sextant) 2)
      )
    )
    (ifte_ (alpha >=? 0)
      (ifte_ (-beta1sq3 >? alpha)
        (store (out ~> sextant) 5)
        (store (out ~> sextant) 6)
      )
      (ifte_ (beta1sq3 >? alpha)
        (store (out ~> sextant) 4)
        (store (out ~> sextant) 5)
      )
    )

  sx <- deref (out ~> sextant)

  cond_ [
    sx ==? 1 ==> do
        -- sextant v1-v2
        let t1 = alpha - obs3 * beta
        let t2 = tbs3 * beta

        let tA = (1 - t1 - t2) * 0.5
        let tB = tA + t1
        let tC = tB + t2

        storesvm tA tB tC

    , sx ==? 2 ==> do
        -- sextant v2-v3
        let t2 =  alpha + obs3 * beta
        let t3 = -alpha + obs3 * beta

        let tB = (1 - t2 - t3) * 0.5
        let tA = tB + t3
        let tC = tA + t2

        storesvm tA tB tC

    , sx ==? 3 ==> do
        -- sextant v3-v4
        let t3 = tbs3 * beta
        let t4 = -alpha - obs3 * beta

        let tB = (1 - t3 - t4) * 0.5
        let tC = tB + t3
        let tA = tC + t4

        storesvm tA tB tC

    , sx ==? 4 ==> do
        -- sextant v4-v5
        let t4 = -alpha + obs3 * beta
        let t5 = -tbs3 * beta

        let tC = (1 - t4 - t5) * 0.5
        let tB = tC + t5
        let tA = tB + t4

        storesvm tA tB tC

    , sx ==? 5 ==> do
        -- sextant v5-v6
        let t5 = -alpha - obs3 * beta
        let t6 = alpha - obs3 * beta

        let tC = (1 - t5 - t6) * 0.5
        let tA = tC + t5
        let tB = tA + t6

        storesvm tA tB tC

    , sx ==? 6 ==> do
        -- sextant v6-v1
        let t6 = -tbs3 * beta
        let t1 = alpha + obs3 * beta

        let tA = (1 - t6 - t1) * 0.5
        let tC = tA + t1
        let tB = tC + t6

        storesvm tA tB tC
    ]


  return $ constRef out
