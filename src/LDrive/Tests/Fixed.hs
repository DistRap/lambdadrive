
--import LDrive.Fixed
    fx <- stateInit "fx" (ival (0 :: Fixed 16))
    fx0 <- stateInit "fx0" (ival (0 :: Fixed 16))
    fx1 <- stateInit "fx1" (ival (1 :: Fixed 16))
    fx2 <- stateInit "fx2" (ival (2 :: Fixed 16))

    fxa <- stateInit "fxa" (ival (1 :: Fixed 16))
    fxs <- stateInit "fxs" (ival (2 :: Fixed 16))
    fxm <- stateInit "fxm" (ival (3 :: Fixed 16))
    fxm1 <- stateInit "fxm1" (ival (3 :: Fixed 16))
    fxm2 <- stateInit "fxm2" (ival (3 :: Fixed 16))
    fxma <- stateInit "fxma" (ival (3 :: Fixed 16))
    fxmb <- stateInit "fxmb" (ival (3 :: Fixed 16))
    fxmsum <- stateInit "fxmsum" (ival (3 :: Fixed 16))
    fxd <- stateInit "fxd" (ival (4 :: Fixed 16))

    fxn <- stateInit "fxn" (ival (-4 :: Fixed 16))

    --fxover <- stateInit "fxover" (ival (32768 :: Fixed 16))
    --
    fxabs <- stateInit "fxabs" (ival (-1337 :: Fixed 16))

    -- ??? how come 2^16-1 is not caught when used as ival for Fixed?
    fxwat <- stateInit "fxwat" (ival (2^16-1 :: Fixed 16))


--- blabla
         let px = (3.14159 :: IDouble)
        let n = (8 :: Uint8)
        let m = (24 :: IDouble)
        store fx $ (123 :: Fixed 16)
        dfx <- deref fx
        assert ((fixedM' dfx) ==? 16)
        let a = dfx
        store fx0 a
        store fxa (a + 123)
        store fxs (a - 23)
        store fxm (a * 23)
        store fxm1 (a * 555)
        store fxm2 (a * 1000)
        store fxd (fixedDiv a 10)

        store fxma 1
        store fxmb (1 * 123456)

        store fxn (-1337)
        leet <- deref fxn
        store fxabs (abs leet)
        --store fx1 $ toFixed (3.137 :: IDouble)
--        store fxp (safeCast $ (13.37 :: IDouble))
--        store fxp2 (castWith 0 $ 2 ** m * px)
--        store fxt $ tof (1.133731337 :: IDouble)
--        store fxt24 $ tof (1.133731337 :: IDouble)
--        x <- deref fxp2
--        let q = x + 20 --  <- deref fxp2
--        store flt ((safeCast x) / (2 ** m) )
 
