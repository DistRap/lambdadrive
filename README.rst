odrive firmware
===============

Experimental version of `ODrive Firmware <https://github.com/madcowswe/ODriveFirmware>`_

Written in http://ivorylang.org/

Requires
--------

- stack https://docs.haskellstack.org/en/stable/README/
- arm-none-eabi-newlib
- arm-none-eabi-gcc

Fedora::

  dnf install arm-none-eabi-gcc-cs arm-none-eabi-newlib


Building
--------

To build all images::

  ./standalone-setup.sh # required if you don't have checked out ivory tower and ivory-tower-stm32 repos in ..
  make

Tests
-----

ADC
  Run PWM on TIM1, also triggering ADC1 with TIM1_CCR4. ADC1 reads out
  pinA0 every 500 msecs and also with ADC interrupt.
Encoder
  Application for testing encoder capture mode with general purpose timer.

  Stores counts in `counter` variable, direction in `dir`, these can be inspected
  in gdb with `p counter` and `p dir`.
PWM
  Test application outputing center aligned 6-channel PWM with
  deadtime enabled using advanced timer TIM1.
SPI
  Application for testing DRV8301 communications.

  Talks to two DRV8301s sharing SPI bus, outputs debug info on UART1.
  Polls status register every 500ms after successful initialization.
CANSendRecv
  Test application sending packets from CAN1, blinks on received packets.
CAN2UART
  Test application for receiving and sending
  CAN packets controlled by UART
Blink
  Blinks external LEDs on GPIO1 and GPIO2
Spin
  Open loop locked motor spin test


Run `make` to build all test applications.
Specific application can be built with `make APP`
loaded with `make APP-load` and `make APP-run`.

To load PWM test application run::

        make pwm-load

to also issue run and start application after loading use::

        make pwm-run

to just run gdb with new binary without loading::

        make pwm-gdb
        # issuing 'load' in gdb         == pwm-load
        # running both 'load' and 'run' == pwm-run


Flashing
--------

With BlackMagic Probe::

  arm-none-eabi-gdb --ex 'target extended-remote /dev/ttyACM0' --ex 'monitor swdp_scan' --ex 'attach 1' --ex 'load' build/can2uart-test/image
