odrive firmware
===============

Experimental version of ODrive Firmware

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

CANSendRcv
  test application sending packets from CAN1, blinks on received packets.
  Image available in build/cansendrcv-test/image
CAN2UART
  test application for receiving and sending
  CAN packets controlled by UART
Blink
  Blinks external LEDs on GPIO1 and GPIO2


Flashing
--------

With BlackMagic Probe::

  arm-none-eabi-gdb --ex 'target extended-remote /dev/ttyACM0' --ex 'monitor swdp_scan' --ex 'attach 1' --ex 'load' build/can2uart-test/image
