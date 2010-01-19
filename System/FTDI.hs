module System.FTDI
    ( -- *Devices
      Device
    , ChipType(..)
    , getChipType
    , setChipType
    , fromUSBDevice
    , guessChipType

      -- *Interfaces
    , Interface(..)

      -- *Device handles
    , DeviceHandle
    , resetUSB
    , getTimeout
    , setTimeout
    , openDevice
    , closeDevice
    , withDeviceHandle

      -- *Interface handles
    , InterfaceHandle
    , getDeviceHandle
    , getInterface
    , openInterface
    , closeInterface
    , withInterfaceHandle

      -- *Data transfer
    , ChunkedReaderT
    , runChunkedReaderT
    , readData

      -- **Low level bulk transfers
      -- |These are low-level functions and as such they ignores things like:
      --
      --   * Max packet size
      --
      --   * Latency timer
      --
      --   * Modem status bytes
      --
      -- USB timeouts are not ignored, but they will prevent the request from
      -- being completed.
    , readBulk
    , writeBulk

      -- *Control requests
    , reset
    , purgeReadBuffer
    , purgeWriteBuffer
    , getLatencyTimer
    , setLatencyTimer
    , BitMode(..)
    , setBitMode

      -- **Line properties
    , Parity(..)
    , BitDataFormat(..)
    , StopBits(..)
    , setLineProperty

      -- **Modem status
    , ModemStatus(..)
    , pollModemStatus

      -- ** Low level
    , control
    , readControl
    , writeControl

      -- *Flow control
    , FlowCtrl(..)
    , setFlowControl
    , setDTR
    , setRTS
    , setEventCharacter
    , setErrorCharacter

      -- *Constants
    , minBaudRate
    , maxBaudRate

      -- *Defaults
    , defaultTimeout

      -- *Miscellaneous
    , calcBaudRateDivisors
    ) where

import System.FTDI.Internal
