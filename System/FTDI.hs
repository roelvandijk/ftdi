{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

module System.FTDI
    ( ChipType(..)
    , Parity(..)
    , BitDataFormat(..)
    , StopBits(..)
    , BitMode(..)
    , ModemStatus(..)

      -- *Devices
    , Device
    , fromUSBDevice
    , guessChipType

      -- *Interfaces
    , Interface(..)

      -- *Device handles
    , DeviceHandle
    , openDevice
    , closeDevice
    , withDeviceHandle

      -- *Interface handles
    , InterfaceHandle
    , openInterface
    , closeInterface
    , withInterfaceHandle

      -- *Reading data
    , ChunkedReader
    , runChunkedReader
    , readData

      -- *Bulk transfers (low level)
    , readBulk
    , writeBulk

      -- *Control requests
    , reset
    , purgeReadBuffer
    , purgeWriteBuffer
    , getLatencyTimer
    , setLatencyTimer
    , pollModemStatus
    , setBitMode
    , setLineProperty

      -- *Flow control
    , FlowCtrl(..)
    , setFlowControl
    , setDTR
    , setRTS
    , setEventCharacter
    , setErrorCharacter

      -- *Constants
    , maxBaudRate
    , defaultTimeout
    ) where

import Prelude hiding ( drop )
import Prelude.Unicode

import Control.Exception              ( Exception, bracket, throwIO )
import Control.Monad                  ( liftM, liftM2 )
import Control.Monad.Trans            ( MonadTrans, MonadIO
                                      , liftIO
                                      )
import Control.Monad.Trans.State      ( StateT, get, put, runStateT )
import Data.Bits                      ( Bits, (.|.), (.&.)
                                      , complement, setBit
                                      , shiftL, testBit
                                      )
import Data.Data                      ( Data )
import Data.Function                  ( on )
import Data.List                      ( minimumBy )

import Data.Typeable                  ( Typeable )
import Data.Word                      ( Word8, Word16 )

import Safe                           ( atMay, headMay )

import Data.ByteString ( ByteString )
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Internal as BSI
import qualified System.USB               as USB


data ChipType = ChipType_AM
              | ChipType_BM
              | ChipType_2232C
              | ChipType_R
              | ChipType_2232H
              | ChipType_4232H
                deriving (Enum, Eq, Ord, Show, Data, Typeable)

data Parity =
            -- |The parity bit is set to one if the number of ones in a given
            -- set of bits is even (making the total number of ones, including
            -- the parity bit, odd).
              Parity_Odd
            -- |The parity bit is set to one if the number of ones in a given
            -- set of bits is odd (making the total number of ones, including
            -- the parity bit, even).
            | Parity_Even
            | Parity_Mark  -- ^The parity bit is always 1.
            | Parity_Space -- ^The parity bit is always 0.
              deriving (Enum, Eq, Ord, Show, Data, Typeable)

data BitDataFormat = Bits_7
                   | Bits_8

data StopBits = StopBit_1
              | StopBit_15
              | StopBit_2
                deriving (Enum)

data BitMode = BitMode_Reset
             | BitMode_BitBang
               -- |Multi-Protocol Synchronous Serial Engine
             | BitMode_MPSSE
               -- |Synchronous Bit-Bang Mode
             | BitMode_SyncBitBang
               -- |MCU Host Bus Emulation Mode
             | BitMode_MCU
               -- |Fast Opto-Isolated Serial Interface Mode
             | BitMode_Opto
             | BitMode_CBus
               -- |Single Channel Synchronous 245 FIFO Mode
             | BitMode_SyncFIFO
              deriving (Eq, Ord, Show, Data, Typeable)

data FlowCtrl = RTS_CTS -- Request-To-Send / Clear-To-Send
              | DTR_DSR -- Data-Terminal-Ready / Data-Set-Ready
              | XOnXOff -- Transmitter on / transmitter off

data Interface = Interface_A
               | Interface_B
               | Interface_C
               | Interface_D
                 deriving (Enum, Eq, Ord, Show, Data, Typeable)

type RequestCode  = Word8
type RequestValue = Word16

data ModemStatus = ModemStatus
    { msClearToSend                ∷ Bool
    , msDataSetReady               ∷ Bool
    , msRingIndicator              ∷ Bool
    , msReceiveLineSignalDetect    ∷ Bool
    , msDataReady                  ∷ Bool
    , msOverrunError               ∷ Bool
    , msParityError                ∷ Bool
    , msFramingError               ∷ Bool
    , msBreakInterrupt             ∷ Bool
    , msTransmitterHoldingRegister ∷ Bool
    , msTransmitterEmpty           ∷ Bool
    , msErrorInReceiverFIFO        ∷ Bool
    } deriving (Eq, Ord, Show, Data, Typeable)

data Device = Device
    { devUSB      ∷ USB.Device
    , devUSBDesc  ∷ USB.DeviceDesc
    , devUSBConf  ∷ USB.ConfigDesc
    , devChipType ∷ ChipType
    }

data DeviceHandle = DeviceHandle
    { devHndUSB     ∷ USB.DeviceHandle
    , devHndDev     ∷ Device
    , devHndTimeout ∷ Int
    }

data InterfaceHandle = InterfaceHandle
    { ifHndUSB       ∷ USB.InterfaceHandle
    , ifHndDevHnd    ∷ DeviceHandle
    , ifHndInterface ∷ Interface
    , ifHndInEPDesc  ∷ USB.EndpointDesc USB.In
    , ifHndOutEPDesc ∷ USB.EndpointDesc USB.Out
    }

data FTDIException = InterfaceInUse
                   | InterfaceNotFound
                     deriving (Show, Data, Typeable)

instance Exception FTDIException

-------------------------------------------------------------------------------
-- Constants
-------------------------------------------------------------------------------

maxBaudRate ∷ Num a ⇒ a
maxBaudRate = 3000000

reqReset           ∷ RequestCode
reqSetModemCtrl    ∷ RequestCode
reqSetFlowCtrl     ∷ RequestCode
reqSetBaudRate     ∷ RequestCode
reqSetData         ∷ RequestCode
reqPollModemStatus ∷ RequestCode
reqSetEventChar    ∷ RequestCode
reqSetErrorChar    ∷ RequestCode
reqSetLatencyTimer ∷ RequestCode
reqGetLatencyTimer ∷ RequestCode
reqSetBitMode      ∷ RequestCode
reqReadPins        ∷ RequestCode
reqReadEEPROM      ∷ RequestCode
reqWriteEEPROM     ∷ RequestCode
reqEraseEEPROM     ∷ RequestCode

reqReset           = 0x00
reqSetModemCtrl    = 0x01
reqSetFlowCtrl     = 0x02
reqSetBaudRate     = 0x03
reqSetData         = 0x04
reqPollModemStatus = 0x05
reqSetEventChar    = 0x06
reqSetErrorChar    = 0x07
reqSetLatencyTimer = 0x09
reqGetLatencyTimer = 0x0A
reqSetBitMode      = 0x0B
reqReadPins        = 0x0C
reqReadEEPROM      = 0x90
reqWriteEEPROM     = 0x91
reqEraseEEPROM     = 0x92

valResetSIO         ∷ RequestValue
valPurgeReadBuffer  ∷ RequestValue
valPurgeWriteBuffer ∷ RequestValue

valResetSIO         = 0
valPurgeReadBuffer  = 1
valPurgeWriteBuffer = 2

valSetDTRHigh ∷ RequestValue
valSetDTRLow  ∷ RequestValue
valSetRTSHigh ∷ RequestValue
valSetRTSLow  ∷ RequestValue

valSetDTRHigh = 0x0101
valSetDTRLow  = 0x0100
valSetRTSHigh = 0x0202
valSetRTSLow  = 0x0200

-------------------------------------------------------------------------------
-- Defaults
-------------------------------------------------------------------------------

defaultTimeout ∷ Int
defaultTimeout = 5000

-------------------------------------------------------------------------------

marshalBitMode ∷ BitMode → Word8
marshalBitMode bm = case bm of
                      BitMode_Reset       → 0x00
                      BitMode_BitBang     → 0x01
                      BitMode_MPSSE       → 0x02
                      BitMode_SyncBitBang → 0x04
                      BitMode_MCU         → 0x08
                      BitMode_Opto        → 0x10
                      BitMode_CBus        → 0x20
                      BitMode_SyncFIFO    → 0x40

marshalFlowControl ∷ FlowCtrl → Word16
marshalFlowControl f = case f of
                         RTS_CTS → 0x0100
                         DTR_DSR → 0x0200
                         XOnXOff → 0x0400

unmarshalModemStatus ∷ Word8 → Word8 → ModemStatus
unmarshalModemStatus a b =
    ModemStatus { msClearToSend                = testBit a 4
                , msDataSetReady               = testBit a 5
                , msRingIndicator              = testBit a 6
                , msReceiveLineSignalDetect    = testBit a 7
                , msDataReady                  = testBit b 0
                , msOverrunError               = testBit b 1
                , msParityError                = testBit b 2
                , msFramingError               = testBit b 3
                , msBreakInterrupt             = testBit b 4
                , msTransmitterHoldingRegister = testBit b 5
                , msTransmitterEmpty           = testBit b 6
                , msErrorInReceiverFIFO        = testBit b 7
                }

marshalModemStatus ∷ ModemStatus → (Word8, Word8)
marshalModemStatus ms = let a = setBitIf 4 (msClearToSend             ms)
                              $ setBitIf 5 (msDataSetReady            ms)
                              $ setBitIf 6 (msRingIndicator           ms)
                              $ setBitIf 7 (msReceiveLineSignalDetect ms)
                              $ 0
                            b = setBitIf 0 (msDataReady                  ms)
                              $ setBitIf 1 (msOverrunError               ms)
                              $ setBitIf 2 (msParityError                ms)
                              $ setBitIf 3 (msFramingError               ms)
                              $ setBitIf 4 (msBreakInterrupt             ms)
                              $ setBitIf 5 (msTransmitterHoldingRegister ms)
                              $ setBitIf 6 (msTransmitterEmpty           ms)
                              $ setBitIf 7 (msErrorInReceiverFIFO        ms)
                              $ 0
                        in (a, b)
    where
      setBitIf ∷ Int → Bool → Word8 → Word8
      setBitIf ix True  val = setBit val ix
      setBitIf _  False val = val

interfaceIndex ∷ Interface → Word16
interfaceIndex = (+ 1) ∘ genFromEnum

interfaceToUSB ∷ Interface → USB.InterfaceNumber
interfaceToUSB = genFromEnum

interfaceEndPointIn ∷ Interface → USB.EndpointAddress USB.In
interfaceEndPointIn i = USB.EndpointAddress $ 1 + 2 ⋅ genFromEnum i

interfaceEndPointOut ∷ Interface → USB.EndpointAddress USB.Out
interfaceEndPointOut i = USB.EndpointAddress $ 2 + 2 ⋅ genFromEnum i

supportedSubDivisors ∷ ChipType → [Int]
supportedSubDivisors ChipType_AM = [0, 1, 2, 4]
supportedSubDivisors _           = [0..7]

-------------------------------------------------------------------------------
-- Devices
-------------------------------------------------------------------------------

fromUSBDevice ∷ USB.Device → ChipType → IO Device
fromUSBDevice dev chip = do
  desc ← USB.getDeviceDesc dev
  conf ← USB.getConfigDesc dev 0
  return Device { devUSB      = dev
                , devUSBDesc  = desc
                , devUSBConf  = conf
                , devChipType = chip
                }

guessChipType ∷ USB.DeviceDesc → Maybe ChipType
guessChipType desc = case USB.deviceReleaseNumber desc of
                       -- Workaround for bug in BM type chips
                       (0,2,0,0) | USB.deviceSerialNumberStrIx desc ≡ 0
                                             → Just ChipType_BM
                                 | otherwise → Just ChipType_AM
                       (0,4,0,0) → Just ChipType_BM
                       (0,5,0,0) → Just ChipType_2232C
                       (0,6,0,0) → Just ChipType_R
                       (0,7,0,0) → Just ChipType_2232H
                       (0,8,0,0) → Just ChipType_4232H
                       _         → Nothing

-------------------------------------------------------------------------------
-- Device Handles
-------------------------------------------------------------------------------

openDevice ∷ Device → IO DeviceHandle
openDevice dev = do
  handle ← USB.openDevice $ devUSB dev
  USB.setConfig handle $ USB.configValue $ devUSBConf dev
  return DeviceHandle { devHndUSB       = handle
                      , devHndDev       = dev
                      , devHndTimeout   = defaultTimeout
                      }

closeDevice ∷ DeviceHandle → IO ()
closeDevice = USB.closeDevice ∘ devHndUSB

withDeviceHandle ∷ Device → (DeviceHandle → IO α) → IO α
withDeviceHandle dev = bracket (openDevice dev) closeDevice

-------------------------------------------------------------------------------
-- Interface Handles
-------------------------------------------------------------------------------

openInterface ∷ DeviceHandle → Interface → IO InterfaceHandle
openInterface devHnd i = do
  usbIfHnd ← USB.claimInterface (devHndUSB devHnd) (interfaceToUSB i)
  maybe (throwIO InterfaceNotFound)
        (\(inEP, outEP) →
             return InterfaceHandle
                        { ifHndUSB              = usbIfHnd
                        , ifHndDevHnd           = devHnd
                        , ifHndInterface        = i
                        , ifHndInEPDesc         = inEP
                        , ifHndOutEPDesc        = outEP
                        }
        )
        $ liftM2 (,)  mInEP mOutEP
    where conf = devUSBConf $ devHndDev devHnd
          ifIx = fromEnum i
          mIfDesc = headMay =<< USB.configInterfaces conf `atMay` ifIx
          mInEP   = headMay ∘ USB.interfaceInEndpoints  =<< mIfDesc
          mOutEP  = headMay ∘ USB.interfaceOutEndpoints =<< mIfDesc

closeInterface ∷ InterfaceHandle → IO ()
closeInterface ifHnd = let usbIfHnd = ifHndUSB ifHnd
                       in USB.releaseInterface usbIfHnd

withInterfaceHandle ∷ DeviceHandle → Interface → (InterfaceHandle → IO α) → IO α
withInterfaceHandle h c = bracket (openInterface h c) closeInterface

-------------------------------------------------------------------------------
-- Bulk transfers
-------------------------------------------------------------------------------

newtype ChunkedReader m α = ChunkedReader {unCR ∷ StateT ByteString m α}
    deriving (Monad, MonadTrans, MonadIO)

runChunkedReader ∷ ChunkedReader m α → ByteString → m (α, ByteString)
runChunkedReader = runStateT ∘ unCR

readData ∷ ∀ m. MonadIO m ⇒ InterfaceHandle → Int → ChunkedReader m [ByteString]
readData ifHnd numBytes = ChunkedReader $
    do prevRest ← get
       let readNumBytes = numBytes - BS.length prevRest
       if readNumBytes > 0
         then do chunks ← readLoop readNumBytes
                 return $ if BS.null prevRest
                          then chunks
                          else prevRest : chunks
         else let (bs, newRest) = BS.splitAt numBytes prevRest
              in put newRest >> return [bs]
    where
      -- Best case:  1 iteration
      -- Worst case: ∞ iterations (need some kind of custom timeout for this case)
      readLoop ∷ Int → StateT ByteString m [ByteString]
      readLoop remaining = do
        -- Amount of bytes we need to request in order to get atleast
        -- 'readNumBytes' bytes of data.
        let reqSize = packetSize ⋅ remaining `divRndUp` packetDataSize
        (bytes, _) ← liftIO $ readBulk ifHnd reqSize

        let receivedBytes     = BS.length bytes
            receivedDataBytes = receivedBytes - packetHeaderSize ⋅ (receivedBytes `divRndUp` packetSize)

        -- The reason for not actually getting the requested amount of bytes
        -- could be either a USB timeout or the FTDI latency timer firing.
        --
        -- In case of a USB timeout:
        --   ∀ (n ∷ Int). n ≥ 1 ∧ BS.length bytes ≡ n ⋅ packetSize
        --
        -- In case of FTDI latency timer:
        --   BS.length bytes < packetSize
        --   (Not entirely sure this is correct. Could also be limited to the
        --   FTDI buffer size)
        if receivedDataBytes < remaining
          then let xs = splitPackets packetSize bytes
               in liftM (xs ++) (readLoop $ remaining - receivedDataBytes)
          else -- We might have received too much data, since we can only
               -- request multiples of 'packetSize' bytes. Split the byte
               -- string at such an index that the first part contains the
               -- remaining data bytes. The rest is kept for future usage.
               let (bs, newRest) = BS.splitAt (splitIndex remaining) bytes
               in put newRest >> return (splitPackets packetSize bs)

      splitIndex n = p ⋅ packetSize + packetHeaderSize
                     + (n - p ⋅ packetDataSize)
          where p = n `div` packetDataSize

      packetDataSize   = packetSize - packetHeaderSize
      packetHeaderSize = 2
      packetSize       = USB.maxPacketSize
                       $ USB.endpointMaxPacketSize
                       $ ifHndInEPDesc ifHnd

readBulk ∷ InterfaceHandle → Int → IO (ByteString, Bool)
readBulk ifHnd numBytes =
    USB.readBulk (ifHndUSB ifHnd)
                 (interfaceEndPointIn $ ifHndInterface ifHnd)
                 (devHndTimeout $ ifHndDevHnd ifHnd)
                 numBytes

writeBulk ∷ InterfaceHandle → ByteString → IO (Int, Bool)
writeBulk ifHnd bs =
    USB.writeBulk (ifHndUSB ifHnd)
                  (interfaceEndPointOut $ ifHndInterface ifHnd)
                  (devHndTimeout $ ifHndDevHnd ifHnd)
                  bs

-------------------------------------------------------------------------------
-- Control Requests
-------------------------------------------------------------------------------

-- |The type of a USB control request.
type USBControl α = USB.DeviceHandle
                  → USB.RequestType
                  → USB.Recipient
                  → RequestCode
                  → RequestValue
                  → Word16
                  → USB.Timeout
                  → α

-- |Generic FTDI control request with explicit index
genControl ∷ USBControl α
           → Word16
           → InterfaceHandle
           → RequestCode
           → RequestValue
           → α
genControl usbCtrl index ifHnd request value =
    usbCtrl usbHnd
            USB.Vendor
            USB.ToDevice
            request
            value
            (index .|. (interfaceIndex $ ifHndInterface ifHnd))
            (devHndTimeout devHnd)
    where devHnd = ifHndDevHnd ifHnd
          usbHnd = devHndUSB devHnd

control ∷ InterfaceHandle → RequestCode → Word16 → IO ()
control = genControl USB.control 0

readControl ∷ InterfaceHandle → RequestCode → Word16 → USB.Size → IO (ByteString, Bool)
readControl = genControl USB.readControl 0

writeControl ∷ InterfaceHandle → RequestCode → Word16 → ByteString → IO (USB.Size, Bool)
writeControl = genControl USB.writeControl 0

-------------------------------------------------------------------------------

reset ∷ InterfaceHandle → IO ()
reset ifHnd = control ifHnd reqReset valResetSIO

purgeReadBuffer ∷ InterfaceHandle → IO ()
purgeReadBuffer ifHnd = control ifHnd reqReset valPurgeReadBuffer

purgeWriteBuffer ∷ InterfaceHandle → IO ()
purgeWriteBuffer ifHnd = control ifHnd reqReset valPurgeWriteBuffer

-------------------------------------------------------------------------------

getLatencyTimer ∷ InterfaceHandle → IO Word8
getLatencyTimer ifHnd = do
    (bs, _) ← readControl ifHnd reqGetLatencyTimer 0 1
    case BS.unpack bs of
      [b] → return b
      _   → error "System.FTDI.getLatencyTimer: failed"

setLatencyTimer ∷ InterfaceHandle → Word8 → IO ()
setLatencyTimer ifHnd latency = control ifHnd reqSetLatencyTimer $ fromIntegral latency

pollModemStatus ∷ InterfaceHandle → IO ModemStatus
pollModemStatus ifHnd = do
    (bs, _) ← readControl ifHnd reqPollModemStatus 0 2
    case BS.unpack bs of
      [x,y] → return $ unmarshalModemStatus x y
      _     → error "System.FTDI.pollModemStatus: failed"

setBitMode ∷ InterfaceHandle → Word8 → BitMode → IO ()
setBitMode ifHnd bitMask bitMode = control ifHnd reqSetBitMode value
    where bitMask' = fromIntegral bitMask
          bitMode' = fromIntegral $ marshalBitMode bitMode
          value    = bitMask' .|. shiftL bitMode' 8

-- TODO: finish implementation (encoding divisor and subdivisor in index and
-- value fields of control request)
-- TODO: baudrate is also a function of the bitmode (bitbang needs baudrate ⋅ 4)
{-
setConfiguration ∷ RealFrac α ⇒ InterfaceHandle → Word8 → BitMode → α → IO ()
setConfiguration ifHnd bitMask bitMode baudRate = do
  setBitMode ifHnd bitMask bitMode
  genControl  USB.control
              (error "TODO")
              ifHnd
              reqSetBaudRate
              (error "TODO")
    where (d, s, e) = calcBaudRateDivisors ( supportedSubDivisors
                                           $ devChipType
                                           $ devHndDev
                                           $ ifHndDevHnd ifHnd
                                           ) baudRate
-}

setLineProperty ∷ InterfaceHandle
                → BitDataFormat
                → StopBits
                → Maybe Parity
                → Bool
                → IO ()
setLineProperty ifHnd bitDataFormat stopBits parity break' =
    control ifHnd
            reqSetData
            $ orBits [ case bitDataFormat of
                         Bits_7 → 7
                         Bits_8 → 8
                     , maybe 0 (\p → (1 + genFromEnum p) `shiftL` 8) parity
                     , genFromEnum stopBits `shiftL` 11
                     , genFromEnum break'   `shiftL` 14
                     ]

-------------------------------------------------------------------------------
-- Flow control
-------------------------------------------------------------------------------

setFlowControl ∷ InterfaceHandle → Maybe FlowCtrl → IO ()
setFlowControl ifHnd mFC = genControl USB.control
                                      (maybe 0 marshalFlowControl mFC)
                                      ifHnd
                                      reqSetFlowCtrl
                                      0

setDTR ∷ InterfaceHandle → Bool → IO ()
setDTR ifHnd b = control ifHnd reqSetModemCtrl
               $ if b then valSetDTRHigh else valSetDTRLow

setRTS ∷ InterfaceHandle → Bool → IO ()
setRTS ifHnd b = control ifHnd reqSetModemCtrl
               $ if b then valSetRTSHigh else valSetRTSLow

genSetCharacter ∷ RequestCode → InterfaceHandle → Maybe Word8 → IO ()
genSetCharacter req ifHnd mEC =
    control ifHnd req $ maybe 0 (\c → setBit (fromIntegral c) 8) mEC

setEventCharacter ∷ InterfaceHandle → Maybe Word8 → IO ()
setEventCharacter = genSetCharacter reqSetEventChar

setErrorCharacter ∷ InterfaceHandle → Maybe Word8 → IO ()
setErrorCharacter = genSetCharacter reqSetErrorChar

-------------------------------------------------------------------------------
-- Miscellaneous
-------------------------------------------------------------------------------

-- |Finds the divisors the most closely represent the requested baud rate.
calcBaudRateDivisors ∷ ∀ α. RealFrac α ⇒ [Int] → α → (Int, Int, α)
calcBaudRateDivisors _  3000000  = (0, 0, 0)
calcBaudRateDivisors _  2000000  = (1, 0, 0)
calcBaudRateDivisors ss baudRate =
    minimumBy (compare `on` (\(_,_,x) → x))
              [ (d, s, (abs $ baudRate - b') ÷ baudRate)
              | s ← ss
              , let s' = fromIntegral s ÷ 8
                    -- The calculated divisor is clamped to an acceptable
                    -- range.
                    d  = clamp 2 (2 ^ (14 ∷ Int) - 1)
                               $ divisor baudRate s'
                    -- Baud rate calculated from found divisors.
                    b' = calcBaudRate d s'
              ]
    where
      -- |Calculates the divisor from a baud rate and a subdivisor.
      divisor ∷ Integral β ⇒ α → α → β
      divisor br s = floor $ (maxBaudRate - br ⋅ s) ÷ br

      -- |Calculates the baud rate from a divisor and a subdivisor.
      calcBaudRate ∷ Int → α → α
      calcBaudRate d s = maxBaudRate ÷ (realToFrac d + s)

-------------------------------------------------------------------------------
-- Utility functions
-------------------------------------------------------------------------------

genFromEnum ∷ (Enum e, Num n) ⇒ e → n
genFromEnum = fromIntegral ∘ fromEnum

orBits ∷ Bits α ⇒ [α] → α
orBits = foldr (.|.) 0

andBits ∷ Bits α ⇒ [α] → α
andBits = foldr (.&.) $ complement 0

prop_clamp ∷ Ord α ⇒ α → α → α → Bool
prop_clamp lo hi x = lo ≤ x ∧ x ≤ hi

clamp ∷ Ord α ⇒ α → α → α → α
clamp lo hi = atLeast lo ∘ atMost hi

atLeast ∷ Ord α ⇒ α → α → α
atLeast = max

atMost ∷ Ord α ⇒ α → α → α
atMost = min

splitChunk ∷ Int → [ByteString] → ([ByteString], [ByteString])
splitChunk = go []
    where
      go ∷ [ByteString] -> Int → [ByteString] → ([ByteString], [ByteString])
      go acc _ [] = (reverse acc, [])
      go acc n (x:xs)
          | lx ≡ n    = (reverse $ x:acc, xs)
          | lx > n    = let (a, b) = BS.splitAt n x
                        in (reverse $ a:acc, b:xs)
          | otherwise = go (x:acc) (n - lx) xs
          where lx = BS.length x

prop_divRndUp_min ∷ Integral α ⇒ α → α → Bool
prop_divRndUp_min x y = let d = divRndUp x y
                        in d ⋅ y ≥ x

prop_divRndUp_max ∷ Integral α ⇒ α → α → Bool
prop_divRndUp_max x y = let d = divRndUp x y
                        in x `div` y ≤ d

divRndUp ∷ Integral α ⇒ α → α → α
divRndUp x y = let r | mod x y ≡ 0 = 0
                     | otherwise   = 1
               in div x y + r

-- |Split a stream of bytes into packets. The first 2 bytes of each
-- packet are the modem status bytes and are dropped.
splitPackets ∷ Int → ByteString → [ByteString]
splitPackets n = go
    where
      go xs | BS.null xs = []
            | otherwise  = case BS.splitAt n xs of
                             (a, b) → BS.drop 2 a : go b
