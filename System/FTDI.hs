{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

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
    , maxBaudRate

      -- *Defaults
    , defaultTimeout

      -- *Miscellaneous
    , calcBaudRateDivisors
    ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- base
import Control.Applicative       ( Applicative, (<$>), Alternative )
import Control.Exception         ( Exception, bracket, throwIO )
import Control.Monad             ( Functor
                                 , Monad, (>>=), (>>), (=<<), return, fail
                                 , liftM, liftM2
                                 , MonadPlus
                                 )
import Control.Monad.Fix         ( MonadFix )
import Data.Bool                 ( Bool, otherwise )
import Data.Bits                 ( Bits, (.|.), (.&.)
                                 , complement, setBit
                                 , shiftL, testBit
                                 )
import Data.Data                 ( Data )
import Data.Eq                   ( Eq, (==) )
import Data.Function             ( ($), on )
import Data.Int                  ( Int )
import Data.List                 ( foldr, head, minimumBy, partition, zip )
import Data.Maybe                ( Maybe(Just, Nothing), maybe )
import Data.Ord                  ( Ord, (<), (>), compare, min, max )
import Data.Tuple                ( fst, snd )
import Data.Typeable             ( Typeable )
import Data.Word                 ( Word8, Word16 )
import Prelude                   ( Enum, succ
                                 , Num, (+), (-), signum, Integral, quotRem, (^)
                                 , RealFrac, Fractional
                                 , fromEnum, fromInteger, fromIntegral
                                 , abs, realToFrac, floor, ceiling
                                 , div, mod, divMod, error
                                 )
import System.IO                 ( IO )
import Text.Show                 ( Show )

-- base-unicode-symbols
import Data.Bool.Unicode         ( (∧) )
import Data.Eq.Unicode           ( (≡), (≢) )
import Data.Function.Unicode     ( (∘) )
import Data.Monoid.Unicode       ( (⊕) )
import Data.Ord.Unicode          ( (≤), (≥) )
import Prelude.Unicode           ( (⋅), (÷) )

-- bytestring
import qualified Data.ByteString as BS
import Data.ByteString           ( ByteString )

-- ftdi
import System.FTDI.Util          ( divRndUp, clamp, genFromEnum, orBits )

-- safe
import Safe                      ( atMay, headMay )

-- transformers
import Control.Monad.Trans       ( MonadTrans, MonadIO, liftIO )
import Control.Monad.Trans.State ( StateT, get, put, runStateT )

-- usb
import qualified System.USB as USB


-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------


type RequestCode  = Word8
type RequestValue = Word16

data FTDIException = InterfaceNotFound deriving (Show, Data, Typeable)

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
-- Devices
-------------------------------------------------------------------------------

data Device = Device
    { devUSB      ∷ USB.Device
    , devUSBConf  ∷ USB.ConfigDesc
    , devChipType ∷ ChipType
    }

data ChipType = ChipType_AM
              | ChipType_BM
              | ChipType_2232C
              | ChipType_R
              | ChipType_2232H
              | ChipType_4232H
                deriving (Enum, Eq, Ord, Show, Data, Typeable)

supportedSubDivisors ∷ ChipType → [Int]
supportedSubDivisors ChipType_AM = [0, 1, 2, 4]
supportedSubDivisors _           = [0..7]

getChipType ∷ Device → ChipType
getChipType = devChipType

setChipType ∷ Device → ChipType → Device
setChipType dev ct = dev {devChipType = ct}

fromUSBDevice ∷ USB.Device → ChipType → Device
fromUSBDevice dev chip =
  Device { devUSB      = dev
         , devUSBConf  = head ∘ USB.deviceConfigs $ USB.deviceDesc dev
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
-- Interfaces
-------------------------------------------------------------------------------

data Interface = Interface_A
               | Interface_B
               | Interface_C
               | Interface_D
                 deriving (Enum, Eq, Ord, Show, Data, Typeable)

interfaceIndex ∷ Interface → Word16
interfaceIndex = succ ∘ genFromEnum

interfaceToUSB ∷ Interface → USB.InterfaceNumber
interfaceToUSB = genFromEnum

interfaceEndPointIn ∷ Interface → USB.EndpointAddress
interfaceEndPointIn i =
    USB.EndpointAddress { USB.endpointNumber    = 1 + 2 ⋅ genFromEnum i
                        , USB.transferDirection = USB.In
                        }

interfaceEndPointOut ∷ Interface → USB.EndpointAddress
interfaceEndPointOut i =
    USB.EndpointAddress { USB.endpointNumber    = 2 + 2 ⋅ genFromEnum i
                        , USB.transferDirection = USB.Out
                        }

-------------------------------------------------------------------------------
-- Device Handles
-------------------------------------------------------------------------------

data DeviceHandle = DeviceHandle
    { devHndUSB     ∷ USB.DeviceHandle
    , devHndDev     ∷ Device
    , devHndTimeout ∷ Int
    }

resetUSB ∷ DeviceHandle → IO ()
resetUSB = USB.resetDevice ∘ devHndUSB

getTimeout ∷ DeviceHandle → Int
getTimeout = devHndTimeout

setTimeout ∷ DeviceHandle → Int → DeviceHandle
setTimeout devHnd timeout = devHnd {devHndTimeout = timeout}

openDevice ∷ Device → IO DeviceHandle
openDevice dev = do
  handle ← USB.openDevice $ devUSB dev
  USB.setConfig handle $ USB.configValue $ devUSBConf dev
  return DeviceHandle { devHndUSB     = handle
                      , devHndDev     = dev
                      , devHndTimeout = defaultTimeout
                      }

closeDevice ∷ DeviceHandle → IO ()
closeDevice = USB.closeDevice ∘ devHndUSB

withDeviceHandle ∷ Device → (DeviceHandle → IO α) → IO α
withDeviceHandle dev = bracket (openDevice dev) closeDevice

-------------------------------------------------------------------------------
-- Interface Handles
-------------------------------------------------------------------------------

data InterfaceHandle = InterfaceHandle
    { ifHndDevHnd    ∷ DeviceHandle
    , ifHndInterface ∷ Interface
    , ifHndInEPDesc  ∷ USB.EndpointDesc
    , ifHndOutEPDesc ∷ USB.EndpointDesc
    }

getDeviceHandle ∷ InterfaceHandle → DeviceHandle
getDeviceHandle = ifHndDevHnd

getInterface ∷ InterfaceHandle → Interface
getInterface = ifHndInterface

openInterface ∷ DeviceHandle → Interface → IO InterfaceHandle
openInterface devHnd i =
    let conf    = devUSBConf $ devHndDev devHnd
        ifIx    = fromEnum i
        mIfDesc = headMay =<< USB.configInterfaces conf `atMay` ifIx
        mInOutEps = partition ((USB.In ≡) ∘ USB.transferDirection ∘ USB.endpointAddress)
                    ∘ USB.interfaceEndpoints
                    <$> mIfDesc
        mInEp   = headMay ∘ fst =<< mInOutEps
        mOutEp  = headMay ∘ snd =<< mInOutEps
    in maybe (throwIO InterfaceNotFound)
             ( \ifHnd → do USB.claimInterface (devHndUSB devHnd) (interfaceToUSB i)
                           return ifHnd
             )
             $ do inEp  ← mInEp
                  outEp ← mOutEp
                  return InterfaceHandle
                     { ifHndDevHnd    = devHnd
                     , ifHndInterface = i
                     , ifHndInEPDesc  = inEp
                     , ifHndOutEPDesc = outEp
                     }

closeInterface ∷ InterfaceHandle → IO ()
closeInterface ifHnd =
    USB.releaseInterface (devHndUSB $ ifHndDevHnd ifHnd)
                         (interfaceToUSB $ ifHndInterface ifHnd)

withInterfaceHandle ∷ DeviceHandle → Interface → (InterfaceHandle → IO α) → IO α
withInterfaceHandle h i = bracket (openInterface h i) closeInterface

-------------------------------------------------------------------------------
-- Data transfer
-------------------------------------------------------------------------------

-- |
newtype ChunkedReaderT m α = ChunkedReaderT {unCR ∷ StateT ByteString m α}
    deriving ( Functor
             , Applicative
             , Alternative
             , Monad
             , MonadPlus
             , MonadTrans
             , MonadIO
             , MonadFix
             )

{-| Run the ChunkedReaderT given an initial state.

The initial state represents excess bytes carried over from a previous
ChunkedReaderT. When invoking runChunkedReaderT for the first time you
can safely pass the 'BS.empty' bytestring as the initial state.

A contrived example showing how you can manually thread the excess
bytes through subsequent invocations of runChunkedReaderT:

@
  example &#x2237; 'InterfaceHandle' &#x2192; IO ()
  example ifHnd = do
    (packets1, rest1) &#x2190; runChunkedReaderT ('readData' ifHnd 400) 'BS.empty'
    print $ 'BS.concat' packets1
    (packets2, rest2) &#x2190; runChunkedReaderT ('readData' ifHnd 200) rest1
    print $ 'BS.concat' packets2
@

However, it is much easier to let 'ChunkedReaderT's monad instance
handle the plumbing:

@
  example &#x2237; 'InterfaceHandle' &#x2192; IO ()
  example ifHnd =
    let reader = do packets1 &#x2190; 'readData' ifHnd 400
                    liftIO $ print $ 'BS.concat' packets1
                    packets2 &#x2190; 'readData' ifHnd 200
                    liftIO $ print $ 'BS.concat' packets1
    in runChunkedReaderT reader 'BS.empty'
@

-}
runChunkedReaderT ∷ ChunkedReaderT m α → ByteString → m (α, ByteString)
runChunkedReaderT = runStateT ∘ unCR

{-| Reads data from the given FTDI interface by performing bulk reads.

This function produces an action in the ChunkedReaderT monad that,
when executed, will read exactly the requested number of
bytes. Executing the readData action will block until all data is
read. The result value is a list of chunks, represented as
ByteStrings. This representation was choosen for efficiency reasons.

Data is read in packets. The function may choose to request more than
needed in order to get the highest possible bandwidth. The excess of
bytes is kept as the state of the ChunkedReaderT monad. A subsequent
invocation of readData will first return bytes from the stored state
before requesting more from the device itself. A consequence of this
behaviour is that even when you request 100 bytes the function will
actually request 512 bytes (depending on the packet size) and /block/
until all 512 bytes are read! There is no workaround since requesting
less bytes than the packet size is an error.

USB timeouts will not interrupt readData. In case of a timeout
readData will simply resume reading data. A small USB timeout can
degrade performance.

The FTDI latency timer can cause poor performance. If the FTDI chip
can't fill a packet before the latency timer fires it is forced to
send an incomplete packet. This will cause a stream of tiny packets
instead of a few large packets. Performance will suffer horribly, but
the request will still be completed.

If you need to make a lot of small requests then a small latency can
actually improve performance.

Modem status bytes are filtered from the result. Every packet send by
the FTDI chip contains 2 modem status bytes. They are not part of the
data and do not count for the number of bytes read. They will not
appear in the result.

Example:

@
  -- Read 100 data bytes from ifHnd
  (packets, rest) &#x2190; 'runChunkedReaderT' ('readData' ifHnd 100) 'BS.empty'
@

-}
-- TODO: timeout
readData ∷ ∀ m. MonadIO m ⇒ InterfaceHandle → Int → ChunkedReaderT m [ByteString]
readData ifHnd numBytes = ChunkedReaderT $
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
      readLoop ∷ Int → StateT ByteString m [ByteString]
      readLoop readNumBytes = do
        -- Amount of bytes we need to request in order to get atleast
        -- 'readNumBytes' bytes of data.
        let reqSize    = packetSize ⋅ reqPackets
            reqPackets = readNumBytes `divRndUp` packetDataSize
        -- Timeout is ignored; the number of bytes that was read
        -- contains enough information.
        (bytes, _) ← liftIO $ readBulk ifHnd reqSize

        let receivedDataBytes   = receivedBytes - receivedHeaderBytes
            receivedBytes       = BS.length bytes
            receivedHeaderBytes = packetHeaderSize ⋅ receivedPackets
            receivedPackets     = receivedBytes `divRndUp` packetSize

        -- The reason for not actually getting the requested amount of bytes
        -- could be either a USB timeout or the FTDI latency timer firing.
        --
        -- In case of a USB timeout:
        --   ∃ (n : Nat). receivedBytes ≡ n ⋅ packetSize
        --
        -- In case of FTDI latency timer:
        --   receivedBytes < packetSize
        if receivedDataBytes < readNumBytes
          then let xs = splitPackets packetSize bytes
               in liftM (xs ⊕) (readLoop $ readNumBytes - receivedDataBytes)
          else -- We might have received too much data, since we can only
               -- request multiples of 'packetSize' bytes. Split the byte
               -- string at such an index that the first part contains
               -- readNumBytes of data bytes. The rest is kept for future usage.
               let (bs, newRest) = BS.splitAt (splitIndex readNumBytes) bytes
               in put newRest >> return (splitPackets packetSize bs)

      splitIndex n = p ⋅ packetSize + packetHeaderSize
                     + (n - p ⋅ packetDataSize)
          where p = n `div` packetDataSize

      packetDataSize   = packetSize - packetHeaderSize
      packetHeaderSize = 2
      packetSize       = USB.maxPacketSize
                         $ USB.endpointMaxPacketSize
                         $ ifHndInEPDesc ifHnd

-- |Perform a bulk read.
--
-- Returns the data that was read (in the form of a 'ByteString') and
-- a flag which indicates whether a timeout occured during the
-- request.
readBulk ∷ InterfaceHandle
         → Int -- ^Number of bytes to read
         → IO (ByteString, Bool)
readBulk ifHnd numBytes =
    USB.readBulk (devHndUSB $ ifHndDevHnd ifHnd)
                 (interfaceEndPointIn $ ifHndInterface ifHnd)
                 (devHndTimeout $ ifHndDevHnd ifHnd)
                 numBytes

-- |Perform a bulk write.
--
-- Returns the number of bytes that where written and a flag which
-- indicates whether a timeout occured during the request.
writeBulk ∷ InterfaceHandle
          → ByteString -- ^Data to be written
          → IO (Int, Bool)
writeBulk ifHnd bs =
    USB.writeBulk (devHndUSB $ ifHndDevHnd ifHnd)
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
           → Word16          -- ^Index
           → InterfaceHandle
           → RequestCode
           → RequestValue
           → α               -- ^Value
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

-- |Reset the FTDI device.
reset ∷ InterfaceHandle → IO ()
reset ifHnd = control ifHnd reqReset valResetSIO

-- |Clear the on-chip read buffer.
purgeReadBuffer ∷ InterfaceHandle → IO ()
purgeReadBuffer ifHnd = control ifHnd reqReset valPurgeReadBuffer

-- |Clear the on-chip write buffer.
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
setLatencyTimer ifHnd latency = control ifHnd reqSetLatencyTimer
                                        $ fromIntegral latency

-- |MPSSE bitbang modes
data BitMode = -- |Switch off bitbang mode, back to regular
               -- serial/FIFO.
               BitMode_Reset
               -- |Classical asynchronous bitbang mode, introduced
               -- with B-type chips.
             | BitMode_BitBang
               -- |Multi-Protocol Synchronous Serial Engine, available
               -- on 2232x chips.
             | BitMode_MPSSE
               -- |Synchronous Bit-Bang Mode, available on 2232x and
               -- R-type chips.
             | BitMode_SyncBitBang
               -- |MCU Host Bus Emulation Mode, available on 2232x
               -- chips. CPU-style fifo mode gets set via EEPROM.
             | BitMode_MCU
               -- |Fast Opto-Isolated Serial Interface Mode, available
               -- on 2232x chips.
             | BitMode_Opto
               -- |Bit-Bang on CBus pins of R-type chips, configure in
               -- EEPROM before use.
             | BitMode_CBus
               -- |Single Channel Synchronous FIFO Mode, available on
               -- 2232H chips.
             | BitMode_SyncFIFO
              deriving (Eq, Ord, Show, Data, Typeable)

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

data Parity = -- |The parity bit is set to one if the number of ones in a given
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

-- |Set RS232 line characteristics
setLineProperty ∷ InterfaceHandle
                → BitDataFormat -- ^Number of bits
                → StopBits      -- ^Number of stop bits
                → Maybe Parity  -- ^Optional parity mode
                → Bool          -- ^Break
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
-- Modem status
-------------------------------------------------------------------------------

-- |Modem status information. The modem status is send as a header for
-- each read access. In the absence of data the FTDI chip will
-- generate the status every 40 ms.
--
-- The modem status can be explicitely requested with the
-- 'pollModemStatus' function.
data ModemStatus = ModemStatus
    { -- |Clear to send (CTS)
      msClearToSend ∷ Bool
      -- |Data set ready (DTS)
    , msDataSetReady ∷ Bool
      -- |Ring indicator (RI)
    , msRingIndicator ∷ Bool
      -- |Receive line signal detect (RLSD)
    , msReceiveLineSignalDetect ∷ Bool
      -- | Data ready (DR)
    , msDataReady ∷ Bool
      -- |Overrun error (OE)
    , msOverrunError ∷ Bool
      -- |Parity error (PE)
    , msParityError ∷ Bool
      -- |Framing error (FE)
    , msFramingError ∷ Bool
      -- |Break interrupt (BI)
    , msBreakInterrupt ∷ Bool
      -- |Transmitter holding register (THRE)
    , msTransmitterHoldingRegister ∷ Bool
      -- |Transmitter empty (TEMT)
    , msTransmitterEmpty ∷ Bool
      -- |Error in RCVR FIFO
    , msErrorInReceiverFIFO ∷ Bool
    } deriving (Eq, Ord, Show, Data, Typeable)

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
marshalModemStatus ms = (a, b)
    where
      a = mkByte $ zip [4..]
                       [ msClearToSend
                       , msDataSetReady
                       , msRingIndicator
                       , msReceiveLineSignalDetect
                       ]
      b = mkByte $ zip [0..]
                       [ msDataReady
                       , msOverrunError
                       , msParityError
                       , msFramingError
                       , msBreakInterrupt
                       , msTransmitterHoldingRegister
                       , msTransmitterEmpty
                       , msErrorInReceiverFIFO
                       ]

      mkByte ∷ [(Int, ModemStatus → Bool)] → Word8
      mkByte ts = foldr (\(n, f) x → if f ms then setBit x n else x)
                        0
                        ts

pollModemStatus ∷ InterfaceHandle → IO ModemStatus
pollModemStatus ifHnd = do
    (bs, _) ← readControl ifHnd reqPollModemStatus 0 2
    case BS.unpack bs of
      [x,y] → return $ unmarshalModemStatus x y
      _     → error "System.FTDI.pollModemStatus: failed"


-------------------------------------------------------------------------------
-- Flow control
-------------------------------------------------------------------------------

data FlowCtrl = RTS_CTS -- ^Request-To-Send \/ Clear-To-Send
              | DTR_DSR -- ^Data-Terminal-Ready \/ Data-Set-Ready
              | XOnXOff -- ^Transmitter on \/ Transmitter off

marshalFlowControl ∷ FlowCtrl → Word16
marshalFlowControl f = case f of
                         RTS_CTS → 0x0100
                         DTR_DSR → 0x0200
                         XOnXOff → 0x0400

-- |Set the flow control for the FTDI chip. Use 'Nothing' to disable flow
-- control.
setFlowControl ∷ InterfaceHandle → Maybe FlowCtrl → IO ()
setFlowControl ifHnd mFC = genControl USB.control
                                      (maybe 0 marshalFlowControl mFC)
                                      ifHnd
                                      reqSetFlowCtrl
                                      0

-- |Set DTR line.
setDTR ∷ InterfaceHandle → Bool → IO ()
setDTR ifHnd b = control ifHnd reqSetModemCtrl
               $ if b then valSetDTRHigh else valSetDTRLow

-- |Set RTS line.
setRTS ∷ InterfaceHandle → Bool → IO ()
setRTS ifHnd b = control ifHnd reqSetModemCtrl
               $ if b then valSetRTSHigh else valSetRTSLow

genSetCharacter ∷ RequestCode → InterfaceHandle → Maybe Word8 → IO ()
genSetCharacter req ifHnd mEC =
    control ifHnd req $ maybe 0 (\c → setBit (fromIntegral c) 8) mEC

-- |Set the special event character. Use 'Nothing' to disable the event
-- character.
setEventCharacter ∷ InterfaceHandle → Maybe Word8 → IO ()
setEventCharacter = genSetCharacter reqSetEventChar

-- |Set the error character.  Use 'Nothing' to disable the error character.
setErrorCharacter ∷ InterfaceHandle → Maybe Word8 → IO ()
setErrorCharacter = genSetCharacter reqSetErrorChar

-------------------------------------------------------------------------------
-- Miscellaneous
-------------------------------------------------------------------------------

-- |Finds the divisors that most closely represent the requested baud rate.
--
-- The subdivisors are divided by 8.
calcBaudRateDivisors ∷ ∀ α. RealFrac α
                     ⇒ [Int]         -- ^Acceptable subdivisors
                     → α             -- ^Baud rate
                     → (Int, Int, α) -- ^(divisor, subdivisor, error)
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

-- |Split a stream of bytes into packets. The first 2 bytes of each packet are
-- the modem status bytes and are dropped.
splitPackets ∷ Int → ByteString → [ByteString]
splitPackets n = go
    where
      go xs | BS.null xs = []
            | otherwise  = case BS.splitAt n xs of
                             (a, b) → BS.drop 2 a : go b
