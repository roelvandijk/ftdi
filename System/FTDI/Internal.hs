{-# LANGUAGE CPP
           , DeriveDataTypeable
           , FlexibleContexts
           , GeneralizedNewtypeDeriving
           , NoImplicitPrelude
           , PatternGuards
           , ScopedTypeVariables
           , UnicodeSyntax
  #-}

module System.FTDI.Internal where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Control.Applicative       ( Applicative, (<$>), Alternative )
import Control.Exception         ( Exception, bracket, throwIO )
import Control.Monad             ( Functor
                                 , Monad, (>>=), (>>), (=<<), return, fail
                                 , liftM
                                 , MonadPlus
                                 )
import Control.Monad.Fix         ( MonadFix )
import Data.Bool                 ( Bool, otherwise )
#ifdef __HADDOCK__
import Data.Bool                 ( Bool(False, True) )
#endif
import Data.Bits                 ( Bits, (.|.)
                                 , setBit, shiftL, shiftR, testBit
                                 )
import Data.Data                 ( Data )
import Data.Eq                   ( Eq, (==) )
import Data.Function             ( ($), on )
import Data.Int                  ( Int )
import Data.List                 ( foldr, head, minimumBy, partition, zip )
import Data.Maybe                ( Maybe(Just, Nothing), maybe )
import Data.Ord                  ( Ord, (<), (>), compare )
import Data.Tuple                ( fst, snd )
import Data.Typeable             ( Typeable )
import Data.Word                 ( Word8, Word16 )
import Prelude                   ( Enum, succ
                                 , Bounded, minBound, maxBound
                                 , Num, (+), (-), Integral, (^)
                                 , Fractional, Real, RealFrac
                                 , Double, Integer
                                 , fromEnum, fromInteger, fromIntegral
                                 , realToFrac, floor, ceiling
                                 , div, error
                                 )
import System.IO                 ( IO )
import Text.Read                 ( Read )
import Text.Show                 ( Show )

-- from base-unicode-symbols:
import Data.Eq.Unicode           ( (≡) )
import Data.Function.Unicode     ( (∘) )
import Data.Monoid.Unicode       ( (⊕) )
import Prelude.Unicode           ( (⋅), (÷) )

-- from bytestring:
import qualified Data.ByteString as BS ( empty, drop, length
                                       , null, splitAt, unpack
                                       )
import Data.ByteString           ( ByteString )

-- from ftdi:
import System.FTDI.Utils         ( divRndUp, clamp, genFromEnum, orBits )

-- from safe:
import Safe                      ( atMay, headMay )

-- from transformers:
import Control.Monad.Trans.State ( StateT, get, put, runStateT )
import Control.Monad.Trans.Class ( MonadTrans, lift )
import Control.Monad.IO.Class    ( MonadIO, liftIO )

-- from usb:
import qualified System.USB as USB


-------------------------------------------------------------------------------
-- Exceptions
-------------------------------------------------------------------------------

data FTDIException = InterfaceNotFound deriving (Show, Data, Typeable)

instance Exception FTDIException


-------------------------------------------------------------------------------
-- Request codes and values
-------------------------------------------------------------------------------

type RequestCode  = Word8
type RequestValue = Word16

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

-- |Default USB timeout. The timeout can be set per device handle with
-- the 'setTimeout' function.
defaultTimeout ∷ Int
defaultTimeout = 5000


-------------------------------------------------------------------------------
-- Devices
-------------------------------------------------------------------------------

-- |A representation of an FTDI device.
data Device = Device
    { devUSB      ∷ USB.Device
    , devUSBConf  ∷ USB.ConfigDesc
    , devChipType ∷ ChipType
    }

-- |The type of FTDI chip in a 'Device'. The capabilities of a device
-- depend on its chip type.
data ChipType = ChipType_AM
              | ChipType_BM
              | ChipType_2232C
              | ChipType_R
              | ChipType_2232H
              | ChipType_4232H
                deriving (Enum, Eq, Ord, Show, Data, Typeable)

getChipType ∷ Device → ChipType
getChipType = devChipType

setChipType ∷ Device → ChipType → Device
setChipType dev ct = dev {devChipType = ct}

-- |Promote a USB device to an FTDI device. You are responsible for
-- supplying the correct USB device and specifying the correct chip
-- type. There is no failsafe way to automatically determine whether a
-- random USB device is an actual FTDI device.
fromUSBDevice ∷ USB.Device -- ^ USB device
              → ChipType
              → Device     -- ^ FTDI device
fromUSBDevice dev chip =
  Device { devUSB      = dev
         , devUSBConf  = head ∘ USB.deviceConfigs $ USB.deviceDesc dev
         , devChipType = chip
         }

-- |Tries to guess the type of the FTDI chip by looking at the USB
-- device release number of a device's descriptor. Each FTDI chip uses
-- a specific release number to indicate its type.
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

-- |A device interface. You can imagine an interface as a port or a
-- communication channel. Some devices support communication over
-- multiple interfaces at the same time.
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

-- |You need a handle in order to communicate with a 'Device'.
data DeviceHandle = DeviceHandle
    { devHndUSB     ∷ USB.DeviceHandle
    , devHndDev     ∷ Device
    , devHndTimeout ∷ Int
    }

-- |Perform a USB device reset.
resetUSB ∷ DeviceHandle → IO ()
resetUSB = USB.resetDevice ∘ devHndUSB

-- |Returns the USB timeout associated with a handle.
getTimeout ∷ DeviceHandle → Int
getTimeout = devHndTimeout

-- |Modifies the USB timeout associated with a handle.
setTimeout ∷ DeviceHandle → Int → DeviceHandle
setTimeout devHnd timeout = devHnd {devHndTimeout = timeout}

-- |Open a device handle to enable communication. Only use this if you
-- can't use 'withDeviceHandle' for some reason.
openDevice ∷ Device → IO DeviceHandle
openDevice dev = do
  handle ← USB.openDevice $ devUSB dev
  USB.setConfig handle $ USB.configValue $ devUSBConf dev
  return DeviceHandle { devHndUSB     = handle
                      , devHndDev     = dev
                      , devHndTimeout = defaultTimeout
                      }

-- |Release a device handle.
closeDevice ∷ DeviceHandle → IO ()
closeDevice = USB.closeDevice ∘ devHndUSB

-- |The recommended way to acquire a handle. Ensures that the handle
-- is released when the monadic computation is completed. Even, or
-- especially, when an exception is thrown.
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
run. When invoking runChunkedReaderT for the first time you can safely pass the
'BS.empty' bytestring as the initial state.

A contrived example showing how you can manually thread the excess bytes
through subsequent invocations of runChunkedReaderT:

@
  example &#x2237; 'InterfaceHandle' &#x2192; IO ()
  example ifHnd = do
    (packets1, rest1) &#x2190; runChunkedReaderT ('readData' ifHnd (return 'False') 400) 'BS.empty'
    print $ 'BS.concat' packets1
    (packets2, rest2) &#x2190; runChunkedReaderT ('readData' ifHnd (return 'False') 200) rest1
    print $ 'BS.concat' packets2
@

However, it is much easier to let 'ChunkedReaderT's monad instance handle the
plumbing:

@
  example &#x2237; 'InterfaceHandle' &#x2192; IO ()
  example ifHnd =
    let reader = do packets1 &#x2190; 'readData' ifHnd (return 'False') 400
                    liftIO $ print $ 'BS.concat' packets1
                    packets2 &#x2190; 'readData' ifHnd (return 'False') 200
                    liftIO $ print $ 'BS.concat' packets1
    in runChunkedReaderT reader 'BS.empty'
@

-}
runChunkedReaderT ∷ ChunkedReaderT m α → ByteString → m (α, ByteString)
runChunkedReaderT = runStateT ∘ unCR

{-| Reads data from the given FTDI interface by performing bulk reads.

This function produces an action in the @ChunkedReaderT@ monad that
will read exactly the requested number of bytes unless it is
explicitly asked to stop early. Executing the @readData@ action will
block until either:

 * All data are read

 * The given checkStop action returns 'True'

The result value is a list of chunks, represented as
@ByteString@s. This representation was choosen for efficiency reasons.

Data are read in packets. The function may choose to request more than
needed in order to get the highest possible bandwidth. The excess of
bytes is kept as the state of the @ChunkedReaderT@ monad. A subsequent
invocation of @readData@ will first return bytes from the stored state
before requesting more from the device itself. A consequence of this
behaviour is that even when you request 100 bytes the function will
actually request 512 bytes (depending on the packet size) and /block/
until all 512 bytes are read! There is no workaround since requesting
less bytes than the packet size is an error.

USB timeouts will not interrupt @readData@. In case of a timeout
@readData@ will simply resume reading data. A small USB timeout can
degrade performance.

The FTDI latency timer can cause poor performance. If the FTDI chip can't fill
a packet before the latency timer fires it is forced to send an incomplete
packet. This will cause a stream of tiny packets instead of a few large
packets. Performance will suffer horribly, but the request will still be
completed.

If you need to make a lot of small requests then a small latency can actually
improve performance.

Modem status bytes are filtered from the result. Every packet send by the FTDI
chip contains 2 modem status bytes. They are not part of the data and do not
count for the number of bytes read. They will not appear in the result.

Example:

@
  -- Read 100 data bytes from ifHnd
  (packets, rest) &#x2190; 'runChunkedReaderT' ('readData' ifHnd (return 'False') 100) 'BS.empty'
@

-}
readData ∷ ∀ m. MonadIO m
         ⇒ InterfaceHandle
         → m Bool -- ^ Check stop action
         → Int -- ^ Number of bytes to read
         → ChunkedReaderT m [ByteString]
readData ifHnd checkStop numBytes = ChunkedReaderT $
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
        -- Timeout is ignored; the number of bytes that was read contains
        -- enough information.
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
          then let xs = splitPackets bytes
               in lift checkStop >>= \stop →
                  if stop
                  then put BS.empty >> return xs
                  else liftM (xs ⊕)
                             (readLoop $ readNumBytes - receivedDataBytes)
          else -- We might have received too much data, since we can only
               -- request multiples of 'packetSize' bytes. Split the byte
               -- string at such an index that the first part contains
               -- readNumBytes of data. The rest is kept for future usage.
               let (bs, newRest) = BS.splitAt (splitIndex readNumBytes) bytes
               in put newRest >> return (splitPackets bs)

      splitIndex n = p ⋅ packetSize + packetHeaderSize
                     + (n - p ⋅ packetDataSize)
          where p = n `div` packetDataSize

      packetDataSize   = packetSize - packetHeaderSize
      packetHeaderSize = 2
      packetSize       = USB.maxPacketSize
                         ∘ USB.endpointMaxPacketSize
                         $ ifHndInEPDesc ifHnd

      -- |Split a stream of bytes into packets. The first 2 bytes of each
      -- packet are the modem status bytes and are dropped.
      splitPackets xs | BS.null xs = []
                      | otherwise  = case BS.splitAt packetSize xs of
                                       (a, b) → BS.drop 2 a : splitPackets b

-- |Perform a bulk read.
--
-- Returns the bytes that where read (in the form of a 'ByteString') and a flag
-- which indicates whether a timeout occured during the request.
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
-- Returns the number of bytes that where written and a flag which indicates
-- whether a timeout occured during the request.
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
           → Word16 -- ^Index
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

-- |Returns the current value of the FTDI latency timer.
getLatencyTimer ∷ InterfaceHandle → IO Word8
getLatencyTimer ifHnd = do
    (bs, _) ← readControl ifHnd reqGetLatencyTimer 0 1
    case BS.unpack bs of
      [b] → return b
      _   → error "System.FTDI.getLatencyTimer: failed"

-- |Set the FTDI latency timer. The latency is the amount of
-- milliseconds after which the FTDI chip will send a packet
-- regardless of the number of bytes in the packet.
setLatencyTimer ∷ InterfaceHandle → Word8 → IO ()
setLatencyTimer ifHnd latency = control ifHnd reqSetLatencyTimer
                                        $ fromIntegral latency

-- |MPSSE bitbang modes
data BitMode = -- |Switch off bitbang mode, back to regular serial/FIFO.
               BitMode_Reset
               -- |Classical asynchronous bitbang mode, introduced with B-type
               -- chips.
             | BitMode_BitBang
               -- |Multi-Protocol Synchronous Serial Engine, available on 2232x
               -- chips.
             | BitMode_MPSSE
               -- |Synchronous Bit-Bang Mode, available on 2232x and R-type
               -- chips.
             | BitMode_SyncBitBang
               -- |MCU Host Bus Emulation Mode, available on 2232x
               -- chips. CPU-style fifo mode gets set via EEPROM.
             | BitMode_MCU
               -- |Fast Opto-Isolated Serial Interface Mode, available on 2232x
               -- chips.
             | BitMode_Opto
               -- |Bit-Bang on CBus pins of R-type chips, configure in EEPROM
               -- before use.
             | BitMode_CBus
               -- |Single Channel Synchronous FIFO Mode, available on 2232H
               -- chips.
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

-- |The bitmode controls the method of communication.
setBitMode ∷ InterfaceHandle → Word8 → BitMode → IO ()
setBitMode ifHnd bitMask bitMode = control ifHnd reqSetBitMode value
    where bitMask' = fromIntegral bitMask
          bitMode' = fromIntegral $ marshalBitMode bitMode
          value    = bitMask' .|. shiftL bitMode' 8

-- |Sets the baud rate. Internally the baud rate is represented as a
-- fraction. The maximum baudrate is the numerator and a special
-- /divisor/ is used as the denominator. The maximum baud rate is
-- given by the 'BaudRate' instance for 'Bounded'. The divisor
-- consists of an integral part and a fractional part. Both parts are
-- limited in range. As a result not all baud rates can be accurately
-- represented. This function returns the nearest representable baud
-- rate relative to the requested baud rate. According to FTDI
-- documentation the maximum allowed error is 3%. The nearest
-- representable baud rate can be calculated with the
-- 'nearestBaudRate' function.
setBaudRate ∷ RealFrac α ⇒ InterfaceHandle → BaudRate α → IO (BaudRate α)
setBaudRate ifHnd baudRate =
  do genControl USB.control ix ifHnd reqSetBaudRate val
     return b
  where
    (val, ix) = encodeBaudRateDivisors chip d s
    (d, s, b) = calcBaudRateDivisors chip baudRate
    chip = devChipType $ devHndDev $ ifHndDevHnd ifHnd


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

-- |Manually request the modem status.
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
-- Baud rate
-------------------------------------------------------------------------------

newtype BRDiv α = BRDiv {unBRDiv ∷ α}
    deriving ( Eq, Ord, Show, Read, Enum, Num, Integral
             , Real, Fractional, RealFrac
             )

instance Num α ⇒ Bounded (BRDiv α) where
    minBound = 0
    maxBound = 2 ^ (14 ∷ Int) - 1

newtype BRSubDiv α = BRSubDiv {unBRSubDiv ∷ α}
    deriving ( Eq, Ord, Show, Read, Enum, Num, Integral
             , Real, Fractional, RealFrac
             )

instance Num α ⇒ Bounded (BRSubDiv α) where
    minBound = 0
    maxBound = 7

-- |Representation of a baud rate. The most interesting part is the
-- instance for 'Bounded'.
newtype BaudRate α = BaudRate {unBaudRate ∷ α}
    deriving ( Eq, Ord, Show, Read, Enum, Num, Integral
             , Real, Fractional, RealFrac
             )

instance Num α ⇒ Bounded (BaudRate α) where
    -- Minimum baud rate is the maximum baudrate divided by the
    -- largest possible divider.
    minBound = fromIntegral
               $ (ceiling ∷ BaudRate Double → BaudRate Integer)
               $ calcBaudRate maxBound maxBound

    maxBound = BaudRate 3000000

-- http://www.ftdichip.com/Documents/AppNotes/AN232B-05_BaudRates.pdf
encodeBaudRateDivisors ∷ ChipType → BRDiv Int → BRSubDiv Int → (Word16, Word16)
encodeBaudRateDivisors chip d s = (v, i)
  where
    v = fromIntegral d .|. shiftL s' 14
    i | ChipType_2232C ← chip = shiftL (shiftR s' 2) 8
      | otherwise = shiftR s' 2
    s' = fromIntegral $ encodeSubDiv s ∷ Word16

    encodeSubDiv ∷ BRSubDiv Int → Int
    encodeSubDiv n =
        case n of
          0 → 0 -- 000 ==> 0/8 = 0
          4 → 1 -- 001 ==> 4/8 = 0.5
          2 → 2 -- 010 ==> 2/8 = 0.25
          1 → 3 -- 011 ==> 1/8 = 0.125
          3 → 4 -- 100 ==> 3/8 = 0.375
          5 → 5 -- 101 ==> 5/8 = 0.625
          6 → 6 -- 110 ==> 6/8 = 0.75
          7 → 7 -- 111 ==> 7/8 = 0.875
          _ → error "Illegal subdivisor"

-- |Calculates the nearest representable baud rate.
nearestBaudRate ∷ RealFrac α ⇒ ChipType → BaudRate α → BaudRate α
nearestBaudRate chip baudRate = b
  where (_, _, b) = calcBaudRateDivisors chip baudRate


-- |Finds the divisors that most closely represent the requested baud rate.
calcBaudRateDivisors ∷ ∀ α. RealFrac α
                     ⇒ ChipType
                     → BaudRate α
                     → (BRDiv Int, BRSubDiv Int, BaudRate α)
calcBaudRateDivisors _    3000000  = (0, 0, 0)
calcBaudRateDivisors _    2000000  = (1, 0, 0)
calcBaudRateDivisors chip baudRate =
    minimumBy (compare `on` (\(_,_,x) → x))
              [ (d, s, b')
              | s ← chipSubDivisors chip
              , let s' = fromIntegral s ÷ 8
                    d  = divisor baudRate s'
                    -- Baud rate calculated from found divisors.
                    b' = calcBaudRate d s'
              ]
    where
      -- |Calculates the divisor from a baud rate and a subdivisor.
      divisor ∷ Integral β ⇒ BaudRate α → BRSubDiv α → BRDiv β
      divisor br s = clamp $ floor $ (maxBound - br ⋅ s') ÷ br
          where s' = BaudRate $ unBRSubDiv s

      chipSubDivisors ∷ ChipType → [BRSubDiv Int]
      chipSubDivisors ChipType_AM = [0, 1, 2, 4]
      chipSubDivisors _           = [0..7]

-- |Calculates the baud rate from a divisor and a subdivisor.
calcBaudRate ∷ Fractional α ⇒ BRDiv Int → BRSubDiv α → BaudRate α
calcBaudRate 0 0 = maxBound
calcBaudRate 1 0 = 2000000
calcBaudRate d s = maxBound ÷ BaudRate (realToFrac d + unBRSubDiv s)
