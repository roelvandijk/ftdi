{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-

TODO: physical quantities

- timeout (time)
- latency (time)
- baudrate (frequency)

-}

module System.FTDI where

import Prelude hiding ( drop )

import Control.Exception              ( Exception, bracket, throwIO )
import Control.Monad                  ( liftM2 )
import Control.Monad.CatchIO          ( MonadCatchIO )
import Data.Bits                      ( Bits, (.|.), (.&.)
                                      , complement, setBit
                                      , shiftL, testBit
                                      )
import Data.Data                      ( Data )
import Data.Function                  ( on )
import Data.List                      ( minimumBy )
import Data.Iteratee.Base             ( EnumeratorGM, drop )
import Data.Iteratee.Base.StreamChunk ( ReadableChunk )
import Data.Typeable                  ( Typeable )
import Data.Word                      ( Word8, Word16 )
import Safe                           ( atMay, headMay )

import qualified Data.ByteString as BS
import qualified System.USB      as USB


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

data FlowCtrl = RTS_CTS -- |Request-To-Send / Clear-To-Send
              | DTR_DSR -- |Data-Terminal-Ready / Data-Set-Ready
              | XOnXOff -- |Transmitter on / transmitter off

data Interface = Interface_A
               | Interface_B
               | Interface_C
               | Interface_D
                 deriving (Enum, Eq, Ord, Show, Data, Typeable)

type RequestCode  = Word8
type RequestValue = Word16

data ModemStatus = ModemStatus
    { msClearToSend                :: Bool
    , msDataSetReady               :: Bool
    , msRingIndicator              :: Bool
    , msReceiveLineSignalDetect    :: Bool
    , msDataReady                  :: Bool
    , msOverrunError               :: Bool
    , msParityError                :: Bool
    , msFramingError               :: Bool
    , msBreakInterrupt             :: Bool
    , msTransmitterHoldingRegister :: Bool
    , msTransmitterEmpty           :: Bool
    , msErrorInReceiverFIFO        :: Bool
    } deriving (Eq, Ord, Show, Data, Typeable)

data Device = Device
    { devUSB      :: USB.Device
    , devUSBDesc  :: USB.DeviceDesc
    , devUSBConf  :: USB.ConfigDesc
    , devChipType :: ChipType
    }

data DeviceHandle = DeviceHandle
    { devHndUSB     :: USB.DeviceHandle
    , devHndDev     :: Device
    , devHndTimeout :: Int
    }

data InterfaceHandle = InterfaceHandle
    { ifHndUSB       :: USB.InterfaceHandle
    , ifHndDevHnd    :: DeviceHandle
    , ifHndInterface :: Interface
    , ifHndInEPDesc  :: USB.EndpointDesc USB.In
    , ifHndOutEPDesc :: USB.EndpointDesc USB.Out
    }

data FTDIException = InterfaceInUse
                   | InterfaceNotFound
                     deriving (Show, Data, Typeable)

instance Exception FTDIException

-------------------------------------------------------------------------------
-- Constants
-------------------------------------------------------------------------------

maxBaudRate :: Num a => a
maxBaudRate = 3000000

reqReset           :: RequestCode
reqSetModemCtrl    :: RequestCode
reqSetFlowCtrl     :: RequestCode
reqSetBaudRate     :: RequestCode
reqSetData         :: RequestCode
reqPollModemStatus :: RequestCode
reqSetEventChar    :: RequestCode
reqSetErrorChar    :: RequestCode
reqSetLatencyTimer :: RequestCode
reqGetLatencyTimer :: RequestCode
reqSetBitMode      :: RequestCode
reqReadPins        :: RequestCode
reqReadEEPROM      :: RequestCode
reqWriteEEPROM     :: RequestCode
reqEraseEEPROM     :: RequestCode

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

valResetSIO         :: RequestValue
valPurgeReadBuffer  :: RequestValue
valPurgeWriteBuffer :: RequestValue

valResetSIO         = 0
valPurgeReadBuffer  = 1
valPurgeWriteBuffer = 2

valSetDTRHigh :: RequestValue
valSetDTRLow  :: RequestValue
valSetRTSHigh :: RequestValue
valSetRTSLow  :: RequestValue

valSetDTRHigh = 0x0101
valSetDTRLow  = 0x0100
valSetRTSHigh = 0x0202
valSetRTSLow  = 0x0200

-------------------------------------------------------------------------------
-- Defaults
-------------------------------------------------------------------------------

defaultTimeout :: Int
defaultTimeout = 5000

-------------------------------------------------------------------------------

marshalBitMode :: BitMode -> Word8
marshalBitMode bm = case bm of
                      BitMode_Reset       -> 0x00
                      BitMode_BitBang     -> 0x01
                      BitMode_MPSSE       -> 0x02
                      BitMode_SyncBitBang -> 0x04
                      BitMode_MCU         -> 0x08
                      BitMode_Opto        -> 0x10
                      BitMode_CBus        -> 0x20
                      BitMode_SyncFIFO    -> 0x40


marshalFlowControl :: FlowCtrl -> Word16
marshalFlowControl f = case f of
                         RTS_CTS -> 0x0100
                         DTR_DSR -> 0x0200
                         XOnXOff -> 0x0400

unmarshalModemStatus :: Word8 -> Word8 -> ModemStatus
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

interfaceIndex :: Interface -> Word16
interfaceIndex = (+ 1) . genFromEnum

interfaceToUSB :: Interface -> USB.InterfaceNumber
interfaceToUSB = genFromEnum

interfaceEndPointIn :: Interface -> USB.EndpointAddress USB.In
interfaceEndPointIn i = USB.EndpointAddress $ 1 + 2 * genFromEnum i

interfaceEndPointOut :: Interface -> USB.EndpointAddress USB.Out
interfaceEndPointOut i = USB.EndpointAddress $ 2 + 2 * genFromEnum i

supportedSubDivisors :: ChipType -> [Int]
supportedSubDivisors ChipType_AM = [0, 1, 2, 4]
supportedSubDivisors _           = [0..7]

-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Devices
-------------------------------------------------------------------------------

fromUSBDevice :: USB.Device -> ChipType -> IO Device
fromUSBDevice dev chip = do
  desc <- USB.getDeviceDesc dev
  conf <- USB.getConfigDesc dev 0
  return Device { devUSB      = dev
                , devUSBDesc  = desc
                , devUSBConf  = conf
                , devChipType = chip
                }

guessChipType :: USB.DeviceDesc -> Maybe ChipType
guessChipType desc = case USB.deviceReleaseNumber desc of
                       -- Workaround for bug in BM type chips
                       (0,2,0,0) | USB.deviceSerialNumberStrIx desc == 0
                                             -> Just ChipType_BM
                                 | otherwise -> Just ChipType_AM
                       (0,4,0,0) -> Just ChipType_BM
                       (0,5,0,0) -> Just ChipType_2232C
                       (0,6,0,0) -> Just ChipType_R
                       (0,7,0,0) -> Just ChipType_2232H
                       (0,8,0,0) -> Just ChipType_4232H
                       _         -> Nothing

-------------------------------------------------------------------------------
-- Device Handles
-------------------------------------------------------------------------------

openDevice :: Device -> IO DeviceHandle
openDevice dev = do
  handle <- USB.openDevice $ devUSB dev
  USB.setConfig handle $ USB.configValue $ devUSBConf dev
  return DeviceHandle { devHndUSB       = handle
                      , devHndDev       = dev
                      , devHndTimeout   = defaultTimeout
                      }

closeDevice :: DeviceHandle -> IO ()
closeDevice = USB.closeDevice . devHndUSB

withDeviceHandle :: Device -> (DeviceHandle -> IO a) -> IO a
withDeviceHandle dev = bracket (openDevice dev) closeDevice

-------------------------------------------------------------------------------
-- Interface Handles
-------------------------------------------------------------------------------

openInterface :: DeviceHandle -> Interface -> IO InterfaceHandle
openInterface devHnd i = do
  usbIfHnd <- USB.claimInterface (devHndUSB devHnd) (interfaceToUSB i)
  maybe (throwIO InterfaceNotFound)
        (\(inEP, outEP) ->
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
          mInEP   = headMay . USB.interfaceInEndpoints  =<< mIfDesc
          mOutEP  = headMay . USB.interfaceOutEndpoints =<< mIfDesc

closeInterface :: InterfaceHandle -> IO ()
closeInterface ifHnd = let usbIfHnd = ifHndUSB ifHnd
                       in USB.releaseInterface usbIfHnd

withInterface :: DeviceHandle -> Interface -> (InterfaceHandle -> IO a) -> IO a
withInterface h c = bracket (openInterface h c) closeInterface

-------------------------------------------------------------------------------
-- Enumerators
-------------------------------------------------------------------------------

enumRead :: (ReadableChunk s el, MonadCatchIO m)
         => InterfaceHandle -> EnumeratorGM s el m a
enumRead ifHnd iter =
    USB.enumReadBulk (ifHndUSB ifHnd)
                     (interfaceEndPointIn $ ifHndInterface ifHnd)
                     (devHndTimeout $ ifHndDevHnd ifHnd)
                     ( USB.maxPacketSize . USB.endpointMaxPacketSize
                     $ ifHndInEPDesc ifHnd
                     )
                     (drop 2 >> iter)

-------------------------------------------------------------------------------
-- Control Requests
-------------------------------------------------------------------------------

-- |The type of a USB control request.
type USBControl a =  USB.DeviceHandle
                  -> USB.RequestType
                  -> USB.Recipient
                  -> RequestCode
                  -> RequestValue
                  -> Word16
                  -> USB.Timeout
                  -> a

-- |Generic FTDI control request with explicit index
genControl :: USBControl a
           -> Word16
           -> InterfaceHandle
           -> RequestCode
           -> RequestValue
           -> a
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

control :: InterfaceHandle -> RequestCode -> Word16 -> IO ()
control = genControl USB.control 0

readControl :: InterfaceHandle -> RequestCode -> Word16 -> USB.Size -> IO (BS.ByteString, Bool)
readControl = genControl USB.readControl 0

writeControl :: InterfaceHandle -> RequestCode -> Word16 -> BS.ByteString -> IO (USB.Size, Bool)
writeControl = genControl USB.writeControl 0

-------------------------------------------------------------------------------

reset :: InterfaceHandle -> IO ()
reset ifHnd = control ifHnd reqReset valResetSIO

purgeReadBuffer :: InterfaceHandle -> IO ()
purgeReadBuffer ifHnd = control ifHnd reqReset valPurgeReadBuffer

purgeWriteBuffer :: InterfaceHandle -> IO ()
purgeWriteBuffer ifHnd = control ifHnd reqReset valPurgeWriteBuffer

-------------------------------------------------------------------------------

getLatencyTimer :: InterfaceHandle -> IO Word8
getLatencyTimer ifHnd = do
    (bs, _) <- readControl ifHnd reqGetLatencyTimer 0 1
    case BS.unpack bs of
      [b] -> return b
      _   -> error "System.FTDI.getLatencyTimer: failed"

setLatencyTimer :: InterfaceHandle -> Word8 -> IO ()
setLatencyTimer ifHnd latency = control ifHnd reqSetLatencyTimer $ fromIntegral latency

pollModemStatus :: InterfaceHandle -> IO ModemStatus
pollModemStatus ifHnd = do
    (bs, _) <- readControl ifHnd reqPollModemStatus 0 2
    case BS.unpack bs of
      [x,y] -> return $ unmarshalModemStatus x y
      _     -> error "System.FTDI.pollModemStatus: failed"

setBitMode :: InterfaceHandle -> Word8 -> BitMode -> IO ()
setBitMode ifHnd bitMask bitMode = control ifHnd reqSetBitMode value
    where bitMask' = fromIntegral bitMask
          bitMode' = fromIntegral $ marshalBitMode bitMode
          value    = bitMask' .|. shiftL bitMode' 8

-- TODO: finish implementation (encoding divisor and subdivisor in index and
-- value fields of control request)
-- TODO: baudrate is also a function of the bitmode (bitbang needs baudrate * 4)
setConfiguration :: RealFrac a => InterfaceHandle -> Word8 -> BitMode -> a -> IO ()
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

setLineProperty :: InterfaceHandle
                -> BitDataFormat
                -> StopBits
                -> Maybe Parity
                -> Bool
                -> IO ()
setLineProperty ifHnd bitDataFormat stopBits parity break =
    control ifHnd
            reqSetData
            $ orBits [ case bitDataFormat of
                         Bits_7 -> 7
                         Bits_8 -> 8
                     , maybe 0 (\p -> (1 + genFromEnum p) `shiftL` 8) parity
                     , genFromEnum stopBits `shiftL` 11
                     , genFromEnum break    `shiftL` 14
                     ]

-------------------------------------------------------------------------------
-- Flow control
-------------------------------------------------------------------------------

setFlowControl :: InterfaceHandle -> Maybe FlowCtrl -> IO ()
setFlowControl ifHnd mFC = genControl USB.control
                                      (maybe 0 marshalFlowControl mFC)
                                      ifHnd
                                      reqSetFlowCtrl
                                      0

setDTR :: InterfaceHandle -> Bool -> IO ()
setDTR ifHnd b = control ifHnd reqSetModemCtrl
                         $ if b then valSetDTRHigh else valSetDTRLow

setRTS :: InterfaceHandle -> Bool -> IO ()
setRTS ifHnd b = control ifHnd reqSetModemCtrl
                         $ if b then valSetRTSHigh else valSetRTSLow

genSetCharacter :: RequestCode -> InterfaceHandle -> Maybe Word8 -> IO ()
genSetCharacter req ifHnd mEC =
    control ifHnd req $ maybe 0 (\c -> setBit (fromIntegral c) 8) mEC

setEventCharacter :: InterfaceHandle -> Maybe Word8 -> IO ()
setEventCharacter = genSetCharacter reqSetEventChar

setErrorCharacter :: InterfaceHandle -> Maybe Word8 -> IO ()
setErrorCharacter = genSetCharacter reqSetErrorChar

-------------------------------------------------------------------------------
-- Miscellaneous
-------------------------------------------------------------------------------

-- |Finds the divisors the most closely represent the requested baud rate.
calcBaudRateDivisors :: forall a. RealFrac a => [Int] -> a -> (Int, Int, a)
calcBaudRateDivisors _  3000000  = (0, 0, 0)
calcBaudRateDivisors _  2000000  = (1, 0, 0)
calcBaudRateDivisors ss baudRate =
    minimumBy (compare `on` (\(_,_,x) -> x))
              [ (d, s, (abs $ baudRate - b') / baudRate)
              | s <- ss
              , let s' = fromIntegral s / 8
                    -- The calculated divisor is clamped to an acceptable
                    -- range.
                    d  = clamp 2 (2 ^ (14::Int) - 1)
                               $ divisor baudRate s'
                    -- Baud rate calculated from found divisors.
                    b' = calcBaudRate d s'
              ]
    where
      -- |Calculates the divisor from a baud rate and a subdivisor.
      divisor :: Integral b => a -> a -> b
      divisor br s = floor $ (maxBaudRate - br * s) / br

      -- |Calculates the baud rate from a divisor and a subdivisor.
      calcBaudRate :: Int -> a -> a
      calcBaudRate d s = maxBaudRate / (realToFrac d + s)

-------------------------------------------------------------------------------
-- Utility functions
-------------------------------------------------------------------------------

genFromEnum :: (Enum e, Num n) => e -> n
genFromEnum = fromIntegral . fromEnum

-- between :: Ord a => a -> a -> a -> Bool
-- between lo hi = (> lo) <^ (&&) ^> (< hi)

orBits :: Bits a => [a] -> a
orBits = foldr (.|.) 0

andBits :: Bits a => [a] -> a
andBits = foldr (.&.) $ complement 0

clamp :: Ord a => a -> a -> a -> a
clamp lo hi x | x < lo    = lo
              | x > hi    = hi
              | otherwise = x
