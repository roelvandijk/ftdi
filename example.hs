module Main where

import Control.Concurrent
import Control.Monad
import Data.Bits
import Data.Word
import Data.Iteratee.Base ( IterateeG(..), IterGV(..), StreamG(..) )
import qualified Data.ByteString as BS
import qualified System.USB      as USB
import qualified System.FTDI     as FTDI

main :: IO ()
main = do ctx      <- USB.newCtx
          devList  <- USB.getDevices ctx
          descList <- mapM USB.getDeviceDesc devList
          let xs :: [(USB.Device, USB.DeviceDesc)]
              xs = filter (isFTDI . snd) $ zip devList descList
          if not $ null xs
            then do let x = head xs
                    maybe (putStrLn "Unknown chip type")
                          (\c -> withBLD =<< FTDI.fromUSBDevice (fst x) c)
                          $ FTDI.guessChipType (snd x)
            else putStrLn "BLD not found :-("

ftdi_vid :: USB.VendorId
ftdi_vid = 0x0403

ft2232h_pid :: USB.ProductId
ft2232h_pid = 0x6010

isFTDI :: USB.DeviceDesc -> Bool
isFTDI = (ftdi_vid ==) . USB.deviceVendorId

withBLD :: FTDI.Device -> IO ()
withBLD dev = FTDI.withDeviceHandle dev $ \devHnd ->
              FTDI.withInterface devHnd FTDI.Interface_A $ \ifHnd ->
                do let ih = FTDI.ifHndUSB ifHnd
                   FTDI.reset            ifHnd
                   FTDI.setLatencyTimer  ifHnd 16

                   FTDI.setBitMode ifHnd 0xff FTDI.BitMode_SyncFIFO
                   FTDI.control ifHnd FTDI.reqSetBaudRate 0

                   -- FTDI.setConfiguration ifHnd
                   --                       0xff
                   --                       FTDI.BitMode_SyncFIFO
                   --                       (FTDI.maxBaudRate :: Double)

                   let theByte = bld_encodeControl LogRatio
                                                   False
                                                   False
                                                   False
                                                   True
                                                   Continuous
                   USB.writeBulk
                      ih
                      (USB.EndpointAddress 2 :: USB.EndpointAddress USB.Out)
                      timeout
                      (BS.singleton theByte)

                   threadDelay 100000

                   let theByte = bld_encodeControl LogRatio
                                                   False
                                                   False
                                                   False
                                                   True
                                                   NoSampling

                   USB.writeBulk
                      ih
                      (USB.EndpointAddress 2 :: USB.EndpointAddress USB.Out)
                      timeout
                      (BS.singleton theByte)

                   replicateM_ 1 $ do
                     bytes <- USB.readBulk
                                ih
                                (USB.EndpointAddress 1 :: USB.EndpointAddress USB.In)
                                timeout
                                512

                     putStrLn $ "Read " ++ show (BS.length bytes) ++ " bytes"
                     mapM_ print $ decodeVoltages bytes

                   let theByte = bld_encodeControl LogRatio
                                                   False
                                                   False
                                                   False
                                                   True
                                                   NoSampling
                   USB.writeBulk
                      ih
                      (USB.EndpointAddress 2 :: USB.EndpointAddress USB.Out)
                      timeout
                      (BS.singleton theByte)

                   return ()

baudRate3M :: Word16
baudRate3M = 0

timeout :: Int
timeout = 5000

-------------------------------------------------------------------------------

decodeVoltages :: BS.ByteString -> [Double]
decodeVoltages bs | BS.length bs < 5 = []
                  | otherwise        = map (toVoltage . decodeSample)
                                     . chunkify 3
                                     . drop 2
                                     $ BS.unpack bs
    where
      decodeSample :: [Word8] -> Word16
      decodeSample xs | length xs /= 3 = 0
                      | otherwise      = let [x, y, z] = xs
                                             bits_0_5 = fromIntegral (x .&. 0x3f)
                                             bits_6_a = fromIntegral (y .&. 0x1f)
                                             bits_b_f = fromIntegral (z .&. 0x1f)
                                         in     bits_0_5
                                            .|. bits_6_a `shiftL` 6
                                            .|. bits_b_f `shiftL` 11

      toVoltage :: Word16 -> Double
      toVoltage w = (fromIntegral w * 4096) / 65535

-------------------------------------------------------------------------------
-- BLD functions

data InputChannel = LogRatio
                  | CurrentD1
                  | CurrentD2

data Sampling = NoSampling
              | Continuous
              | Single
                deriving (Enum, Show)

bld_encodeControl :: InputChannel -> Bool -> Bool -> Bool -> Bool -> Sampling -> Word8
bld_encodeControl chan out2 out3 out4 clearErr sampling = chanVal chan
                                                        + digOutVal out2 2
                                                        + digOutVal out3 3
                                                        + digOutVal out4 4
                                                        + digOutVal clearErr 5
                                                        + genFromEnum sampling * 64
    where
      chanVal LogRatio  = 0
      chanVal CurrentD1 = 3
      chanVal CurrentD2 = 2

      digOutVal :: Bool -> Int -> Word8
      digOutVal False _ = 0
      digOutVal True n  = 2 ^ n

-------------------------------------------------------------------------------
-- Miscellaneous

genFromEnum :: (Enum e, Num n) => e -> n
genFromEnum = fromIntegral . fromEnum

ignore :: Monad m => m a -> m ()
ignore = (>> return ())

chunkify :: Int -> [a] -> [[a]]
chunkify n = go
    where
      go [] = []
      go xs = let (a, b) = splitAt n xs
              in a : go b

-- prop_concat n xs = n > 0 ==>
--                    xs == concat $ chunkify n xs

-- prop_tranpose xs = not (null xs) ==>
--                    xs == (fst $ transpose $ chunkify 1 xs)

-- prop_length n xs = n > 0 ==>
--                    divRoundUp (length xs) n == length (chunkify n xs)

-- divRoundUp x y = let (d, m) = x `divMod` y
--                  in d + signum m

