{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Control.Monad
import Data.Bits
import Data.Time.Clock     ( getCurrentTime, diffUTCTime )
import Data.Word
import Prelude.Unicode
import Text.Printf         ( printf )
import qualified Data.ByteString as BS
import qualified System.FTDI     as FTDI
import qualified System.USB      as USB

main ∷ IO ()
main = do ctx      ← USB.newCtx
          devList  ← USB.getDevices ctx
          descList ← mapM USB.getDeviceDesc devList
          let xs ∷ [(USB.Device, USB.DeviceDesc)]
              xs = filter (isFTDI ∘ snd) $ zip devList descList
          if not $ null xs
            then do let x = head xs
                    maybe (putStrLn "Unknown chip type")
                          (\c → withBLD =<< FTDI.fromUSBDevice (fst x) c)
                          $ FTDI.guessChipType (snd x)
            else putStrLn "BLD not found :-("

ftdi_vid ∷ USB.VendorId
ftdi_vid = 0x0403

ft2232h_pid ∷ USB.ProductId
ft2232h_pid = 0x6010

isFTDI ∷ USB.DeviceDesc → Bool
isFTDI = (ftdi_vid ≡) ∘ USB.deviceVendorId

withBLD ∷ FTDI.Device → IO ()
withBLD dev = FTDI.withDeviceHandle dev $ \devHnd →
              FTDI.withInterfaceHandle
                      (devHnd {FTDI.devHndTimeout = timeout})
                      FTDI.Interface_A $ \ifHnd →
                do FTDI.reset            ifHnd
                   FTDI.setLatencyTimer  ifHnd latency
                   FTDI.setBitMode ifHnd 0xff FTDI.BitMode_SyncFIFO

                   (results, rest) ← readSomeBytes ifHnd numBytes numLoops

                   let totalBytes = sum $ map fst results
                       avgBytes = fromIntegral totalBytes ÷ numLoops
                       times    = map snd results
                       avgTime  = sum times ÷ numLoops
                       minTime  = minimum times
                       maxTime  = maximum times

                   printf "Requested bytes: %i * %i = %i\n"
                          (numLoops ∷ Int)
                          numBytes
                          (numLoops ⋅ numBytes ∷ Int)
                   printf "Total bytes:                %i\n" totalBytes
                   printf "Rest bytes:                 %i\n" rest
                   printf "Avg bytes:                  %i\n" (floor avgBytes ∷ Int)
                   printf "Minimum time:               %.3f seconds\n" minTime
                   printf "Average time:               %.3f seconds\n" avgTime
                   printf "Maximum time:               %.3f seconds\n" maxTime
                   printf "Minimum samples per second: %i\n"
                          (floor $ avgBytes ÷ 3 ÷ maxTime ∷ Int)
                   printf "Average samples per second: %i\n"
                          (floor $ avgBytes ÷ 3 ÷ avgTime ∷ Int)
                   printf "Maximum samples per second: %i\n"
                          (floor $ avgBytes ÷ 3 ÷ minTime ∷ Int)
    where numLoops ∷ ∀ α. Num α ⇒ α
          numLoops = 5
          numBytes = 1000 ⋅ 1000 ⋅ 3 ⋅ 1
          timeout  = 5000
          latency  = 16

readSomeBytes ∷ FTDI.InterfaceHandle → Int → Int → IO ([(Int, Double)], Int)
readSomeBytes ifHnd numBytes numLoops = do
  let reader ∷ FTDI.ChunkedReader IO [BS.ByteString]
      reader = FTDI.readData ifHnd numBytes

      timedChunk ∷ BS.ByteString → IO ((Int, Double), BS.ByteString)
      timedChunk rest = do
        t1 ← getCurrentTime
        start ifHnd
        (packets, rest') ← FTDI.runChunkedReader reader rest
        stop ifHnd
        t2 ← getCurrentTime

        let totalBytes = sum $ map BS.length packets
        -- printf "%i bytes in %i packets\n"
        --        totalBytes
        --        (length packets)
        -- forM_ (zip packets [(1 ∷ Int)..]) $ \(p, n) →
        --       do printf "packet %i\n" n
        --          printPacket 66 p
        -- printf "\n"
        return $ ((totalBytes, realToFrac $ diffUTCTime t2 t1), rest')

      readLoop ∷ ([(Int, Double)], BS.ByteString)
               → Int
               → IO ([(Int, Double)], BS.ByteString)
      readLoop (acc, rest) _ = do (result, rest') ← timedChunk rest
                                  return $ (result:acc, rest')

  (results, rest) ← foldM readLoop ([], BS.empty) [1 .. numLoops]
  return (results, BS.length rest)

printPacket ∷ Int → BS.ByteString → IO ()
printPacket n bs = forM_ (chunkify n $ BS.unpack bs)
                 $ \row → do forM_ row $ printf "%02x "
                             putStrLn ""

sendBLDControlByte ∷ FTDI.InterfaceHandle → Word8 → IO ()
sendBLDControlByte ih = ignore ∘ FTDI.writeBulk ih ∘ BS.singleton

start ∷ FTDI.InterfaceHandle → IO ()
start ih = sendBLDControlByte ih
         $ bld_encodeControl LogRatio
                             False
                             False
                             False
                             True
                             Continuous

stop ∷ FTDI.InterfaceHandle → IO ()
stop ih = sendBLDControlByte ih
        $ bld_encodeControl LogRatio
                            False
                            False
                            False
                            True
                            NoSampling

-------------------------------------------------------------------------------

groupSamples ∷ [Word8] → [(Word8, Word8, Word8)]
groupSamples bs = map extractSample ∘ chunkify 3 $ drop 2 bs
          where
            extractSample ∷ [Word8] → (Word8, Word8, Word8)
            extractSample xs | length xs ≢ 3 = (0, 0, 0)
                             | otherwise = let [x, y, z] = xs
                                           in (x, y, z)

decodeSample ∷ (Word8, Word8, Word8) → Double
decodeSample (x, y, z) = let bits_0_5 = fromIntegral (x .&. 0x3f)
                             bits_6_a = fromIntegral (y .&. 0x1f)
                             bits_b_f = fromIntegral (z .&. 0x1f)
                             w16 ∷ Word16
                             w16 =     bits_0_5
                                   .|. bits_6_a `shiftL` 6
                                   .|. bits_b_f `shiftL` 11
                         in (fromIntegral w16 ⋅ 4096) ÷ 65535

-- decodeVoltages ∷ BS.ByteString → [Double]
-- decodeVoltages bs | BS.length bs < 5 = []
--                   | otherwise        = map (toVoltage ∘ decodeSample)
--                                      ∘ chunkify 3
--                                      ∘ drop 2
--                                      $ BS.unpack bs
--     where
--       decodeSample ∷ [Word8] → Word16
--       decodeSample xs | length xs ≠ 3 = 0
--                       | otherwise     = let [x, y, z] = xs
--                                             bits_0_5 = fromIntegral (x .&. 0x3f)
--                                             bits_6_a = fromIntegral (y .&. 0x1f)
--                                             bits_b_f = fromIntegral (z .&. 0x1f)
--                                         in     bits_0_5
--                                            .|. bits_6_a `shiftL` 6
--                                            .|. bits_b_f `shiftL` 11

--       toVoltage ∷ Word16 → Double
--       toVoltage w = (fromIntegral w ⋅ 4096) ÷ 65535

-------------------------------------------------------------------------------
-- BLD functions

data InputChannel = LogRatio
                  | CurrentD1
                  | CurrentD2

data Sampling = NoSampling
              | Continuous
              | Single
                deriving (Enum, Show)

bld_encodeControl ∷ InputChannel → Bool → Bool → Bool → Bool → Sampling → Word8
bld_encodeControl chan out2 out3 out4 clearErr sampling
  = chanVal chan
    + digOutVal out2 2
    + digOutVal out3 3
    + digOutVal out4 4
    + digOutVal clearErr 5
    + genFromEnum sampling ⋅ 64
    where
      chanVal LogRatio  = 0
      chanVal CurrentD1 = 3
      chanVal CurrentD2 = 2

      digOutVal ∷ Bool → Int → Word8
      digOutVal False _  = 0
      digOutVal True  n  = 2 ^ n

-------------------------------------------------------------------------------
-- Miscellaneous

genFromEnum ∷ (Enum e, Num n) ⇒ e → n
genFromEnum = fromIntegral ∘ fromEnum

ignore ∷ Monad m ⇒ m α → m ()
ignore = (>> return ())

chunkify ∷ Int → [α] → [[α]]
chunkify n = go
    where
      go [] = []
      go xs = let (a, b) = splitAt n xs
              in a : go b


