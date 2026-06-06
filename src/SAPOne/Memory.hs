{-# LANGUAGE NumericUnderscores #-}

module SAPOne.Memory where

import Clash.Annotations.TH
import Clash.Prelude
import SAPOne.Types

memory :: (HiddenClockResetEnable dom) => Signal dom Bool -> Signal dom Word8 -> Signal dom Word8
memory en bus = blockRamFile d16 "program.bin" mar (pure Nothing)
  where
    mar = register 0 (mux en busUpper mar)
    busUpper = (.&.) 0b0000_1111 <$> bus

memory' :: (HiddenClockResetEnable dom) => Signal dom Bool -> Signal dom Word8 -> Signal dom Word8
memory' en bus = blockRam prog mar (pure Nothing)
  where
    prog =
      0b0000_1101
        :> 0b0001_1110
        :> 0b0010_1111
        :> 0b1111_0000
        :> 0b0000_0000
        :> 0b0000_0000
        :> 0b0000_0000
        :> 0b0000_0000
        :> 0b0000_0000
        :> 0b0000_0000
        :> 0b0000_0000
        :> 0b0000_0000
        :> 0b0000_0000
        :> 0b0000_0011
        :> 0b0000_0100
        :> 0b0000_0010
        :> Nil
    mar = register (0 :: Word8) (mux en busAddr mar)
    busAddr = (.&.) 0b0000_1111 <$> bus

memory''' :: (HiddenClockResetEnable dom) => Signal dom Bool -> Signal dom Word8 -> Signal dom Word8
memory''' en bus = blockRamFilePow2 "program.hex" addr (pure Nothing)
  where
    mar = register 0 (mux en bus mar)
    addr = unpack . slice d3 d0 <$> mar

memory'''' :: (HiddenClockResetEnable dom) => Signal dom Bool -> Signal dom (Unsigned 4) -> Signal dom Word8
memory'''' en addr = blockRamFilePow2 "program.hex" mar (pure Nothing)
  where
    mar = register 0 (mux en addr mar)

asyncMemory :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom (Unsigned 4) -> Signal dom Word8
asyncMemory load addr = res
  where
    res = register 0 (mux load inst res)
    inst = asyncRomFilePow2 "program.hex" <$> addr

memory'' :: (HiddenClockResetEnable dom) => Signal dom (Unsigned 4) -> Signal dom Word8
memory'' addr = blockRamPow2 prog addr (pure Nothing)
  where
    prog =
      0b0000_1101
        :> 0b0001_1110
        :> 0b0010_1111
        :> 0b1111_0000
        :> 0b0000_0000
        :> 0b0000_0000
        :> 0b0000_0000
        :> 0b0000_0000
        :> 0b0000_0000
        :> 0b0000_0000
        :> 0b0000_0000
        :> 0b0000_0000
        :> 0b0000_0000
        :> 0b0000_0011
        :> 0b0000_0100
        :> 0b0000_0010
        :> Nil

topEntity ::
  "CLK" ::: Clock System ->
  "RST" ::: Reset System ->
  "EN" :::Enable System ->
  "LOAD" ::: Signal System Bool ->
  "BUS" ::: Signal System Word8 ->
  "OUT" ::: Signal System Word8
topEntity clk rst en = withClockResetEnable clk rst en memory'''

makeTopEntity 'topEntity
