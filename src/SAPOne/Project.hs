{-# LANGUAGE NumericUnderscores #-}

module SAPOne.Project where

import Clash.Annotations.TH
import Clash.Prelude
import qualified Data.Text.IO as T

type Word8 = BitVector 8

type Addr = BitVector 4

data Instruction
  = LDA Addr
  | ADD Addr
  | SUB Addr
  | OUT
  | HLT
  deriving (Show)

parseInstruction :: Word8 -> Instruction
parseInstruction input = inst
  where
    rest = resize $ input .&. 0b00001111
    inst =
      case (input .&. 0b11110000) `shiftR` 4 of
        0x0 -> LDA rest
        0x1 -> ADD rest
        0x2 -> SUB rest
        0xE -> OUT
        0xF -> HLT
        _ -> HLT

pc :: (HiddenClockResetEnable dom) => Signal dom Bool -> Signal dom Word8
pc inc = output
  where
    output = regEn 0 inc (output + 1)

sapRegister :: (HiddenClockResetEnable dom) => Signal dom Bool -> Signal dom Word8 -> Signal dom Word8
sapRegister = regEn 0

regA :: (HiddenClockResetEnable dom) => Signal dom Bool -> Signal dom Word8 -> Signal dom Word8
regA = sapRegister

regB :: (HiddenClockResetEnable dom) => Signal dom Bool -> Signal dom Word8 -> Signal dom Word8
regB = sapRegister

regIr :: (HiddenClockResetEnable dom) => Signal dom Bool -> Signal dom Word8 -> Signal dom Word8
regIr = sapRegister

alu :: Signal dom Bool -> Signal dom Word8 -> Signal dom Word8 -> Signal dom Word8
alu en a b = alu' <$> en <*> a <*> b
  where
    alu' :: Bool -> Word8 -> Word8 -> Word8
    alu' enableSub a' b'
      | enableSub = a' - b'
      | otherwise = a' + b'

busMux ::
  (HiddenClockResetEnable dom) =>
  Signal dom Word8 -> -- Adder
  Signal dom Word8 -> -- A Reg
  Signal dom Word8 -> -- Instruction
  Signal dom Word8 -> -- Memory
  Signal dom Word8 -> -- Program Counter
  Signal dom (BitVector 5) ->
  Signal dom Word8
busMux addrOut aOut irOut memOut pcOut busSel = register 0 busMux'
  where
    busMux' =
      mux (busSel .==. 0b00001) addrOut
        $ mux (busSel .==. 0b00010) aOut
        $ mux (busSel .==. 0b00100) irOut
        $ mux (busSel .==. 0b01000) memOut
        $ mux (busSel .==. 0b10000) pcOut 0

busPack :: (HiddenClockResetEnable dom) => Signal dom Bool -> Signal dom Bool -> Signal dom Bool -> Signal dom Bool -> Signal dom Bool -> Signal dom (BitVector 5)
busPack a b c d e = pack <$> bundle (a :> b :> c :> d :> e :> Nil)

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

skipFirst :: (HiddenClockResetEnable dom) => Signal dom Word8 -> Signal dom Word8
skipFirst = mealy skipFirst' False
  where
    skipFirst' st i
      | not st = (True, 0)
      | otherwise = (True, i)

data ControlSignal
  = Hlt
  | PcInc
  | PcEn
  | MemLoad
  | MemEn
  | IrLoad
  | IrEn
  | ALoad
  | AEn
  | BLoad
  | AdderSub
  | AdderEn

ctrl :: ControlSignal -> Int
ctrl opcode = case opcode of
  Hlt -> 11
  PcInc -> 10
  PcEn -> 9
  MemLoad -> 8
  MemEn -> 7
  IrLoad -> 6
  IrEn -> 5
  ALoad -> 4
  AEn -> 3
  BLoad -> 2
  AdderSub -> 1
  AdderEn -> 0

type ControlWord = BitVector 12

type Stage = Index 6

type OpCode = BitVector 4

controller :: (HiddenClockResetEnable dom) => Signal dom OpCode -> Signal dom ControlWord
controller op = controller'' <$> stage <*> op
  where
    stage = traceSignal1 "stage" $ register (0 :: Stage) out
    out = satAdd SatWrap 1 <$> stage

controller'' :: Stage -> OpCode -> ControlWord
controller'' s o = case (s, o) of
  (0, _) -> 0 `setBit` ctrl PcEn `setBit` ctrl MemLoad
  (1, _) -> 0 `setBit` ctrl PcInc
  (2, _) -> 0 `setBit` ctrl MemEn `setBit` ctrl IrLoad
  (3, 0b0000) -> 0 `setBit` ctrl IrEn `setBit` ctrl MemLoad
  (3, 0b0001) -> 0 `setBit` ctrl IrEn `setBit` ctrl MemLoad
  (3, 0b0010) -> 0 `setBit` ctrl IrEn `setBit` ctrl MemLoad
  (3, 0b1111) -> 0 `setBit` ctrl Hlt
  (4, 0b0000) -> 0 `setBit` ctrl MemEn `setBit` ctrl ALoad
  (4, 0b0001) -> 0 `setBit` ctrl MemEn `setBit` ctrl BLoad
  (4, 0b0010) -> 0 `setBit` ctrl MemEn `setBit` ctrl BLoad
  (5, 0b0001) -> 0 `setBit` ctrl AdderEn `setBit` ctrl ALoad
  (5, 0b0010) -> 0 `setBit` ctrl AdderSub `setBit` ctrl AdderEn `setBit` ctrl ALoad
  _ -> 0

controller' :: Stage -> OpCode -> (Stage, ControlWord)
controller' 0 _ = (1, 0 `setBit` ctrl PcEn `setBit` ctrl MemLoad)
controller' 1 _ = (2, 0 `setBit` ctrl PcInc)
controller' 2 _ = (3, 0 `setBit` ctrl MemEn `setBit` ctrl IrLoad)
controller' 3 opcode = case opcode of
  0b0000 -> (4, 0 `setBit` ctrl IrEn `setBit` ctrl MemLoad)
  0b0001 -> (4, 0 `setBit` ctrl IrEn `setBit` ctrl MemLoad)
  0b0010 -> (4, 0 `setBit` ctrl IrEn `setBit` ctrl MemLoad)
  0b1111 -> (4, 0 `setBit` ctrl Hlt)
  _ -> (4, 0)
controller' 4 opcode = case opcode of
  0b0000 -> (5, 0 `setBit` ctrl MemEn `setBit` ctrl ALoad)
  0b0001 -> (5, 0 `setBit` ctrl MemEn `setBit` ctrl BLoad)
  0b0010 -> (5, 0 `setBit` ctrl MemEn `setBit` ctrl BLoad)
  _ -> (5, 0)
controller' 5 opcode = case opcode of
  0b0001 -> (0, 0 `setBit` ctrl AdderEn `setBit` ctrl ALoad)
  0b0010 -> (0, 0 `setBit` ctrl AdderSub `setBit` ctrl AdderEn `setBit` ctrl ALoad)
  _ -> (0, 0)
controller' _ _ = (0, 0)

checkBit :: (Functor f, Bits a) => ControlSignal -> f a -> f Bool
checkBit inp word = flip testBit (ctrl inp) <$> word

cpu :: (HiddenClockResetEnable dom) => Signal dom Word8
cpu = bus
  where
    busEn = traceSignal1 "busEn" $ busPack pcEn memEn irEn aEn adderEn
    bus = traceSignal1 "bus" $ busMux addrOut aOut irOut memOut pcOut busEn
    addrOut = traceSignal1 "addrOut" $ alu adderSub aOut bOut
    aOut = traceSignal1 "regA" $ regA aLoad bus
    bOut = traceSignal1 "regB" $ regB bLoad bus
    irOut = traceSignal1 "regIR" $ regIr irLoad bus
    pcOut = traceSignal "pc" $ pc pcInc
    busUpper = traceSignal "busUpper" $ slice d7 d4 <$> bus
    memOut = traceSignal "mem" $ memory' memLoad bus

    ctrlWord = traceSignal "controller" $ controller busUpper
    pcInc = traceSignal "pcInc" $ checkBit PcInc ctrlWord
    adderSub = traceSignal "adderSub" $ checkBit AdderSub ctrlWord
    aLoad = traceSignal "aLoad" $ checkBit ALoad ctrlWord
    bLoad = traceSignal "bLoad" $ checkBit BLoad ctrlWord
    irLoad = traceSignal "irLoad" $ checkBit IrLoad ctrlWord
    memLoad = traceSignal "memLoad" $ checkBit MemLoad ctrlWord

    pcEn = traceSignal "pcEn" $ checkBit PcEn ctrlWord
    memEn = traceSignal "memEn" $ checkBit MemEn ctrlWord
    irEn = traceSignal "irEn" $ checkBit IrEn ctrlWord
    aEn = traceSignal "aEn" $ checkBit AEn ctrlWord
    adderEn = traceSignal "adderEn" $ checkBit AdderEn ctrlWord

cpu' :: Signal System Word8
cpu' = withClockResetEnable clockGen resetGen enableGen cpu

main :: IO ()
main = do
  vcd <-
    dumpVCD
      (1, 100)
      cpu'
      [ "busEn",
        "bus",
        "addrOut",
        "pcInc",
        "regA",
        "regB",
        "regIR",
        "mem",
        "pc"
      ]
  either (writeFile "test.vcd") (T.writeFile "test.vcd") vcd

topEntity ::
  "i_Switch"
    ::: ( "1" ::: Signal System Bit,
          "2" ::: Signal System Bit,
          "3" ::: Signal System Bit,
          "4" ::: Signal System Bit
        ) ->
  "o_LED"
    ::: ( "1" ::: Signal System Bit,
          "2" ::: Signal System Bit,
          "3" ::: Signal System Bit,
          "4" ::: Signal System Bit
        )
topEntity (s1, s2, s3, s4) = (s1, s2, s3, s4)

makeTopEntity 'topEntity
