{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Blinking.Project where

import Clash.Prelude
import Data.Either()
import Data.Maybe()
import RetroClash.Clock (ClockDivider, Milliseconds)
import RetroClash.Utils (succIdx)
import Clash.Annotations.TH (makeTopEntity)

-- Change this to the raw clock rate of the FPGA board you are targeting
createDomain vSystem {vName = "Dom100", vPeriod = hzToPeriod 100_000_000}

data OnOff on off
  = On (Index on)
  | Off (Index off)
  deriving (Generic, NFDataX)

isOn :: OnOff on off -> Bool
isOn On {} = True
isOn Off {} = False

countOnOff :: (KnownNat on, KnownNat off) => OnOff on off -> OnOff on off
countOnOff (On x) = maybe (Off 0) On $ succIdx x
countOnOff (Off y) = maybe (On 0) Off $ succIdx y


blinkingSecond ::
  forall dom.
  (HiddenClockResetEnable dom, _) =>
  Signal dom Bit
blinkingSecond = boolToBit . isOn <$> r
  where
    r ::
      Signal
        dom
        ( OnOff
            (ClockDivider dom (Milliseconds 500))
            (ClockDivider dom (Milliseconds 500))
        )
    r = register (Off 0) $ countOnOff <$> r

withResetEnableGen
    :: (KnownDomain dom)
    => (HiddenClockResetEnable dom => r)
    -> Clock dom -> r
withResetEnableGen board clk =
    withClockResetEnable clk resetGen enableGen board

topEntity ::
  "i_Clk" ::: Clock Dom100 ->
  "o_LED_1" ::: Signal Dom100 Bit
topEntity = withResetEnableGen blinkingSecond

makeTopEntity 'topEntity
