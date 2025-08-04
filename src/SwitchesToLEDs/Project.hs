module SwitchesToLEDs.Project where

import Clash.Annotations.TH
import Clash.Prelude

topEntity ::
  "BTN" ::: Signal System Bit ->
  "LED" ::: Signal System Bit
topEntity = id

makeTopEntity 'topEntity
