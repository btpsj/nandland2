module LightingWithLogic.Project where

import Clash.Annotations.TH
import Clash.Prelude

topEntity ::
  "i_Switch"
    ::: ( "1" ::: Signal System Bit,
          "2" ::: Signal System Bit
        ) ->
  "o_LED_1" ::: Signal System Bit
topEntity (s1, s2) = (.&.) <$> s1 <*> s2

makeTopEntity 'topEntity
