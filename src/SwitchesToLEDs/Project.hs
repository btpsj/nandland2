module SwitchesToLEDs.Project where

import Clash.Annotations.TH
import Clash.Prelude

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
