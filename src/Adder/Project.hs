module Adder.Project where

import Clash.Annotations.TH
import Clash.Prelude

halfAdder :: Bit -> Bit -> (Bit, Bit)
halfAdder a b = (s, c)
  where
    s = a `xor` b
    c = a .&. b

fullAdder :: Bit -> Bit -> Bit -> (Bit, Bit)
fullAdder a b cin = (s, cout)
  where
    (s', c) = halfAdder a b
    (s, cout') = halfAdder s' cin
    cout = c .|. cout'

topEntity ::
  "i_Switch"
    ::: ( "1" ::: Signal System Bit,
          "2" ::: Signal System Bit
        ) ->
  "o_LED"
    ::: ( "1" ::: Signal System Bit,
          "2" ::: Signal System Bit
        )
topEntity (a, b) = (s, c)
  where
    (s, c) = unbundle $ halfAdder <$> a <*> b

makeTopEntity 'topEntity
