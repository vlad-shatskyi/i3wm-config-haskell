module Languages.I4 where

import qualified Languages.I3 as I3
import DataTypes.Other

data I4 = I4 I3.I3
        | ModeDefinition ModeName [I4]
