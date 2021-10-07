import XMonad

import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import Xmonad.Layout.ThreeColumns


main :: IO ()
main = xmonad $ def
    { modMask = mod4Mask
    , terminal = "terminator"
    }
    `additionalKeysP`
    [   ("M-S-z", spawn "i3lock -c 000000"  )
    ,   ("M-]"  , spawn "firefox"           )
    ,   ("M-d"  , spawn "dmenu_run"         )       
    ]


myLayout = tiled ||| Mirror tilted ||| Full
    where
        tiled = Tell nmaster delta ration
        nmaster = 1  --default number of windows int he master paine
        ratio = 1/2  -- defa
        delta = 3/1000
