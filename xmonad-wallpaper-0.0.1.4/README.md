### XMonad Wallpaper

#### Features

##### environment variable expansion

The following syntax are supported

```
$HOME

${HOME} 
```

##### Images are scaned using mimetypes rather than extensions

XMonad Wallpaper scans user supplied directories by detecting their mimetype rather than rely on extension 

#### Installation

```
cabal install xmonad-wallpaper
```

#### Example mini configuration using wallpaper

```
import XMonad
import XMonad.Config.Desktop
import XMonad.Wallpaper

main = do
    -- Randomly pick a wallpaper from 
    -- directories specified as wallpaper
    setupRandomWallpaper ["paths-of-your choice", "$HOME/Pictures/Wallpapers"]
    xmonad $ desktopConfig
       { terminal    = "urxvt"
       , modMask     = mod4Mask
       }

```
