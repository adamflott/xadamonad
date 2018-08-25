module XMonad.Wallpaper where

import XMonad.Core
import XMonad.Wallpaper.Find
import XMonad.Wallpaper.Expand

import Control.Applicative
import System.Random

{- |
Example usage: (~/.xmonad/xmonad.hs)

> import XMonad
> import XMonad.Config.Desktop
> import XMonad.Wallpaper
> main = do
>     setRandomWallpaper ["paths-of-your choice", "$HOME/Pictures/Wallpapers"]
>     xmonad $ desktopConfig
>        { terminal    = "urxvt"
>        , modMask     = mod4Mask
>        }

paths will be expanded using environment variables, and paths are not exist will be ignored during scan phase.

For more information about path expansion, see also 'expand'.

-}

setRandomWallpaper filepaths = do
    rootPaths  <- mapM expand filepaths
    candidates <- findImages rootPaths
    wallpaper  <- ((!!) candidates) <$> getStdRandom (randomR (0, length candidates - 1)) 
    spawn $ "feh --bg-scale " ++ wallpaper
