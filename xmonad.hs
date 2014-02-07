{-# LANGUAGE ScopedTypeVariables #-}

-- XMonad setup for debian 
-- rileyrg@gmail.com

import XMonad

import Data.List
import System.Exit

import System.IO

import qualified XMonad.StackSet as W

import XMonad.Actions.CycleWS
import XMonad.Actions.CycleWindows  (rotFocusedUp, rotFocusedDown)
import XMonad.Actions.RotSlaves

import qualified XMonad.Actions.Submap as SM
import qualified XMonad.Actions.Search as S
import XMonad.Actions.WindowBringer()
import qualified Data.Map as M
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops (ewmhDesktopsLogHook)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout.Circle
import XMonad.Layout.DragPane
import XMonad.Layout.Grid
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Reflect
import XMonad.Prompt
import XMonad.Prompt.AppLauncher as AL
import XMonad.Prompt.Ssh
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Util.NamedScratchpad

-- import XMonad.Hooks.DebugStack

myWorkSpaces :: [String]
myWorkSpaces = ["1:Desktop", "2:TV" ,"3:IDE","4:Wireshark"]

myManageHook :: ManageHook
myManageHook =  composeAll . concat $
                               [ 
                               [fmap(i `isPrefixOf`) resource --> doIgnore  | i <- myIgnores]
                               ,[fmap(f `isPrefixOf`) resource --> doCenterFloat   | f <- myFloats]
                               ,[ 
                                 className =? "jetbrains-idea-ce"  --> doShift "3:IDE"
                               , className =? "Eclipse"  --> doShift "3:IDE"
                               , className =? "Wireshark"  --> doShift "4:Wireshark"
                               ,isDialog  --> doCenterFloat
                               -- ,isFullscreen  --> doFullFloat
                                ] 
                               ]

    where
      myFloats        = ["gimmix","xvidcap", "Xvidcap", "recordMyDesktop","screensaver","pinentry-gtk-2","key-mon","xsane"]
      myIgnores       = ["Gnome-panel","gworldclock","ddd", "bsh-Interpreter","Do","gnubiff","Audacious"]

myLayout = 
    -- onWorkspace "2:TV"   (noBorders (fullscreenFull  Full )) $
    -- onWorkspace "3:IDE"  (noBorders (fullscreenFull  Full )) $
    -- onWorkspace "4:Wireshark"   (noBorders (fullscreenFull  Full )) $
    avoidStruts $ mkToggle (single REFLECTY) $ mkToggle (single REFLECTX) $ mkToggle (single MIRROR) $ mkToggle(FULL??EOT) (Tall 1 (3/100) (1/2) ||| dragPane Horizontal 0.1 0.5||| Grid ||| Circle )

myTerminal :: String
myTerminal = "urxvt"

scratchpads :: [NamedScratchpad]
scratchpads = [

     NS "9patchresizer" "9patchresizer" (className =?"9Patch Resizer") nonFloating,
     NS "androidemulator" "emulator -avd default" (className =?"emulator64-arm") nonFloating,
     NS "chrome" "google-chrome --new-window" (className =? "Google-chrome") nonFloating ,
     NS "emacs" "edit -c" (className =? "Emacs") nonFloating,
     NS "evince" "evince" (className =? "evince") nonFloating ,
     NS "gimp" "gimp" (className =? "Gimp") nonFloating ,
     NS "firefox" "firefox" (className =? "Firefox") nonFloating ,
     NS "htop" "urxvt -name urxvt -title htop -e sh -c htop" (title =? "htop") nonFloating,
     NS "intellij" "intellij" (className =?"jetbrains-idea-ce") nonFloating,
     NS "pcmanfm" "pcmanfm" (appName =?"pcmanfm") nonFloating,
     NS "rhythmbox" "rhythmbox" (className =? "Rhythmbox") nonFloating ,
     NS "terminal" "mytoggleterm" (appName =? "terminal") nonFloating,
     NS "tint2" "tint2 -c ${HOME}/.xmonad/.tintrc && sleep 5 && killall tint2" (appName =? "tint2") nonFloating,
     NS "wireshark" "wireshark" (className =? "Wireshark") nonFloating ,
     NS "xfce4-appfinder" "xfce4-appfinder" (className =? "Xfce4-appfinder") nonFloating
 ]

-- searchEngineMap method = M.fromList
--                            [("g", method S.google)
--                            ,("h", method S.hoogle)
--                            ,("w", method S.wikipedia)
--                            ,("i", method S.imdb)
--                            ,("a", method S.amazon)
--                            ,("m", method S.maps)
--                            ,("y", method S.youtube)
--                            ,("h", method hayoo)
--                            ]
--       where
--         hayoo = S.searchEngine "Hayoo"  "http://holumbus.fh-wedel.de/hayoo/hayoo.html?query="

searchEngineMap method = M.fromList
                           [((0, xK_g), method S.google)
                           ,((0, xK_H), method S.hoogle)
                           ,((0, xK_w), method S.wikipedia)
                           ,((0, xK_i), method S.imdb)
                           ,((0, xK_a), method S.amazon)
                           ,((0, xK_m), method S.maps)
                           ,((0, xK_y), method S.youtube)
                           ,((0, xK_h), method hayoo)
                           ]
      where
        hayoo = S.searchEngine "Hayoo"  "http://holumbus.fh-wedel.de/hayoo/hayoo.html?query="

myKeys :: [(String, X ())]
myKeys= [
              ("<Print>", spawn "fullscreenshot")
             ,("M-s", SM.submap $ searchEngineMap $ S.promptSearchBrowser greenXPConfig "firefox")
             ,("M-g",  S.promptSearchBrowser greenXPConfig "firefox" S.google)
             ,("M-<F1>", AL.launchApp defaultXPConfig "gnome-terminal -x info " )
             ,("M-S-2",namedScratchpadAction scratchpads "9patchresizer")
             ,("M-S-c",namedScratchpadAction scratchpads "chrome")
             ,("M-S-d",namedScratchpadAction scratchpads "evince")
             ,("M-S-e",namedScratchpadAction scratchpads "emacs")
             ,("M-S-f",namedScratchpadAction scratchpads "firefox")
             ,("M-S-g",namedScratchpadAction scratchpads "gimp")
             ,("M-S-i",namedScratchpadAction scratchpads "intellij")
             ,("M-S-w",namedScratchpadAction scratchpads "wireshark")
             ,("M-C-2",namedScratchpadAction scratchpads "tint2")
             ,("M-C-a",namedScratchpadAction scratchpads "xfce4-appfinder")
             ,("M-C-c",namedScratchpadAction scratchpads "conky")
             ,("M-C-h",sshPrompt defaultXPConfig)
             ,("M-C-n",namedScratchpadAction scratchpads "pcmanfm")
             ,("M-C-p",namedScratchpadAction scratchpads "htop")
             ,("M-C-s",spawn "screenshot")
             ,("M-C-t",namedScratchpadAction scratchpads "terminal")
             ,("M-C-x",spawn "gxmessage -wrap -bg black -fg green `xprop | grep WM_`")
             
             -- workspace management
             
             ,("M-<Down>",  nextWS)
             ,("M-<Up>",    prevWS)
             ,("M-C-<Up>",  shiftToNext)
             ,("M-C-<Down>",    shiftToPrev)
             ,("M-<Right>", nextScreen)
             ,("M-<Left>",  prevScreen)
             ,("M-C-<Right>", shiftNextScreen)
             ,("M-C-<Left>", shiftPrevScreen)
             ,("M-j",  rotSlavesDown)
             ,("M-k",  rotSlavesUp)
             ,("M-u",  rotFocusedDown)
             ,("M-i",  rotFocusedUp)
             ,("M-C-z",     toggleWS)
             ,("M-x", sendMessage $ Toggle MIRROR)
             ,("M-f", sendMessage $ Toggle FULL)
             ,("M-y", sendMessage $ Toggle REFLECTX)
             ,("M-b", sendMessage ToggleStruts)
             
             -- session management
             
             ,("M-C-f", withFocused float)
             ,("M-C-<Delete>", spawn "xkill")
             ,("M-S-<Delete>", kill)
             ,("M-C-s", spawn "gksu poweroff")
             ,("M-C-r", spawn "gksu reboot")
             ,("M-C-l", spawn "xscreensaver-command -l")
             ,("M-C-q", spawn "kill -9 -1")
             
             -- Quit xmonad
             ,("M-S-q", io exitSuccess)
             ,("M-C-v", spawn "gnome-volume-control")
             ,("M-C-o", spawn "pkill -u ${USER}")
             ]
                
myStartUpHook :: X ()
myStartUpHook  =   do
  setWMName "LG3D" -- Java progs need this a jdk 1.6+
  spawnOnce "xcompmgr -c"
  spawnOnce "dbus-launch --session-exit"
  spawnOnce "nm-applet --sm-disable"
  spawnOnce "feh --bg-fill ${WALLPAPER}"
  spawnOnce "xscreensaver -no-splash"
  spawnOnce "dropbox start -i"

myConfig p =  defaultConfig {
             manageHook =  namedScratchpadManageHook scratchpads <+> manageDocks <+>  myManageHook
           ,logHook =  dynamicLogWithPP xmobarPP {ppOutput = hPutStrLn p}  <+> ewmhDesktopsLogHook -- <+> debugStackLogHook
           ,layoutHook = myLayout
           , startupHook = myStartUpHook
           , workspaces= myWorkSpaces
           , modMask = mod4Mask
           , focusFollowsMouse = False
           , terminal =  myTerminal
           } 

main :: IO ()
main =  do 
  xm <- spawnPipe "xmobar"
  xmonad $ myConfig xm 
             `additionalKeysP` myKeys

