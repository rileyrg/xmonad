{-# LANGUAGE ScopedTypeVariables #-}

-- XMonad setup for debian
-- rileyrg@gmail.com

import           XMonad

import           Data.List

import           System.IO

import           XMonad.Actions.CycleWindows         (rotFocusedDown,
                                                      rotFocusedUp)
import           XMonad.Actions.CycleWS
import           XMonad.Actions.RotSlaves

import qualified XMonad.Actions.Search               as S
import qualified XMonad.Actions.Submap               as SM

import           XMonad.Actions.UpdatePointer
import           XMonad.Actions.WindowBringer        ()
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops           (ewmhDesktopsLogHook)
import           XMonad.Hooks.FadeWindows
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.SetWMName
import           XMonad.Layout.Circle
import           XMonad.Layout.DragPane
import           XMonad.Layout.Grid
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.Reflect
import           XMonad.Prompt
import           XMonad.Prompt.AppLauncher           as AL
import           XMonad.Prompt.Ssh
import           XMonad.Util.EZConfig
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Run
import           XMonad.Util.SpawnOnce

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
                                ]
                               ]

    where
      myFloats        = ["xvidcap","pinentry"]
      myIgnores       = ["xx","xxx"]

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
     NS "conky" "conky -c ${HOME}/.conky/conkyrc -q" (className =? "Conky") doFloat ,
     NS "emacs" "edit -c" (className =? "Emacs") nonFloating,
     NS "evince" "evince" (className =? "evince") nonFloating ,
     NS "gimp" "gimp" (className =? "Gimp") nonFloating ,
     NS "firefox" "firefox" (className =? "Firefox") nonFloating ,
     NS "htop" "urxvt -name htop -title htop -e htop" (title =? "htop") nonFloating,
     NS "intellij" "intellij" (className =?"jetbrains-idea-ce") nonFloating,
     NS "pcmanfm" "pcmanfm" (appName =?"pcmanfm") nonFloating,
     NS "rhythmbox" "rhythmbox" (className =? "Rhythmbox") nonFloating ,
     NS "terminal" "mytoggleterm" (appName =? "terminal") nonFloating,
     NS "wireshark" "wireshark" (className =? "Wireshark") nonFloating ,
     NS "xfce4-appfinder" "xfce4-appfinder" (className =? "Xfce4-appfinder") nonFloating
 ]

smap  m = mkKeymap (myConfig undefined)
                         [("g", m S.google)
                         ,("h", m S.hoogle)
                         ,("w", m S.wikipedia)
                         ,("i", m S.imdb)
                         ,("a", m S.amazon)
                         ,("m", m S.maps)
                         ,("y", m S.youtube)
                         ,("h", m hayoo)
                         ]
    where
      hayoo = S.searchEngine "Hayoo"  "http://holumbus.fh-wedel.de/hayoo/hayoo.html?query="

myKeys :: [(String, X ())]
myKeys= [
              ("<Print>", spawn "fullscreenshot")
             ,("M-s", SM.submap $ smap $ S.promptSearchBrowser greenXPConfig "firefox")
             ,("M-g",  S.promptSearchBrowser greenXPConfig "firefox" S.google)
             ,("M-<F1>", AL.launchApp def "gnome-terminal -x info " )
             ,("M-S-2",namedScratchpadAction scratchpads "9patchresizer")
             ,("M-S-c",namedScratchpadAction scratchpads "chrome")
             ,("M-S-d",namedScratchpadAction scratchpads "evince")
             ,("M-S-e",namedScratchpadAction scratchpads "emacs")
             ,("M-S-f",namedScratchpadAction scratchpads "firefox")
             ,("M-S-g",namedScratchpadAction scratchpads "gimp")
             ,("M-S-i",namedScratchpadAction scratchpads "intellij")
             ,("M-S-w",namedScratchpadAction scratchpads "wireshark")

             -- sys type apps

             ,("M-C-a",namedScratchpadAction scratchpads "xfce4-appfinder")
             ,("M-C-c",namedScratchpadAction scratchpads "conky")
             ,("M-C-h",sshPrompt def)
             ,("M-C-n",namedScratchpadAction scratchpads "pcmanfm")
             ,("M-C-p",namedScratchpadAction scratchpads "htop")
             ,("M-C-s",spawn "screenshot")
             ,("M-C-t",namedScratchpadAction scratchpads "terminal")
             ,("M-C-x",spawn "xinfoatmouse")

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
             ,("M-C-l", spawn "xscreensaver-command -l")
             -- Quit xmonad
             ,("M-C-d", spawn "confirmtoquit --shutdown")
             ,("M-C-q", spawn "confirmtoquit")
             ,("M-C-r", spawn "confirmtoquit --restart")
             ,("M-C-v", spawn "gnome-volume-control")
             ]

myStartUpHook :: X ()
myStartUpHook  =   do
  setWMName "LG3D" -- Java progs need this a jdk 1.6+
  spawnOnce "xcompmgr"
  spawnOnce "feh --bg-fill ${WALLPAPER}"
  spawnOnce "dropbox start -i"
  spawnOnce "xscreensaver -no-splash"
  spawnOnce "sleep 1 && wicd-client --tray"
  spawnOnce "sleep 1 &&  stalonetray"

myFadeHook = composeAll [
              isUnfocused --> transparency 0.2
              ,opaque
             ]

myConfig p =  def {
             manageHook = myManageHook <+> namedScratchpadManageHook scratchpads <+> manageDocks
           ,logHook = fadeWindowsLogHook myFadeHook <+> updatePointer (0.5, 0.5) (0, 0) <+> dynamicLogWithPP xmobarPP {
--           ,logHook = fadeWindowsLogHook myFadeHook <+>  updatePointer (Relative 0.5 0.5)  <+> dynamicLogWithPP xmobarPP {
             ppOutput = hPutStrLn p
             ,ppOrder               = \(ws:l:t:_)   -> [ws]
             }  <+> ewmhDesktopsLogHook -- <+> debugStackLogHook
-- <+> debugStackLogHook
           ,handleEventHook = fadeWindowsEventHook
           ,layoutHook = myLayout
           ,startupHook = myStartUpHook
           ,workspaces= myWorkSpaces
           ,modMask = mod4Mask
           ,focusFollowsMouse = False
           ,terminal =  myTerminal
           }

main :: IO ()
main =  do
  xm <- spawnPipe "xmobar"
  xmonad $ myConfig xm
             `additionalKeysP` myKeys

