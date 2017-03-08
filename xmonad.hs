{-# LANGUAGE ScopedTypeVariables #-}
-- XMonad setup for debian
-- Richard Riley
-- rileyrg@gmail.com

-- {-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import           XMonad

import           Data.List

import           System.IO
import           System.Exit
    
import           XMonad.Actions.CycleWindows         (rotFocusedDown,
                                                      rotFocusedUp)
import           XMonad.Actions.CycleWS
import           XMonad.Actions.RotSlaves

import qualified XMonad.Actions.Search               as S
import qualified XMonad.Actions.Submap               as SM

-- import           XMonad.Actions.UpdatePointer
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
import           XMonad.Layout.PerWorkspace 
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.Reflect
import           XMonad.Prompt
import           XMonad.Prompt.AppLauncher           as AL
import           XMonad.Prompt.Ssh
import           XMonad.Util.EZConfig
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Run
import           XMonad.Util.SpawnOnce

import System.Directory


-- import XMonad.Hooks.DebugStack

myWorkSpaces :: [String]
myWorkSpaces = ["1:General", "2:Gimp" ,"3:IDE","4:Wireshark","5:**","9:TV"]

myManageHook :: ManageHook
myManageHook =  composeAll . concat $
                               [
                               [fmap(i `isPrefixOf`) resource --> doIgnore  | i <- myIgnores]
                               ,[fmap(f `isPrefixOf`) resource --> doCenterFloat   | f <- myFloats]
                               ,[
                                 className =? "SpiderOak"  --> doShift "2:Gimp"
                               , className =? "AfterShot 3"  --> doShift "2:Gimp"
                               , className =? "dvd"  --> doShift "2:Gimp"
                               , className =? "Gimp"  --> doShift "2:Gimp"
                               , className =? "Blender"  --> doShift "3:IDE"
                               , className =? "QtCreator"  --> doShift "3:IDE"
                               , className =? "Android SDK Manager"  --> doShift "2:General"
                               , className =? "jetbrains-idea-ce"  --> doShift "3:IDE"
                               , className =? "Eclipse"  --> doShift "3:IDE" 
                               , className =? "Wireshark"  --> doShift "4:Wireshark"
                               ,isDialog  --> doCenterFloat
                                ]
                               ]

    where
      myFloats        = ["xvidcap","pinentry"]
      myIgnores       = ["xx","xxx"]

myLayout =
    onWorkspace "1:General" (avoidStruts $ mkToggle(FULL??EOT) (Tall 1 (3/100) (1/2)))  $
    -- onWorkspace "2:Gimp"  (noBorders (fullscreenFull  Full )) $
    -- onWorkspace "3:IDE"  (noBorders (fullscreenFull  Full )) $
    -- onWorkspace "4:Wireshark"   (noBorders (fullscreenFull  Full )) $
    -- onWorkspace "5:Blender"   (noBorders (fullscreenFull  Full )) $
    avoidStruts $ mkToggle (single REFLECTY) $ mkToggle (single REFLECTX) $ mkToggle (single MIRROR) $ mkToggle(FULL??EOT) (Tall 1 (3/100) (1/2) ||| dragPane Horizontal 0.1 0.5||| Grid ||| Circle )

myTerminal :: String
myTerminal = "urxvt"
scratchpads :: [NamedScratchpad]
scratchpads = [
     NS "9patchresizer" "9patchresizer.sh" (className =?"9Patch Resizer") nonFloating,
     NS "texturepacker" "TexturePackerGUI" (className =? "TexturePacker") nonFloating ,
     NS "phpstorm" "phpstorm" (className =? "jetbrains-phpstorm") nonFloating ,
     NS "rube" "rube" (className =? "Rube.bin") nonFloating ,
     NS "androidemulator" "emulator -avd default" (className =?"emulator64-arm") nonFloating,
     NS "blender" "~/thirdparty/blender/blender" (className =? "Blender") nonFloating,
     NS "particleeditor" "~/bin/particleeditor.sh" (title =? "Particle Editor") nonFloating,
     NS "groovyconsole" "groovyConsole" (className =? "org-codehaus-groovy-tools-GroovyStarter") nonFloating,
     NS "chrome" "google-chrome --new-window" (className =? "Google-chrome-unstable") nonFloating ,
     NS "qtcreator" "qtcreator" (className =? "QtCreator") nonFloating ,
     NS "conky" "conky -c ${HOME}/.conkyrc -q" (className =? "Conky") doFloat ,
     NS "emacs" "myemacsclient" (className =? "Emacs") nonFloating,
--     NS "physed" "physed.sh" (title =? "Physics Body Editor") nonFloating , 
     NS "gimp" "gimp" (className =? "Gimp") nonFloating ,
     NS "firefox" "firefox" (className =? "Firefox-esr") nonFloating ,
     NS "torbrowser" "torbrowser-launcher" (className =? "Tor Browser") nonFloating ,
     NS "htop" "urxvt -name htop -title htop -e htop" (title =? "htop") nonFloating,
     NS "inkscape" "inkscape" (className =?"Inkscape") nonFloating,
     NS "intellij" "idea.sh" (className =?"jetbrains-idea-ce") nonFloating,
     NS "filemanager" "pcmanfm" (className =?"Pcmanfm") nonFloating,
     NS "rhythmbox" "rhythmbox" (className =? "Rhythmbox") nonFloating ,
     NS "terminal" "mytoggleterm" (appName =? "terminal") nonFloating,
     NS "pdf" "acroread" (className =? "Acroread-en") nonFloating,
     NS "tiled" "tiled" (className =? "Tiled") nonFloating,
     NS "unison" "unison" (title =? "Unison") nonFloating,
     NS "wireshark" "wireshark" (className =? "Wireshark") nonFloating ,
     NS "pavucontrol" "pavucontrol" (title =? "Volume Control") nonFloating ,
     NS "aftershotpro" "AfterShot3X64" (className =? "AfterShot Pro 3") nonFloating ,
     NS "dvd" "smplayer" (className =? "smplayer") nonFloating ,
     NS "xfe" "xfe" (className =? "Xfe") nonFloating ,
     NS "audacity" "audacity" (className =? "Audacity") nonFloating ,
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
             ,("M-c",  kill)
             ,("M-<F1>", AL.launchApp def "urxvt -e info " )
             ,("M-S-a",namedScratchpadAction scratchpads "aftershotpro")
             ,("M-S-b",namedScratchpadAction scratchpads "blender")
             ,("M-S-d",namedScratchpadAction scratchpads "dvd")
             ,("M-S-e",namedScratchpadAction scratchpads "emacs")
             -- ,("M-S-f",namedScratchpadAction scratchpads "firefox")
             ,("M-S-f",namedScratchpadAction scratchpads "chrome")
             ,("M-S-g",namedScratchpadAction scratchpads "gimp")
             ,("M-S-i",namedScratchpadAction scratchpads "intellij")
             ,("M-S-k",namedScratchpadAction scratchpads "inkscape")
             ,("M-S-r",namedScratchpadAction scratchpads "pdf")
             ,("M-S-s",namedScratchpadAction scratchpads "unison")
             ,("M-S-p",namedScratchpadAction scratchpads "phpstorm")
             ,("M-S-q",namedScratchpadAction scratchpads "qtcreator")
             ,("M-S-t",namedScratchpadAction scratchpads "tiled")
             ,("M-S-u",namedScratchpadAction scratchpads "rube")
             ,("M-S-w",namedScratchpadAction scratchpads "wireshark")
             ,("M-S-x",namedScratchpadAction scratchpads "xfe")
             ,("M-S-y",namedScratchpadAction scratchpads "audacity")

             -- sys type apps

             ,("M-C-a",namedScratchpadAction scratchpads "xfce4-appfinder") 
             ,("M-C-c",namedScratchpadAction scratchpads "conky")
             ,("M-C-h",sshPrompt def)
             ,("M-C-f",namedScratchpadAction scratchpads "filemanager")
             ,("M-C-S-f",spawn "pcmanfm")
             ,("M-C-p",namedScratchpadAction scratchpads "htop")
             ,("M-C-s",spawn "screenshot")
             ,("M-C-t",namedScratchpadAction scratchpads "terminal")
             ,("M-C-x",spawn "xinfoatmouse")
             ,("M-C-v",spawn "pavucontrol")

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
             ,("M-x", sendMessage $ Toggle MIRROR)
             ,("M-f", sendMessage $ Toggle FULL)
             ,("M-y", sendMessage $ Toggle REFLECTX)
             ,("M-b", sendMessage ToggleStruts)

             -- session management

             -- ,("M-C-f", withFocused float)
             ,("M-C-<Delete>", spawn "xkill")
             ,("M-S-<Delete>", kill)
             ,("M-C-l", spawn "xscreensaver-command -l")
             -- Quit xmonad
             ,("M-C-z", spawn "confirmtoquit.sh --suspend")
             ,("M-C-d", spawn "confirmtoquit.sh --shutdown")
             ,("M-C-q", io exitSuccess)
             ,("M-C-r", spawn "confirmtoquit.sh --restart")
             ]


myStartUpHook :: X ()


myStartUpHook  =   do
  useWicd <- io $ doesFileExist "/usr/bin/wicd-gtk"
  setWMName "LG3D" -- Java progs need this a jdk 1.6+
  mapM_ spawnOnce ([ if c then (if b>0 then "sleep "++ show b ++" && " else [])  ++ a else [] |(a,b,c)<-myStartupSpawns]++["sleep 2 && wicd-gtk --tray"|useWicd])

myStartupSpawns :: [(String, Integer, Bool)]
myStartupSpawns = [
      -- ("stalonetray",0,True)
      -- ("xfce4-power-manager",2,True) 
      -- ,("SpiderOak",2,True)
      -- ,("dropbox start -i",2,True)
      ]

myFadeHook :: Query Opacity  
myFadeHook = composeAll [
              isUnfocused --> transparency 0.2
              ,opaque
             ]

myConfig p =  def {
  manageHook = myManageHook <+> namedScratchpadManageHook scratchpads <+> manageDocks
  ,logHook = fadeWindowsLogHook myFadeHook <+> dynamicLogWithPP xmobarPP {             
    ppOutput = hPutStrLn p
   }  <+> ewmhDesktopsLogHook
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

