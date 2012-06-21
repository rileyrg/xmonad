
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------
-- xmonad.hs
-- Richard G. Riley <rgr@richardriley.net>
-- Thanks to the #xmonad chaps
-- on freednode IRC for help and advice.
-------------------------------------------

import XMonad
import XMonad.Core

import Data.List

import qualified XMonad.StackSet as W

import Control.OldException
import Control.Monad
import DBus
import DBus.Connection
import DBus.Message


import XMonad.Actions.GridSelect 
import XMonad.Actions.MouseResize
import XMonad.Actions.CycleWS
import XMonad.Actions.CycleWindows  (rotFocusedUp, rotFocusedDown)
import XMonad.Actions.RotSlaves

import XMonad.Actions.PerWorkspaceKeys
import qualified XMonad.Actions.Submap as SM
import qualified XMonad.Actions.Search as S
import XMonad.Actions.WindowBringer
import XMonad.Actions.WindowGo
import qualified Data.Map as M
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout.Circle
import XMonad.Layout.DragPane
import XMonad.Layout.IM
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
-- import XMonad.Layout.Magnifier
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Monitor
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Reflect
import XMonad.Layout.SimpleFloat
import XMonad.Layout.WindowArranger
import XMonad.Prompt
import XMonad.Prompt.AppLauncher as AL
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import XMonad.Util.Run
import qualified XMonad.StackSet as Window
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Scratchpad
import XMonad.Util.NamedScratchpad

import System.Posix.Unistd (getSystemID, nodeName)
import System.Environment (getEnvironment)



-- import Graphics.X11.Xlib

myWorkSpaces    = ["1", "2:TV" ,"3","4","5","6","7:Eclipse","8:Gimp"]
myManageHook =  composeAll . concat $
                               [ 
                               [fmap(i `isPrefixOf`) resource --> doIgnore  | i <- myIgnores]
                               ,[fmap(f `isPrefixOf`) resource --> doCenterFloat   | f <- myFloats]
                               ,[ 
                                className =? "Gimp"  --> doShift "8:Gimp"
                               , className =? "SDL_App"  --> doShift "6:Android"
                               , className =? "screenkey"  --> doShift "6:Android"
                               , className =? "MPlayer"  --> doShift "2:TV"
                               ,isDialog  --> doCenterFloat
                               ,isFullscreen  --> doFullFloat
                                ] 
                               ]

    where
      myFloats        = ["gimmix","Gnome-system-monitor","Vncviewer", "Steam", "xvidcap", "Xvidcap", "recordMyDesktop","screensaver","gnome-panel","ktorrent","Ddd","GPicker","gpicker", "barrybackup","pinentry-gtk-2","recordMyDesktop","key-mon","xsane"]
      myIgnores       = ["gworldclock","ddd", "bsh-Interpreter","Do","gnubiff","Audacious"]
      role = stringProperty "WM_WINDOW_ROLE"



myLayout = smartBorders(
    onWorkspace "8:Gimp" (withIM 0.11 (Role "gimp-toolbox") $ reflectHoriz $ withIM 0.15 (Role "gimp-dock") Full) $
    -- onWorkspace "2:TV"   (smartBorders $ avoidStruts  Full ) $
    onWorkspace "2:TV"   (smartBorders   Full ) $
    avoidStruts $ mouseResize $ windowArrange $ smartBorders $ mkToggle (single REFLECTY) $ mkToggle (single REFLECTX) $ mkToggle (single MIRROR) $ mkToggle(FULL??EOT) (Tall 1 (3/100) (1/2) ||| dragPane Horizontal 0.1 0.5||| Grid ||| Circle ))    

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

myTerminal = "~/bin/myterm"
myToggleTerminal = "~/bin/mytoggleterm webs"

myFadeLogHook :: X ()
myFadeLogHook = fadeInactiveLogHook fadeAmount
      where fadeAmount = 0xaaaaaaaa

scratchpads = [

 -- run htop in xterm, find it by title, use default floating window placement

--     NS "google-chrome" "google-chrome" (className =? "Google-chrome") nonFloating ,
--     NS "browser" "conkeror" (className =? "Conkeror") nonFloating ,
     NS "chrome" "google-chrome --new-window" (className =? "Google-chrome") nonFloating ,
     NS "iceweasel" "iceweasel" (className =? "Iceweasel") nonFloating ,
     NS "facebook" "google-chrome --app=\"http://www.facebook.com\" --new-window --class=\"facebook\"" (className =? "facebook") nonFloating ,
     NS "evince" "evince" (className =? "evince") nonFloating ,
     NS "yoono" "yoono.sh" (name =? "Yoono") defaultFloating ,
     NS "audacious" "audacious" (className =? "Audacious") defaultFloating ,
     NS "emacs" "edit -c" (className =? "Emacs") nonFloating,
     NS "translate" "chromium-browser --app=http://translate.google.com/#" (name =? "Google Translate") nonFloating,
     NS "unison" "unison-gtk" (className =? "unison-gtk") nonFloating ,
     NS "htop" "xterm -name htop -e htop" (title =? "htop") 
         (customFloating $ W.RationalRect (1/3) (1/8) (2/3) (1/2)) ,
     NS "newsbeuter" "xterm -e myrss" (title =? "myrss")
         (customFloating $ W.RationalRect (1/2) 0 (1/2) (1/4)) 

 ] where name = stringProperty "WM_NAME"


isLightHost :: String -> Bool
isLightHost = (`elem` ["X30","PUB","Hermes"])

-- This retry is really awkward, but sometimes DBus won't let us get our
-- name unless we retry a couple times.
getWellKnownName :: Connection -> IO ()
getWellKnownName dbus = tryGetName `catchDyn` (\ (DBus.Error _ _) ->
                                                getWellKnownName dbus)
    where
      tryGetName = do
        namereq <- newMethodCall serviceDBus pathDBus interfaceDBus "RequestName"
        addArgs namereq [String "org.xmonad.Log", Word32 5]
        sendWithReplyAndBlock dbus namereq 0
        return ()

main = withConnection Session $ \ dbus -> do 

  putStrLn "Getting well-known name."
  getWellKnownName dbus
  putStrLn "Got name, starting XMonad."

  host   <- fmap  nodeName getSystemID

  xmonad myConfig
             { logHook    =  dynamicLogWithPP  defaultPP {
                   ppOutput   = \ str -> do
                     let str'  = "<span font=\"Terminus 9 Bold\">" ++ str ++ 
                                 "</span>"
                         str'' = sanitize str'
                     msg <- newSignal "/org/xmonad/Log" "org.xmonad.Log" 
                                "Update"
                     addArgs msg [String str'']
                     -- If the send fails, ignore it.
                     send dbus msg 0 `catchDyn`
                       (\ (DBus.Error _name _msg) ->
                         return 0)
                     return ()
                 , ppTitle    = pangoColor "#003366" . shorten 50
                 , ppCurrent  = pangoColor "#006666" . wrap "[" "]"
                 , ppVisible  = pangoColor "#663366" . wrap "(" ")"
                 , ppHidden   = wrap " " " "
                 , ppUrgent   = pangoColor "red"
                 } >> myFadeLogHook

             }

myConfig = gnomeConfig {
             manageHook =  namedScratchpadManageHook scratchpads <+> manageDocks <+> myManageHook
           ,layoutHook = myLayout
           , startupHook = startupHook gnomeConfig >> setWMName "LG3D"
           , workspaces= myWorkSpaces
           , modMask = mod4Mask
           , focusFollowsMouse = False
           , terminal =  myTerminal
           } `additionalKeys` [
        -- scrot is a nicely configurable screen shot utility
           ((0, xK_Print), spawn "scrot -s -d 5 -q50 'shot-%Y%m%d-%H.%M.%S.png' -e 'mv $f ~/tmp/' && nautilus ~/tmp")
           ,((modMask myConfig .|. controlMask, xK_p ), spawn "sleep 0.2;scrot -d2 -q50 -s 'shot-%Y%m%d-%H.%M.%S.png' -e 'mv $f ~/tmp && eog ~/tmp/$f'")
           ,((modMask myConfig, xK_b), bringMenu)
         --  ,((modMask myConfig.|. shiftMask, xK_g), goToSelected defaultGSConfig)
        
           ,((modMask myConfig, xK_s), SM.submap $ searchEngineMap $ S.promptSearchBrowser defaultXPConfig "chromium-browser")
           ,((modMask myConfig, xK_g),  S.promptSearch greenXPConfig S.google)
        
           ,((modMask myConfig, xK_p), shellPrompt defaultXPConfig)

           , ((modMask myConfig , xK_F1), AL.launchApp defaultXPConfig "gnome-terminal -x info " )
--           ,((modMask myConfig , xK_F1), manPrompt defaultXPConfig)

        
           ,((modMask myConfig .|. shiftMask, xK_o), spawn "ooffice -calc")
           ,((mod3Mask, xK_space), spawn "gnome-do")
           ,((modMask myConfig .|. shiftMask, xK_n), spawn "nautilus /home/shamrock")
           ,((modMask myConfig , xK_v),  spawn "conkeror -f unfocus")
           ,((modMask myConfig .|. shiftMask, xK_a),  namedScratchpadAction scratchpads "audacious")
           ,((modMask myConfig .|. shiftMask, xK_g),  namedScratchpadAction scratchpads "chrome")
           ,((modMask myConfig .|. shiftMask, xK_f),  namedScratchpadAction scratchpads "facebook")
           ,((modMask myConfig .|. shiftMask, xK_i),  namedScratchpadAction scratchpads "iceweasel")
           ,((modMask myConfig .|. shiftMask, xK_p),  namedScratchpadAction scratchpads "htop")
           ,((modMask myConfig .|. shiftMask, xK_m),  spawn "ssh hermes xterm -e htop")
           ,((modMask myConfig .|. shiftMask, xK_e),  namedScratchpadAction scratchpads "emacs")
           ,((modMask myConfig .|. shiftMask, xK_t), scratchpadSpawnActionCustom myToggleTerminal)
           ,((modMask myConfig .|. shiftMask, xK_d),  namedScratchpadAction scratchpads "evince")
           ,((modMask myConfig .|. shiftMask, xK_y),  namedScratchpadAction scratchpads "yoono")
        
           ,((modMask myConfig .|. controlMask, xK_x), spawn "gxmessage -wrap -bg black -fg green `xprop | grep WM_`") -- query X resources
        

        
           ,((modMask myConfig .|. shiftMask, xK_h), sshPrompt defaultXPConfig)
        
           -- workspace management
                                                                                            
           ,((modMask myConfig .|. controlMask, xK_t),  namedScratchpadAction scratchpads "translate")
           ,((modMask myConfig, xK_Down),  nextWS)
           ,((modMask myConfig, xK_j),  rotSlavesDown)
           ,((modMask myConfig, xK_k),  rotSlavesUp)
           ,((modMask myConfig, xK_u),  rotFocusedDown)
           ,((modMask myConfig, xK_i),  rotFocusedUp)
           ,((modMask myConfig, xK_Up),    prevWS)
           ,((modMask myConfig .|. controlMask, xK_Up),  shiftToNext)
           ,((modMask myConfig .|. controlMask, xK_Down),    shiftToPrev)
           ,((modMask myConfig, xK_Right), nextScreen)
           ,((modMask myConfig, xK_Left),  prevScreen)
           ,((modMask myConfig .|. controlMask, xK_Right), shiftNextScreen)
           ,((modMask myConfig .|. controlMask, xK_Left),  shiftPrevScreen)
           ,((modMask myConfig .|. controlMask, xK_z),     toggleWS)
           ,((modMask myConfig ,xK_x), sendMessage $ Toggle MIRROR)
           ,((modMask myConfig ,xK_f), sendMessage $ Toggle FULL)
           ,((modMask myConfig ,xK_y), sendMessage $ Toggle REFLECTX)
           ,((modMask myConfig ,xK_b), sendMessage(ToggleStruts))
           -- ,((modMask myConfig, xK_x), sendMessage (Toggle "Circle"))
        
        
           -- session management
           ,((modMask myConfig .|. controlMask, xK_f), withFocused float)
           ,((modMask myConfig .|. controlMask, xK_Delete), spawn "xkill")
           ,((modMask myConfig .|. shiftMask,   xK_Delete), kill)
           ,((modMask myConfig .|. controlMask, xK_s), spawn "gksu poweroff")
           ,((modMask myConfig .|. controlMask, xK_r), spawn "gksu reboot")
           ,((modMask myConfig .|. controlMask, xK_l ), spawn "gnome-screensaver-command --lock")
           ,((modMask myConfig .|. controlMask, xK_v), spawn "gnome-volume-control")
           ,((modMask myConfig .|. controlMask, xK_o), spawn "sleep 0.5; xset dpms force off")
         
           -- , ((modMask myConfig .|. shiftMask,                 xK_space), layoutScreens 2 (TwoPane 0.5 0.5))
           -- , ((modMask myConfig .|. controlMask .|. shiftMask, xK_space), rescreen)
        
        
        
           -- multi monitor stuff
           ,((modMask myConfig .|. controlMask, xK_1), spawn "xrandr -s 0")
           ,((modMask myConfig .|. controlMask, xK_2), spawn "xrandr -s 1")
           ,((modMask myConfig .|. controlMask, xK_3), spawn "xrandr -s 2")
        
           ] 

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

pangoColor :: String -> String -> String
pangoColor fg = wrap left right
 where
  left  = "<span foreground=\"" ++ fg ++ "\">"
  right = "</span>"

sanitize :: String -> String
sanitize [] = []
sanitize (x:rest) | fromEnum x > 127 = "&#" ++ show (fromEnum x) ++ "; " ++
                                       sanitize rest
                  | otherwise        = x : sanitize rest




