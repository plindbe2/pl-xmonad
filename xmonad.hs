import Control.Monad (foldM, when, mapM, mapM_, MonadPlus(mplus))
import Data.Char (chr)
import Data.List (elemIndices, foldr, intercalate, isInfixOf, isSuffixOf)
import Data.Traversable (traverse)
import System.IO
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.WindowNames
import XMonad.Actions.WorkspaceNames
import XMonad.Core (withWindowSet)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Fullscreen
import XMonad.Layout.Minimize
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Prompt
import XMonad.Prompt.Workspace (Wor(Wor))
import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.WorkspaceCompare (getSortByIndex)

import qualified XMonad.StackSet as W


{- Misc functions -}

-- Append the name to the current named workspace.
appendCurrentWorkspaceName :: String -> X ()
appendCurrentWorkspaceName new =
    getWorkspaceTag >>=
    getNamedWorkspace >>= \old ->
    setCurrentWorkspaceName $ newName old new

    where newName old' new' = let lst = split '|' old'
            in intercalate "|" $ (filter (not . null) lst) ++ [new']

appendWorkspace :: XPConfig -> X ()
appendWorkspace conf =
    mkXPrompt pr conf (const (return [])) appendCurrentWorkspaceName
    where pr = Wor "Append Name:"

curWorkspaceWinNames :: X [String]
curWorkspaceWinNames = withWindowSet $ mapM (fmap show . getName) . W.index

-- Get the current focused window.
currentFocusWin :: W.StackSet i l a s sd -> Maybe a
currentFocusWin ws = maybe Nothing (\x -> (Just (W.focus x))) ((W.stack . W.workspace . W.current) ws)

-- get the current name.
getNamedWorkspace :: String -> X String
getNamedWorkspace ws =
    getWorkspaceNames >>= \wss ->
    return $ drop 2 $ wss ws

-- Get the current workspace (Ex. "1", "2", ..., "9")
getWorkspaceTag :: X String
getWorkspaceTag = do
    ws <- gets windowset
    return $ W.tag $ W.workspace $ W.current ws

-- Match the short name with the appropriate one.
matchShortName :: [((String -> Bool), (String -> String))] -> String -> Maybe String
matchShortName ((x,y):xs) name = case x name of
                                   True  -> Just (y name)
                                   False -> matchShortName xs name
matchShortName _ _ = Nothing

-- Merge the current workspace into the Next/Prev.
mergeTo :: Direction1D -> X ()
mergeTo dir = findWorkspace getSortByIndex dir AnyWS 1 >>= \nextTag ->
    getWorkspaceTag >>= \curTag ->
    gets windowset >>= \ws ->
    windows $ mergeWorkspaces [curTag] nextTag $ W.workspaces ws

-- Merge the workspace tags from a list of workspace names (String numbers [1..9] by default)
mergeWorkspaceTags :: [WorkspaceId] -> X String
mergeWorkspaceTags wss =
    foldM combineNames [] wss
    where combineNames x y = getNamedWorkspace y >>= \y' ->
              let new = split '|' y'
              in return (intercalate "|" $ filter (not . null) (x : new))

-- Merge the workspace into the Next/Prev.
mergeCurrentWorkspaceTagsDir :: Direction1D -> X ()
mergeCurrentWorkspaceTagsDir dir = findWorkspace getSortByIndex dir AnyWS 1 >>=
    mergeCurrentWorkspaceTagsTo

mergeCurrentWorkspaceTagsTo :: WorkspaceId -> X ()
mergeCurrentWorkspaceTagsTo dest = getWorkspaceTag >>= \curTag ->
    mergeWorkspaceTags [curTag, dest] >>=
    setWorkspaceName dest >>
    setCurrentWorkspaceName ""

-- Merge a list of workspaces (identified by tag id) into another workspace
-- (also identified by id) using supplied [Workspace]
mergeWorkspaces :: (Ord a, Eq s, Eq i) => [i] -> i -> [W.Workspace i l a] -> (W.StackSet i l a s sd -> W.StackSet i l a s sd)
mergeWorkspaces wsIds destId wss = foldr (.) id (map moveWorkspaceWindows wss)
    where moveWorkspaceWindows ws' =
            let stack' = W.stack ws'
                tag'   = W.tag ws'
            in if tag' `elem` wsIds
                 then (moveFocused destId stack') . (moveUps destId stack') . (moveDowns destId stack')
                 else id
          -- todo : This is a little inefficient because shiftWin searches the
          -- whole stack set for the window id.
          moveFocused destId stack' = maybe (id) (\x -> (W.shiftWin destId) (W.focus x)) stack'
          moveUps destId stack' = maybe (id) (\x -> foldr (.) id $ map (W.shiftWin destId) (W.up x)) stack'
          moveDowns destId stack' = maybe (id) (\x -> foldr (.) id $ map (W.shiftWin destId) (W.down x)) stack'

-- Sink the current focused window given a StackSet.
sinkCurrentWindow :: W.StackSet i l Window s sd -> X ()
sinkCurrentWindow ws = maybe (return ()) (windows . W.sink) (currentFocusWin ws)

-- Split the string at char.
split :: Char -> String -> [String]
split _ "" =  []
split c s = let (l, s') = break (==c) s
    in  l : case s' of
        []      -> []
        (_:s'') -> split c s''


{- Configs -}

-- AdditionalKeys
myAdditionalKeys =
    [ ((mod4Mask .|. shiftMask, xK_Return  ), spawn "urxvt -e tmux -2")
    , ((mod4Mask, xK_b                     ), sendMessage ToggleStruts)
    , ((mod4Mask, xK_n                     ), withFocused minimizeWindow)
    , ((mod4Mask .|. shiftMask, xK_n       ), sendMessage RestoreNextMinimizedWin)
    , ((mod4Mask, xK_t                     ), spawn "urxvt")
    , ((mod4Mask .|. shiftMask, xK_t       ), spawn "urxvt -e $HOME/bin/tmux_sess.sh")
    , ((mod4Mask, xK_w                     ), prevScreen)
    , ((mod4Mask, xK_e                     ), nextScreen)
    , ((mod4Mask .|. shiftMask, xK_Left    ), swapTo Prev)
    , ((mod4Mask .|. shiftMask, xK_Right   ), swapTo Next)
    , ((mod4Mask .|. shiftMask, xK_r       ), renameWorkspace myShellPrompt)
    , ((mod4Mask .|. shiftMask, xK_u       ), focusUrgent)
    , ((mod4Mask, xK_0                     ), spawn "urxvt -e cd \"$(cat $HOME/tmp/hotkey_path)\" ; tmux -2")
    , ((mod4Mask .|. shiftMask, xK_e       ), appendShortnameWorkspace)
    , ((mod4Mask .|. shiftMask, xK_w       ), setCurrentWorkspaceName "")
    , ((mod4Mask .|. controlMask, xK_Right ), mergeTo Next >> mergeCurrentWorkspaceTagsDir Next)
    , ((mod4Mask .|. controlMask, xK_Left  ), mergeTo Prev >> mergeCurrentWorkspaceTagsDir Prev)
    , ((mod4Mask .|. controlMask, xK_r     ), appendWorkspace myShellPrompt)
    , ((mod4Mask .|. controlMask, xK_e     ), tagWholeWorkspace)
    , ((mod4Mask .|. shiftMask, xK_f       ), gets windowset >>= sinkCurrentWindow)
    , ((mod4Mask .|. shiftMask, xK_g       ), gets windowset >>= \ws -> spawn $ "echo '" ++ (show ws) ++ "' >> /tmp/foo")
    , ((mod4Mask .|. shiftMask, xK_p       ), passClipboard myShellPrompt)
    , ((mod4Mask .|. shiftMask, xK_e       ), spawn "emacs")
    , ((mod4Mask .|. shiftMask, xK_m       ), spawn "urxvt -e tmux -2 new-session mc")
    ]
    ++
    [((mod4Mask .|. controlMask, i), mergeCurrentWorkspaceTo j >> mergeCurrentWorkspaceTagsTo j) | i <- [xK_0..xK_9], j <- [[chr $ fromIntegral i]]]
    where appendShortnameWorkspace = logTitle >>= \x ->
            whenJust x $ \y ->
            whenJust (matchShortName myShortNames y) appendCurrentWorkspaceName
          -- This is a local function because it relies on tag names being
          -- string numbers.
          mergeCurrentWorkspaceTo destTag = getWorkspaceTag >>= \curTag ->
            gets windowset >>= \ws ->
            windows $ mergeWorkspaces [curTag] destTag (W.workspaces ws)
          tagWholeWorkspace = curWorkspaceWinNames >>=
            mapM_ filterAppendNames
            where filterAppendNames name = whenJust (matchShortName myShortNames name) $ appendCurrentWorkspaceName
          passClipboard conf =
            mkXPrompt pr conf (const (return [])) $ \s -> spawn $ "$HOME/bin/pass-xclip.sh " ++ s
            where pr = Wor "Pass:"

-- Layout hook
myLayoutHook = avoidStruts (
    minimize (
            tiled |||
            Mirror tiled |||
            noBorders (fullscreenFull Full)
        )
    )
    where
        -- default tiling algorithm partitions the screen into two panes
        tiled = spacing 2 $ Tall nmaster delta ratio

        -- The default number of windows in the master pane
        nmaster = 1

        -- Default proportion of screen occupied by master pane
        ratio = 1/2

        -- Percent of screen to increment by when resizing panes
        delta = 5/100

-- Shortnames
myShortNames :: [((String -> Bool), (String -> String))]
myShortNames = [ (("Pentadactyl" `isSuffixOf`), (\_ -> "ff"))
               , ((=="Library"), (\_ -> "ffdl"))
               , ((\_ -> True), (take 4)) -- catch all
               ]

-- Shell prompt theme
myShellPrompt = defaultXPConfig
       { font = "xft:terminus:pixelsize=10"
       , bgColor           = "#222222"
       , fgColor           = "#777777"
       , bgHLight          = "#93a1a1"
       , fgHLight          = "#002b36"
       , borderColor       = "#2aa198"
       , promptBorderWidth = 1
       , position          = Top
       , height            = 15
       , defaultText       = []
       }

-- XmobarPP
myXmobarPP :: Handle -> PP
myXmobarPP handle = xmobarPP
    { ppOutput = hPutStrLn handle
    , ppTitle = xmobarColor "green" "" . shorten 50
    , ppUrgent = xmobarColor "yellow" "red"
    , ppLayout = xmobarColor "yellow" "" .
        (\x -> case x of
            "Minimize Spacing 2 Tall"        -> "[]="
            "Minimize Mirror Spacing 2 Tall" -> "TTT"
            "Minimize Full"                  -> "[ ]"
        )
    }

-- Xmonad Config
myXmonadConfig handle = withUrgencyHook NoUrgencyHook $ defaultConfig
    { modMask = mod4Mask,
      manageHook = manageDocks <+> manageHook defaultConfig,
      layoutHook = avoidStruts $ smartBorders $ myLayoutHook,
      logHook = (dynamicLogWithPPMap $ getWindowNames (foc . filt) filt filt) =<< workspaceNamesPP (myXmobarPP handle),
      -- logHook = dynamicLogWithPPMod filterShortNames (wrap "{" "}") =<< workspaceNamesPP (myXmobarPP handle)
      handleEventHook = mconcat [docksEventHook, handleEventHook defaultConfig]
    }
    `additionalKeys`
    myAdditionalKeys
  where filt = filterShortNames myShortNames
        foc  = wrap "<fn=1>" "</fn>"

{- Main -}

main = do
    xmproc <- spawnPipe "/home/phil/.cabal/bin/xmobar /home/phil/.xmonad/xmobarrc"
    xmonad $ myXmonadConfig xmproc
