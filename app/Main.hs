{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Control.Concurrent
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Control.Monad.Trans.State
import Data.Char
import Data.Coerce
import Data.Maybe
import Data.Monoid
import System.Random
import Text.Printf
import Unsafe.Coerce

import Graphics.UI.WX hiding (Event, newEvent, empty)
import Graphics.UI.WXCore hiding (Event, empty)
import Graphics.UI.WXCore.WxcTypes
-- import qualified Graphics.UI.WX as WX
import Reactive.Banana
import Reactive.Banana.WX

import Paths_perplex

import qualified Trie as T
-- import Trie (Trie)

import Perplex

main :: IO ()
main = start $ do
  -- TODO: this part is same as cli, we should refactor that
  wordsFile <- getDataFileName "words.txt"
  raw <- filter ((>2) . length) . lines <$> readFile wordsFile

  -- TODO: lots of optimisation
  let dict :: T.Trie Char
      dict = T.fromList raw
      letterFreqs = freqs (concat raw)

  forkIO_ $ evaluate (rnf dict)
  forkIO_ $ evaluate (rnf letterFreqs)

  -- TODO: Is wxWANTS_CHARS necessary? Need to test on other platforms
  topLevelFrame <- frame [ style :~ (.+. wxWANTS_CHARS) ]
  win <- panel topLevelFrame [ style :~ (.+. wxWANTS_CHARS) ]

  fileMenu <- menuPane [ text := "&File" ]
  mclose <- menuItem fileMenu [ text := "&Close\tCtrl+W" ]
  set topLevelFrame [ menuBar          := [fileMenu]
                    , on (menu mclose) := void $ windowClose topLevelFrame False
                    ]

  splitter <- splitterWindow win [ style :~ (.+. wxSP_3DSASH) ]

  -- TODO: Make board size dynamic
  board <- panel splitter [ bgcolor := rgb @Int 0xFF 0xA9 0x1F ]
  let tile = do
        backing <- panel board [ bgcolor := white ]

        -- Vertical text alignment isn't supported (apparently, the native libs don't support it)
        -- so we must use layout to align the textbox itself.
        --
        -- Horizontal alignment is also broken due to a longstanding issue with wxWidgets:
        -- The textbox will resize to fit text, but its top-left stays in the same place.
        -- wxST_NO_AUTORESIZE is a poor solution as it clips text if it grows.
        --
        -- Instead we return an action which can be used to realign the textbox.
        -- TODO: Why not just put the align action in onResize?
        --
        -- Ideally we would be able to have the letter expand horizontally
        -- but not vertically (see http://trac.wxwidgets.org/ticket/15415).
        -- Alternatively - try nested sizers
        -- http://trac.wxwidgets.org/attachment/ticket/15415/flex_grid_sizer_hv_workaround.py
        --
        -- Moreover, the textbox has a lot of space at the top because it must make space
        -- for all possible letters - presumably, capitals with accents.
        -- We don't want that so we fudge it away. TODO: Try GetTextExtent?
        letter <- staticText backing [ fontWeight := WeightBold , fontSize := 48 ]
        let fudgeAlign = marginBottom . marginWidth 9

        windowSetLayout backing (fudgeAlign . floatCentre $ widget letter)
        return (letter, void $ windowLayout backing, fill . widget $ backing)

  topHalf <- panel splitter []
  bottomHalf <- panel splitter []

  inputWord <- staticText bottomHalf [ fontSize := 20, text := "-", color := rgb @Int 0x33 0x33 0x33 ]
  let fixInputAlign = void $ windowLayout bottomHalf

  statsLinePane <- panel bottomHalf []
  statsLine <- staticText statsLinePane []
  wordListPane <- panel bottomHalf []
  wordList <- wrapSizerCreate wxHORIZONTAL wxWRAPSIZER_DEFAULT_FLAGS

  let tileSz = 80
      boardSize = 5
      boardSz = boardSize * tileSz

  tiles <- replicateM boardSize (replicateM boardSize tile)

  windowSetLayout win (fill $ container splitter
    (hsplit splitter 0 0

      (fill . container topHalf . margin 16 . floatCentre . shaped $
        container board (margin 2 . minsize (sz boardSz boardSz) $
          grid 2 2 ((map . map) (\(_,_,x) -> x) tiles)))

      (fill . container bottomHalf $ column 0
        [ rigid . hfloatCentre . minhsize 150 . boxed "" . floatCentre $ widget inputWord
        , margin 4 . hfill . container statsLinePane . floatCentre $ widget statsLine
        -- (mac) The textbox doesn't centre quite vertically in the staticBox,
        -- which seems to have a lot of padding at the top.
        , margin 4 . boxed "" . margin 4 . fill . minsize (sz 100 100) .
            container wordListPane . fill $ sizer wordList
        ])))
  let
    -- (Tested on mac only) wxEVT_CHAR doesn't return special keys.
    -- TODO: Use wxEVT_CHAR for letters and wxAccelerator for specials
    -- TODO: Refactor wxHaskell's WXCore.Events module?
    windowOnKeyCharHook win f = windowOnEvent win [wxEVT_CHAR_HOOK] f (f <=< eventKeyFromEvent . objectCast)

  seed <- getStdGen
  net <- compile $ mdo

    -- TODO: Perhaps we can change wxHaskell's 'write-only' events,
    -- so they can be used with reactive-banana's event1/event0
    (eEventKey, doEventKey) <- newEvent
    liftIO $ windowOnKeyCharHook win doEventKey
    let
      eKey = keyKey <$> eEventKey
      eKeyChar = toLower <$> filterJust (keyChar <$> eKey)
      eKeyBack = filterJust $ keyBack <$> eKey
      eKeySpace = filterJust $ keySpace <$> eKey
      eKeyReturn = filterJust $ keyReturn <$> eKey
      eBoardNew = () <$ filterE (\ek -> isJust (keyReturn $ keyKey ek) && keyModifiers ek == justShift) eEventKey

    eBoard <- unfoldE (runState $ genBoard (coerce <$> pick letterFreqs) boardSize) seed' eBoardNew
    let (board0, seed') = flip runState seed $ genBoard (coerce <$> pick letterFreqs) boardSize
    bBoard <- stepper board0 eBoard

    let bAllWords = T.intersection dict . unsafeCoerce . candidates boardSize <$> bBoard
    react (bAllWords <@ eBoardNew) $ \ws -> messageDialog topLevelFrame "All Words:" (unwords $ T.toList ws) wxOK

    let stats f t = printf "Words: %d/%d (%d%%)" f t (floor (realToFrac f / realToFrac t * 100 :: Double) :: Int)
    sink statsLine [ text :== stats <$> (length <$> bWords) <*> (T.size <$> bAllWords) ]
    react eWords $ \_ -> windowLayout statsLinePane

    forM2_ (keys boardSize) (concat tiles) $ \k (l,fixAlign,_) -> do
      sink l [ text :== (:"") . toUpper . coerce . getTileAt k <$> bBoard ]
      reactimate $ fixAlign <$ eBoard

    bInputWord <- accumB "" $ unions
      [ (\c w -> w ++ [c]) <$> eKeyChar
      , initOrNil <$ eKeyBack
      , const "" <$ eSubmit
      , const "" <$ eBoardNew
      ]

    let eSubmit :: Event String
        eSubmit = bInputWord <@ (eKeySpace <> eKeyReturn)
        eAccepted :: Event String
        eAccepted = filterApply ((\all found w -> w `T.elem` all && w `notElem` found) <$> bAllWords <*> bWords) eSubmit

    eWords <- accumE [] $ unions
      [ const [] <$ eBoardNew
      , (:) <$> eAccepted
      ]
    bWords <- stepper [] eWords

    react eWords $ \ws ->
      -- NOTE: This part assumes that words are added one at a time
      case ws of
        [] -> sizerClear wordList True
        (w:_) -> do
          putStrLn ("Add word! " ++ w)
          w' <- staticText wordListPane [ text := w, fontSize := 14 ]
          -- FIXME: wxWrapSizer seems to be broken; I can't get it to
          -- minsize properly, no matter what. If there are too
          -- many words, they will get clipped by the window.
          sizerAddWindow wordList w' 1 0 0 ptrNull

          windowLayout wordListPane
          return ()

    sink inputWord [ text :== bInputWord ]
    reactB_ bInputWord fixInputAlign

    -- Debug output
    reactimate $ (\k -> putStrLn ("Key: " ++ show k)) <$> eKey
    reactimate $ print <$> (bWords <@ eBoardNew)

    return ()

  actuate net
  windowSetLayout topLevelFrame (fill $ widget win)

  windowLayout statsLinePane
  forM_ (concat tiles) $ \(_,fixAlign,_) -> fixAlign

-- | Step a state function each time an event arrives
unfoldE :: MonadMoment m => (s -> (a, s)) -> s -> Event e -> m (Event a)
unfoldE f k es = do
  e <- accumE (undefined, k) (f . snd <$ es)
  return (fst <$> e)

forM2_ :: Applicative m => [a] -> [b] -> (a -> b -> m c) -> m ()
forM2_ as bs f = zipWithM_ f as bs

-- TODO: Change below helpers from Maybe () to Bool and see if we can
-- replace them with stuff from wxHaskell

-- It seems that when using CHAR_HOOK, the Fn modifer and Fn keys
-- get passed as \NUL for some reason, so filter it out.
keyChar (KeyChar '\0') = Nothing
keyChar (KeyChar c)
  | not (isAlpha c) = Nothing
keyChar (KeyChar c) = Just c
keyChar _ = Nothing

keyBack KeyBack = Just ()
keyBack _ = Nothing

keySpace KeySpace = Just ()
keySpace _ = Nothing

keyReturn KeyReturn = Just ()
keyReturn _ = Nothing

minhsize w = minsize (sz w (-1))
minvsize h = minsize (sz (-1) h)

initOrNil :: [a] -> [a]
initOrNil [] = []
initOrNil [_] = []
initOrNil (x:xs) = x : initOrNil xs

-- Copied from wxHaskell : wxcore/src/haskell/Graphics/UI/WXCore/Events.hs
eventKeyFromEvent :: KeyEvent a -> IO EventKey
eventKeyFromEvent event
  = do x <- keyEventGetX event
       y <- keyEventGetY event
       obj    <- eventGetEventObject event
       point' <- if objectIsNull obj
                 then return (Point x y)
                 else windowCalcUnscrolledPosition (objectCast obj) (Point x y)

       altDown'     <- keyEventAltDown event
       controlDown' <- keyEventControlDown event
       shiftDown'   <- keyEventShiftDown event
       metaDown'    <- keyEventMetaDown event
       let modifiers = Modifiers altDown' shiftDown' controlDown' metaDown'

       keyCode <- keyEventGetKeyCode event
       let key = keyCodeToKey keyCode

       return (EventKey key modifiers point')

react :: Event a -> (a -> IO b) -> MomentIO ()
react e f = reactimate (void . f <$> e)

react_ :: Event a -> IO b -> MomentIO ()
react_ e a = react e (const a)

reactB :: Behavior a -> (a -> IO b) -> MomentIO ()
reactB b f = do
    e <- changes b
    reactimate' (fmap (void . f) <$> e)

reactB_ :: Behavior a -> IO b -> MomentIO ()
reactB_ b a = reactB b (const a)

instance Monoid a => Monoid (Event a) where
  mempty = never
  mappend = unionWith mappend

forkIO_ :: IO () -> IO ()
forkIO_ = void . forkIO
