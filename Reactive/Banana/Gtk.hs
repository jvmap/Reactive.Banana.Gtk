{-# LANGUAGE ExistentialQuantification #-}
{-----------------------------------------------------------------------------
    reactive-banana-gtk
    
    Utility functions for interfacing with gtk
------------------------------------------------------------------------------}

module Reactive.Banana.Gtk where

import Reactive.Banana (liftIO, (<$>))
import qualified Reactive.Banana as Rb
import qualified Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk (AttrOp ((:=)))

{-----------------------------------------------------------------------------
    GTK
    
    Utilities for representing stuff from GTK as events and behaviors
------------------------------------------------------------------------------}

-- | Event with exactly one parameter.
event1 :: Gtk.GObjectClass w => w -> Gtk.Signal w (a -> IO ()) -> Rb.NetworkDescription (Rb.Event a)
event1 widget signal = Rb.fromAddHandler $ \callback -> do
    cid <- Gtk.on widget signal callback
    return $ Gtk.signalDisconnect cid

-- | Event without parameters.
event0 :: Gtk.GObjectClass w => w -> Gtk.Signal w (IO ()) -> Rb.NetworkDescription (Rb.Event ())
event0 widget signal = Rb.fromAddHandler $ \callback -> do
    cid <- Gtk.on widget signal (callback ())
    return $ Gtk.signalDisconnect cid

-- | Behavior from an attribute
behavior :: w -> Gtk.Attr w a -> Rb.NetworkDescription (Rb.Behavior a)
behavior widget attr = Rb.fromPoll . liftIO $ Gtk.get widget attr

data Prop' w = forall a. (Gtk.Attr w a) :== Rb.Discrete a

infixr 0 :==

-- | "Animate" a property with a stream of events
sink :: w -> [Prop' w] -> Rb.NetworkDescription ()
sink widget props = mapM_ sink1 props where
    sink1 (attr :== x) = do
        Rb.liftIOLater $ Gtk.set widget [attr := Rb.initial x]
        Rb.reactimate $ (\x -> Gtk.set widget [attr := x]) <$> Rb.changes x

