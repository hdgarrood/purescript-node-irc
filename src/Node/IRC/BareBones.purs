module Node.IRC.BareBones
  ( IRC()
  , Client()
  , createClient
  , ArgumentsJS()
  , IRCCallback()
  , addListener
  , once
  , say
  ) where

import Prelude
import Control.Monad.Eff (kind Effect, Eff)

foreign import data IRC :: Effect
foreign import data Client :: Type

-- | Create an IRC client, by supplying a hostname, nick for the client to use,
-- | and an array of channels to connect to.
-- | ```purescript
-- | createClient "irc.freenode.org" "bot" ["#purescript"]
-- | ```
-- | Note that this function can return before the IRC client has managed to
-- | register, and attempting to send messages before registration will usually
-- | result in a runtime error. APIs that wrap this one should consider making
-- | this mistake impossible to make.
foreign import createClient ::
  forall e. String -> String -> Array String -> Eff (irc :: IRC | e) Client

-- | A JavaScript `arguments` object.
foreign import data ArgumentsJS :: Type

-- |
type IRCCallback e args =
  { fromArgumentsJS :: ArgumentsJS -> args
  , action :: args -> Eff (irc :: IRC | e) Unit
  }

foreign import clientMethod ::
  forall e args. String -> Client -> String -> IRCCallback e args -> Eff (irc :: IRC | e) Unit

-- | The low-level mechanism for having a client react to events. The second
-- | argument is the message type. Message types are defined by node-irc.
-- | ```purescript
-- | addListener client "message" callback
-- | ```
addListener ::
  forall e args. Client -> String -> IRCCallback e args -> Eff (irc :: IRC | e) Unit
addListener =
  clientMethod "addListener"

-- | The low-level mechanism for having a client react to events. The second
-- | argument is the message type. Message types are defined by node-irc.
-- |
-- | This is like `addListener`, except that the callback will be called at
-- | most once.
-- |
-- | ```purescript
-- | once client "message" callback
-- | ```
once ::
  forall e args. Client -> String -> IRCCallback e args -> Eff (irc :: IRC | e) Unit
once =
  clientMethod "once"

-- | Say something to a specific channel or nick.
-- | ```purescript
-- | say client "#yourchannel" "I'm a bot!"
-- | say client "nonbeliever" "Yes, really."
-- | ```
foreign import say ::
  forall e. Client -> String -> String -> Eff (irc :: IRC | e) Unit
