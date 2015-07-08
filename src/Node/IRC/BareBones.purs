module Node.IRC.BareBones where

import Prelude
import Control.Monad.Eff

foreign import data Client :: *
foreign import data IRC :: !

-- | Create an IRC client, by supplying a hostname, nick for the client to use,
-- | and an array of channels to connect to.
-- | ```purescript
-- | createClient "irc.freenode.org" "bot" ["#purescript"]
-- | ```
foreign import createClient ::
  forall e. String -> String -> Array String -> Eff (irc :: IRC | e) Client

-- | A JavaScript `arguments` object.
foreign import data ArgumentsJS :: *

-- | 
type IRCCallback e args =
  { fromArgumentsJS :: ArgumentsJS -> args
  , action :: args -> Eff (irc :: IRC | e) Unit
  }

-- | The low-level mechanism for having a client react to events.
-- | ```purescript
-- | addListener client "message" callback
-- | ```
foreign import addListener ::
  forall e args. Client -> String -> IRCCallback e args -> Eff (irc :: IRC | e) Unit

-- | Say something to a specific channel or nick.
-- | ```purescript
-- | say client "#yourchannel" "I'm a bot!"
-- | say client "nonbeliever" "Yes, really."
-- | ```
foreign import say ::
  forall e. Client -> String -> String -> Eff (irc :: IRC | e) Unit
