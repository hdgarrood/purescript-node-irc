module Node.IRC where

import Prelude
import Control.Monad.Eff

import qualified Node.IRC.BareBones as BareBones

-------------
-- Re-exports

type Client = BareBones.Client
type IRC = BareBones.IRC

-----------
-- The rest

newtype Host = Host String

runHost :: Host -> String
runHost (Host s) = s

newtype Channel = Channel String

runChannel :: Channel -> String
runChannel (Channel s) = s

newtype MessageText = MessageText String

runMessageText :: MessageText -> String
runMessageText (MessageText s) = s

newtype Nick = Nick String

runNick :: Nick -> String
runNick (Nick s) = s

createClient :: forall e.
  Host -> Nick -> Array Channel -> Eff (irc :: IRC | e) Client
createClient (Host host) (Nick nick) chans =
  BareBones.createClient host nick (map runChannel chans)

say :: forall e.
  Client -> Channel -> MessageText -> Eff (irc :: IRC | e) Unit
say client (Channel chan) (MessageText text) =
  BareBones.say client chan text

type ChannelMessageEvent =
  { nick :: Nick
  , text :: MessageText
  }

foreign import channelMessageFromArgumentsJS ::
  BareBones.ArgumentsJS -> ChannelMessageEvent

onChannelMessage :: forall e.
  BareBones.Client
  -> Channel
  -> (ChannelMessageEvent -> Eff (irc :: IRC | e) Unit)
  -> Eff (irc :: IRC | e) Unit
onChannelMessage client chan cb =
  BareBones.addListener client
                        (toStr chan)
                        (mkCallback cb)
  where
  toStr c =
    "message#" <> runChannel c
  mkCallback =
    { fromArgumentsJS: channelMessageFromArgumentsJS
    , action: _
    }
