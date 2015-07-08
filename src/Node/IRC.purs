module Node.IRC where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff
import Control.Monad.Reader.Trans (ReaderT(..), runReaderT)
import Control.Monad.Reader.Class (ask)
import Control.Monad.Error.Class (throwError)

import qualified Node.IRC.BareBones as BareBones

-------------
-- Re-exports

type IRC = BareBones.IRC

-----------
-- The rest

type Setup e a = ReaderT BareBones.Client (Aff (irc :: IRC | e)) a

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

connect :: forall e.
  Host -> Nick -> Array Channel -> Setup e Unit -> Aff (irc :: IRC | e) Unit
connect (Host host) (Nick nick) chans setup = do
  client <- liftEff $ BareBones.createClient host nick (map runChannel chans)
  -- Wait for registered event
  -- TODO: Handle error
  makeAff $ \_ success ->
    let cb = {fromArgumentsJS: const unit, action: success}
    in BareBones.addListener client "registered" cb
  runReaderT setup client

sayChannel :: forall e.
  Channel -> MessageText -> Setup e Unit
sayChannel (Channel chan) (MessageText text) =
  ReaderT \client -> liftEff $ BareBones.say client chan text

sayNick :: forall e.
  BareBones.Client -> Nick -> MessageText -> Eff (irc :: IRC | e) Unit
sayNick client (Nick nick) (MessageText text) =
  BareBones.say client nick text

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
