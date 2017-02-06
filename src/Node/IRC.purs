module Node.IRC
  ( IRC()
  , Setup()
  , Host(..), runHost
  , Channel(..), runChannel
  , MessageText(..), runMessageText
  , Nick(..), runNick
  , connect
  , sayChannel
  , sayNick
  , ChannelMessageEvent()
  , onChannelMessage
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, logShow, CONSOLE())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (Aff, makeAff, runAff)
import Control.Monad.Reader.Trans (ReaderT(..), runReaderT)
import Unsafe.Coerce (unsafeCoerce)
import Partial.Unsafe (unsafePartial)
import Data.Array (unsafeIndex)

import Node.IRC.BareBones as BareBones

-------------
-- Re-exports

type IRC = BareBones.IRC

-----------
-- The rest

type Setup e a = ReaderT BareBones.Client (Aff (irc :: IRC, console :: CONSOLE | e)) a

runSetup :: forall e.
  Setup e Unit -> BareBones.Client -> Eff (irc :: IRC, console :: CONSOLE | e) Unit
runSetup setup client =
  void $ runAff logShow pure (runReaderT setup client)

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
  Host -> Nick -> Channel -> Setup e Unit -> Aff (irc :: IRC, console :: CONSOLE | e) Unit
connect (Host host) (Nick nick) chan setup = do
  client <- liftEff $ do
    c <- BareBones.createClient host nick [runChannel chan]
    -- Add an error handler, because otherwise errors will crash the whole
    -- program
    BareBones.addListener c "error"
      { fromArgumentsJS: unsafeFirstArgument, action: printInspect }
    pure c

  waitForEvent client "registered"
  waitForEvent client "join"

  -- Set it up
  runReaderT setup client

  where
  waitForEvent client eventType =
    makeAff \_ success ->
      BareBones.once client eventType
        { fromArgumentsJS: const unit, action: success }

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

-- | Add a callback to be run every time a message is sent to a particular
-- | channel.
onChannelMessage :: forall e.
  Channel
  -> (ChannelMessageEvent -> Setup e Unit)
  -> Setup e Unit
onChannelMessage chan cb =
  ReaderT \client -> liftEff $
    BareBones.addListener client
                          (toStr chan)
                          (mkCallback client)
  where
  toStr c =
    "message" <> runChannel c
  mkCallback client =
    { fromArgumentsJS: channelMessageFromArgumentsJS
    , action: \event -> runSetup (cb event) client
    }

foreign import channelMessageFromArgumentsJS ::
  BareBones.ArgumentsJS -> ChannelMessageEvent

unsafeFirstArgument :: forall a. BareBones.ArgumentsJS -> a
unsafeFirstArgument args = unsafePartial (flip unsafeIndex 0 $ unsafeCoerce args)

printInspect :: forall e a. a -> Eff (console :: CONSOLE | e) Unit
printInspect = log <<< inspect

foreign import inspect :: forall a. a -> String
