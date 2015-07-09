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
import Control.Monad.Eff
import Control.Monad.Eff.Console (log, print, CONSOLE())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error())
import Control.Monad.Aff
import Control.Monad.Reader.Trans (ReaderT(..), runReaderT)
import Control.Monad.Reader.Class (ask)
import Control.Monad.Error.Class (throwError)
import qualified Data.Array.Unsafe as AU
import Unsafe.Coerce (unsafeCoerce)

import qualified Node.IRC.BareBones as BareBones

-------------
-- Re-exports

type IRC = BareBones.IRC

-----------
-- The rest

type Setup e a = ReaderT BareBones.Client (Aff (irc :: IRC, console :: CONSOLE | e)) a

runSetup :: forall e a.
  Setup e Unit -> BareBones.Client -> Eff (irc :: IRC, console :: CONSOLE | e) Unit
runSetup setup client =
  runAff print return (runReaderT setup client)

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
    return c

  waitForEvent client "registered"
  waitForEvent client "join"

  -- Set it up
  runReaderT setup client

  where
  waitForEvent client eventType =
    makeAff \_ success ->
      BareBones.addListener client eventType
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
unsafeFirstArgument = flip AU.unsafeIndex 0 <<< unsafeCoerce

printInspect :: forall e a. a -> Eff (console :: CONSOLE | e) Unit
printInspect = log <<< inspect

foreign import inspect :: forall a. a -> String
