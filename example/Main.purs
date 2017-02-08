module Main where

import Prelude
import Data.Maybe (Maybe(..))
import Data.String as S
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Aff (launchAff)

import Node.IRC (Channel(..), Host(..), IRC, MessageText(..), Nick(..), connect, onChannelMessage, runMessageText, sayChannel)

main :: forall e. Eff (err :: EXCEPTION, irc :: IRC, console :: CONSOLE | e) Unit
main = void $ launchAff $ do
  let chan = Channel "#purescript-bot-testing"
  connect (Host "irc.freenode.net") (Nick "pursuit-bot") chan $ do
    sayChannel chan (MessageText "Hello, world")
    onChannelMessage chan \event -> do
      let text = runMessageText event.text
      liftEff $ log $ "Got a message: " <> text
      when (S.charAt 0 text == Just '@') $
        sayChannel chan (MessageText $ "I heard: " <> text)
