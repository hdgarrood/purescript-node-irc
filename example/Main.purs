module Main where

import Prelude
import Data.Maybe
import qualified Data.String as S
import Control.Monad
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console
import Control.Monad.Aff

import Node.IRC

main :: forall e. Eff (irc :: IRC, console :: CONSOLE | e) Unit
main = launchAff $ do
  let chan = Channel "#purescript-bot-testing"
  connect (Host "irc.freenode.net") (Nick "pursuit-bot") chan $ do
    sayChannel chan (MessageText "Hello, world")
    onChannelMessage chan \event -> do
      let text = runMessageText event.text
      liftEff $ log $ "Got a message: " <> text
      when (S.charAt 0 text == Just '@') $
        sayChannel chan (MessageText $ "I heard: " <> text)
