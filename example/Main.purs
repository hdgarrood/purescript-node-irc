module Main where

import Prelude
import Control.Monad
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.Aff
import qualified Data.String as S

import Node.IRC

main :: forall e. Eff (irc :: IRC, console :: CONSOLE | e) Unit
main = launchAff $ do
  let chan = Channel "#purescript"
  connect (Host "irc.freenode.net") (Nick "pursuit-bot") [chan] $
    sayChannel chan (MessageText "Hello, world")

    onChannelMessage chan \event -> do
      let text = runMessageText event.text
      print $ "Got a message: " <> text
      when (S.charAt 1 text == Just '@')
        sayChannel chan (MessageText $ "I heard: " <> text)
