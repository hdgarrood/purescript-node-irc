module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Aff

import Node.IRC

main :: forall e. Eff (irc :: IRC | e) Unit
main = launchAff $ do
  let chan = Channel "#purescript-bot-testing"
  connect (Host "irc.freenode.net") (Nick "pursuit-bot") [chan] $
    sayChannel chan (MessageText "Hello, world")

--    onMessage chan $ \(MessageText text) -> do
--      result <- queryPursuit $ "/search?q=" <> text
--      say chan (formatResult result)
