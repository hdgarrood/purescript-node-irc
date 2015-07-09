## Module Node.IRC

#### `IRC`

``` purescript
type IRC = IRC
```

#### `Setup`

``` purescript
type Setup e a = ReaderT Client (Aff (irc :: IRC, console :: CONSOLE | e)) a
```

#### `Host`

``` purescript
newtype Host
  = Host String
```

#### `runHost`

``` purescript
runHost :: Host -> String
```

#### `Channel`

``` purescript
newtype Channel
  = Channel String
```

#### `runChannel`

``` purescript
runChannel :: Channel -> String
```

#### `MessageText`

``` purescript
newtype MessageText
  = MessageText String
```

#### `runMessageText`

``` purescript
runMessageText :: MessageText -> String
```

#### `Nick`

``` purescript
newtype Nick
  = Nick String
```

#### `runNick`

``` purescript
runNick :: Nick -> String
```

#### `connect`

``` purescript
connect :: forall e. Host -> Nick -> Channel -> Setup e Unit -> Aff (irc :: IRC, console :: CONSOLE | e) Unit
```

#### `sayChannel`

``` purescript
sayChannel :: forall e. Channel -> MessageText -> Setup e Unit
```

#### `sayNick`

``` purescript
sayNick :: forall e. Client -> Nick -> MessageText -> Eff (irc :: IRC | e) Unit
```

#### `ChannelMessageEvent`

``` purescript
type ChannelMessageEvent = { nick :: Nick, text :: MessageText }
```

#### `onChannelMessage`

``` purescript
onChannelMessage :: forall e. Channel -> (ChannelMessageEvent -> Setup e Unit) -> Setup e Unit
```

Add a callback to be run every time a message is sent to a particular
channel.


