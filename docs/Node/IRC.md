## Module Node.IRC

#### `IRC`

``` purescript
type IRC = IRC
```

#### `Setup`

``` purescript
type Setup e a = ReaderT Client (Aff (irc :: IRC | e)) a
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
connect :: forall e. Host -> Nick -> Array Channel -> Setup e Unit -> Aff (irc :: IRC | e) Unit
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

#### `channelMessageFromArgumentsJS`

``` purescript
channelMessageFromArgumentsJS :: ArgumentsJS -> ChannelMessageEvent
```

#### `onChannelMessage`

``` purescript
onChannelMessage :: forall e. Client -> Channel -> (ChannelMessageEvent -> Eff (irc :: IRC | e) Unit) -> Eff (irc :: IRC | e) Unit
```

#### `unsafeFirstArgument`

``` purescript
unsafeFirstArgument :: forall a. ArgumentsJS -> a
```

#### `printInspect`

``` purescript
printInspect :: forall e a. a -> Eff (console :: CONSOLE | e) Unit
```

#### `inspect`

``` purescript
inspect :: forall a. a -> String
```


