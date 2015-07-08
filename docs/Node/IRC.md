## Module Node.IRC

#### `Client`

``` purescript
type Client = Client
```

#### `IRC`

``` purescript
type IRC = IRC
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

#### `createClient`

``` purescript
createClient :: forall e. Host -> Nick -> Array Channel -> Eff (irc :: IRC | e) Client
```

#### `say`

``` purescript
say :: forall e. Client -> Channel -> MessageText -> Eff (irc :: IRC | e) Unit
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


