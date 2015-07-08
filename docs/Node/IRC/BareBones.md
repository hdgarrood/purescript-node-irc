## Module Node.IRC.BareBones

#### `Client`

``` purescript
data Client :: *
```

#### `IRC`

``` purescript
data IRC :: !
```

#### `createClient`

``` purescript
createClient :: forall e. String -> String -> Array String -> Eff (irc :: IRC | e) Client
```

Create an IRC client, by supplying a hostname, nick for the client to use,
and an array of channels to connect to.
```purescript
createClient "irc.freenode.org" "bot" ["#purescript"]
```

#### `ArgumentsJS`

``` purescript
data ArgumentsJS :: *
```

A JavaScript `arguments` object.

#### `IRCCallback`

``` purescript
type IRCCallback e args = { fromArgumentsJS :: ArgumentsJS -> args, action :: args -> Eff (irc :: IRC | e) Unit }
```

#### `addListener`

``` purescript
addListener :: forall e args. Client -> String -> IRCCallback e args -> Eff (irc :: IRC | e) Unit
```

The low-level mechanism for having a client react to events.
```purescript
addListener client "message" "I'm a bot!"
```

#### `say`

``` purescript
say :: forall e. Client -> String -> String -> Eff (irc :: IRC | e) Unit
```

Say something to a specific channel or nick.
```purescript
say client "#yourchannel" "I'm a bot!"
say client "nonbeliever" "Yes, really."
```


