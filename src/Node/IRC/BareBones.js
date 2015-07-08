"use strict"

// module Node.IRC.BareBones

exports.createClient = function (host) {
  return function (nick) {
    return function (channels) {
      return function () {
        var irc = require('irc')
        console.log('created a client')
        return new irc.Client(host, nick, { channels: channels })
      }
    }
  }
}

exports.addListener = function (client) {
  return function (eventType) {
    return function (ircCallback) {
      return function () {
        client.addListener(eventType, function() {
          var args = ircCallback.fromArgumentsJS(arguments)
          ircCallback.action(args)()
          console.log('Added a listener')
        })
      }
    }
  }
}

exports.say = function (client) {
  return function (target) {
    return function (message) {
      return function () {
        console.log('client is: ' + require('util').inspect(client))
        console.log('About to say ' + message + ' to ' + target)
        client.say(target, message)
      }
    }
  }
}
