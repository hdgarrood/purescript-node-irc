"use strict"

// module Node.IRC.BareBones

exports.createClient = function (host) {
  return function (nick) {
    return function (channels) {
      return function () {
        var irc = require('irc')
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
        })
      }
    }
  }
}

exports.say = function (client) {
  return function (target) {
    return function (message) {
      return function () {
        client.say(target, message)
      }
    }
  }
}
