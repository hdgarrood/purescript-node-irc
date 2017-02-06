"use strict"

exports.createClient = function (host) {
  return function (nick) {
    return function (channels) {
      return function () {
        var irc = require('irc')
        console.log('Created a client')
        return new irc.Client(host, nick, { channels: channels })
      }
    }
  }
}

exports.clientMethod = function (method) {
  return function (client) {
    return function (eventType) {
      return function (ircCallback) {
        return function () {
          client[method](eventType, function() {
            console.log('Got a ' + eventType + ' event!')
            var args = ircCallback.fromArgumentsJS(arguments)
            ircCallback.action(args)()
          })
          console.log('Added a ' + eventType + ' listener')
        }
      }
    }
  }
}

exports.say = function (client) {
  return function (target) {
    return function (message) {
      return function () {
        console.log('About to say "' + message + '" to ' + target)
        client.say(target, message)
      }
    }
  }
}
