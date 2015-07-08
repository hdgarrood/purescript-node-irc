"use strict"

// module Node.IRC

exports.channelMessageFromArgumentsJS = function (args) {
  return {
    nick: args[0],
    text: args[1],
  }
}

exports.inspect = require('util').inspect
