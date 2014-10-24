// Copyright Joyent, Inc. and other Node contributors.
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to permit
// persons to whom the Software is furnished to do so, subject to the
// following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
// NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
// USE OR OTHER DEALINGS IN THE SOFTWARE.

// Hello, and welcome to hacking node.js!
//
// This file is invoked by node::Load in src/node.cc, and responsible for
// bootstrapping the node.js core. Special caution is given to the performance
// of the startup process, so many dependencies are invoked lazily.

// This is an excerpt an node.js that initialize process and exit
var startup = {};

startup._lazyConstants = null;

startup.lazyConstants = function() {
  if (!startup._lazyConstants) {
    startup._lazyConstants = process.binding('constants');
  }
  return startup._lazyConstants;
};

function initProcessKillAndExit( process ) {
  process.exit = function(code) {
    if (!process._exiting) {
      process._exiting = true;
      process.emit('exit', code || 0);
    }
    process.reallyExit(code || 0);
  };

  process.kill = function(pid, sig) {
    var r;

    // preserve null signal
    if (0 === sig) {
      r = process._kill(pid, 0);
    } else {
      sig = sig || 'SIGTERM';
      if (startup.lazyConstants()[sig] &&
          sig.slice(0, 3) === 'SIG') {
        r = process._kill(pid, startup.lazyConstants()[sig]);
      } else {
        throw new Error('Unknown signal: ' + sig);
      }
    }

    if (r) {
      throw errnoException(process._errno, 'kill');
    }

    return true;
  };
}

initProcessChannel = function() {
  // If we were spawned with env NODE_CHANNEL_FD then load that up and
  // start parsing data from that stream.
  if (process.env.NODE_CHANNEL_FD) {
    var fd = parseInt(process.env.NODE_CHANNEL_FD, 10);
    assert(fd >= 0);

    // Make sure it's not accidentally inherited by child processes.
    delete process.env.NODE_CHANNEL_FD;

    var cp = require('child_process');

    // Load tcp_wrap to avoid situation where we might immediately receive
    // a message.
    // FIXME is this really necessary?
    process.binding('tcp_wrap');

    cp._forkChild(fd);
    assert(process.send);
  }
}

exports.initProcessKillAndExit = initProcessKillAndExit;
exports.initProcessChannel = initProcessChannel;
