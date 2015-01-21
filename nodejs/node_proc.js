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

// This is an excerpt an node.js that initializes process and exit
var startup = {};
var assert;

startup._lazyConstants = null;

startup.lazyConstants = function() {
  if (!startup._lazyConstants) {
    startup._lazyConstants = process.binding('constants');
  }
  return startup._lazyConstants;
};

var NativeModule = { require: require };

startup.processFatal = function(process) {
   // call into the active domain, or emit uncaughtException,
   // and exit if there are no listeners.
   process._fatalException = function(er) {
      var caught = false;
      if (process.domain) {
         var domain = process.domain;
         var domainModule = NativeModule.require('domain');
         var domainStack = domainModule._stack;

         // ignore errors on disposed domains.
         //
         // XXX This is a bit stupid.  We should probably get rid of
         // domain.dispose() altogether.  It's almost always a terrible
         // idea.  --isaacs
         if (domain._disposed)
            return true;

         er.domain = domain;
         er.domainThrown = true;
         // wrap this in a try/catch so we don't get infinite throwing
         try {
            // One of three things will happen here.
            //
            // 1. There is a handler, caught = true
            // 2. There is no handler, caught = false
            // 3. It throws, caught = false
            //
            // If caught is false after this, then there's no need to exit()
            // the domain, because we're going to crash the process anyway.
            caught = domain.emit('error', er);

            // Exit all domains on the stack.  Uncaught exceptions end the
            // current tick and no domains should be left on the stack
            // between ticks.
            var domainModule = NativeModule.require('domain');
            domainStack.length = 0;
            domainModule.active = process.domain = null;
         } catch (er2) {
            // The domain error handler threw!  oh no!
            // See if another domain can catch THIS error,
            // or else crash on the original one.
            // If the user already exited it, then don't double-exit.
            if (domain === domainModule.active)
               domainStack.pop();
            if (domainStack.length) {
               var parentDomain = domainStack[domainStack.length - 1];
               process.domain = domainModule.active = parentDomain;
               caught = process._fatalException(er2);
            } else
               caught = false;
         }
      } else {
         caught = process.emit('uncaughtException', er);
      }
      // if someone handled it, then great.  otherwise, die in C++ land
      // since that means that we'll exit the process, emit the 'exit' event
      if (!caught) {
         try {
            if (!process._exiting) {
               process._exiting = true;
               process.emit('exit', 1);
            }
         } catch (er) {
            // nothing to be done about it at this point.
         }
      }
      // if we handled an error, then make sure any ticks get processed
      if (caught)
         process._needTickCallback();
      return caught;
   };
};


startup.processAssert = function(process) {
   // Note that calls to assert() are pre-processed out by JS2C for the
   // normal build of node. They persist only in the node_g build.
   // Similarly for debug().
   assert = process.assert = function(x, msg) {
      if (!x) throw new Error(msg || 'assertion error');
   };
};

startup.processKillAndExit = function(process) {
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

startup.processChannel = function(process) {
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

exports.initFatal = startup.processFatal;
exports.initAssert = startup.processAssert;
exports.initProcessKillAndExit = startup.processKillAndExit;
exports.initProcessChannel = startup.processChannel;
