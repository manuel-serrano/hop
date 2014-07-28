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

// This is an excerpt an node.js that initialize tick counters.
function initNodeTick( process ) {
  processNextTick = function() {
    var _needTickCallback = process._needTickCallback;
    var nextTickQueue = [];
    var needSpinner = true;
    var inTick = false;

    // this infobox thing is used so that the C++ code in src/node.cc
    // can have easy accesss to our nextTick state, and avoid unnecessary
    // calls into process._tickCallback.
    // order is [length, index, depth]
    // Never write code like this without very good reason!
    var infoBox = process._tickInfoBox;
    var length = 0;
    var index = 1;
    var depth = 2;

    process.nextTick = function nextTick(cb) {
      process._currentTickHandler(cb);
    };

    // needs to be accessible from cc land
    process._currentTickHandler = _nextTick;
    process._nextDomainTick = _nextDomainTick;
    process._tickCallback = _tickCallback;
    process._tickDomainCallback = _tickDomainCallback;
    process._tickFromSpinner = _tickFromSpinner;

    // the maximum number of times it'll process something like
    // nextTick(function f(){nextTick(f)})
    // It's unlikely, but not illegal, to hit this limit.  When
    // that happens, it yields to libuv's tick spinner.
    // This is a loop counter, not a stack depth, so we aren't using
    // up lots of memory here.  I/O can sneak in before nextTick if this
    // limit is hit, which is not ideal, but not terrible.
    process.maxTickDepth = 1000;

    function tickDone(tickDepth_) {
      if (infoBox[length] !== 0) {
        if (infoBox[length] <= infoBox[index]) {
          nextTickQueue = [];
          infoBox[length] = 0;
        } else {
          nextTickQueue.splice(0, infoBox[index]);
          infoBox[length] = nextTickQueue.length;
          if (needSpinner) {
            _needTickCallback();
            needSpinner = false;
          }
        }
      }
      inTick = false;
      infoBox[index] = 0;
      infoBox[depth] = tickDepth_;
    }

    function maxTickWarn() {
      // XXX Remove all this maxTickDepth stuff in 0.11
      var msg = '(node) warning: Recursive process.nextTick detected. ' +
                'This will break in the next version of node. ' +
                'Please use setImmediate for recursive deferral.';
      if (process.throwDeprecation)
        throw new Error(msg);
      else if (process.traceDeprecation)
        console.trace(msg);
      else
        console.error(msg);
    }

    function _tickFromSpinner() {
      needSpinner = true;
      // coming from spinner, reset!
      if (infoBox[depth] !== 0)
        infoBox[depth] = 0;
      // no callbacks to run
      if (infoBox[length] === 0)
        return infoBox[index] = infoBox[depth] = 0;
      process._tickCallback();
    }

    // run callbacks that have no domain
    // using domains will cause this to be overridden
    function _tickCallback() {
      var callback, nextTickLength, threw;

      if (inTick) return;
      if (infoBox[length] === 0) {
        infoBox[index] = 0;
        infoBox[depth] = 0;
        return;
      }
      inTick = true;

      while (infoBox[depth]++ < process.maxTickDepth) {
        nextTickLength = infoBox[length];
        if (infoBox[index] === nextTickLength)
          return tickDone(0);

        while (infoBox[index] < nextTickLength) {
          callback = nextTickQueue[infoBox[index]++].callback;
          threw = true;
          try {
            callback();
            threw = false;
          } finally {
            if (threw) tickDone(infoBox[depth]);
          }
        }
      }

      tickDone(0);
    }

    function _tickDomainCallback() {
      var nextTickLength, tock, callback, threw;

      // if you add a nextTick in a domain's error handler, then
      // it's possible to cycle indefinitely.  Normally, the tickDone
      // in the finally{} block below will prevent this, however if
      // that error handler ALSO triggers multiple MakeCallbacks, then
      // it'll try to keep clearing the queue, since the finally block
      // fires *before* the error hits the top level and is handled.
      if (infoBox[depth] >= process.maxTickDepth)
        return _needTickCallback();

      if (inTick) return;
      inTick = true;

      // always do this at least once.  otherwise if process.maxTickDepth
      // is set to some negative value, or if there were repeated errors
      // preventing depth from being cleared, we'd never process any
      // of them.
      while (infoBox[depth]++ < process.maxTickDepth) {
        nextTickLength = infoBox[length];
        if (infoBox[index] === nextTickLength)
          return tickDone(0);

        while (infoBox[index] < nextTickLength) {
          tock = nextTickQueue[infoBox[index]++];
          callback = tock.callback;
          if (tock.domain) {
            if (tock.domain._disposed) continue;
            tock.domain.enter();
          }
          threw = true;
          try {
            callback();
            threw = false;
          } finally {
            // finally blocks fire before the error hits the top level,
            // so we can't clear the depth at this point.
            if (threw) tickDone(infoBox[depth]);
          }
          if (tock.domain) {
            tock.domain.exit();
          }
        }
      }

      tickDone(0);
    }

    function _nextTick(callback) {
#:tprint( "_nextTick cb=", callback );
      // on the way out, don't bother. it won't get fired anyway.
      if (process._exiting)
        return;
#:tprint( "GLOP.1" );       
      if (infoBox[depth] >= process.maxTickDepth)
        maxTickWarn();
#:tprint( "GLOP.2" );       

      var obj = { callback: callback, domain: null };

      nextTickQueue.push(obj);
      infoBox[length]++;

      if (needSpinner) {
#:tprint( "_needTickBack=", _needTickCallback );
        _needTickCallback();
        needSpinner = false;
      }
    }

    function _nextDomainTick(callback) {
      // on the way out, don't bother. it won't get fired anyway.
      if (process._exiting)
        return;
      if (infoBox[depth] >= process.maxTickDepth)
        maxTickWarn();

      var obj = { callback: callback, domain: process.domain };

      nextTickQueue.push(obj);
      infoBox[length]++;

      if (needSpinner) {
        _needTickCallback();
        needSpinner = false;
      }
    }
  };

  processNextTick();
}

exports.initNodeTick = initNodeTick;


   
   
