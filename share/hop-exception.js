var hop_get_stack;
var hop_report_exception;
var hop_mangledp;
var hop_demangle;
var hop_make_exception_stack;
var hop_make_exception_frame;
var hop_onerror_handler_hop_exception;
var hop_last_exception_hop_exception;
var BgL_sc_objzd2ze3string_1z31_hop_exception;
var in_exception_report_hop_exception;
var const_hop_exception;
var BgL_sc_const_2z00_hop_exception;
var BgL_sc_const_3z00_hop_exception;
var BgL_sc_const_4z00_hop_exception;
var BgL_sc_const_5z00_hop_exception;
var BgL_sc_const_6z00_hop_exception;
var BgL_sc_const_7z00_hop_exception;
var BgL_sc_const_8z00_hop_exception;
var BgL_sc_const_9z00_hop_exception;
var BgL_sc_const_10z00_hop_exception;
var BgL_sc_const_11z00_hop_exception;
var BgL_sc_const_12z00_hop_exception;
var BgL_sc_const_13z00_hop_exception;
var BgL_sc_const_14z00_hop_exception;
var BgL_sc_const_15z00_hop_exception;
var BgL_sc_const_16z00_hop_exception;
var BgL_sc_const_17z00_hop_exception;
var BgL_sc_const_18z00_hop_exception;
var BgL_sc_const_19z00_hop_exception;
var BgL_sc_const_20z00_hop_exception;
var BgL_sc_const_21z00_hop_exception;
var BgL_sc_const_22z00_hop_exception;
var BgL_sc_const_23z00_hop_exception;
const_hop_exception = "font-family: arial; font-size: 10pt; overflow: visible";
BgL_sc_const_2z00_hop_exception = "font-family: monospace; color: #777";
BgL_sc_const_3z00_hop_exception = "position: fixed;\n    top: 0; left: 0; right: 0; bottom: 0;\n    opacity: 0.8;\n    background: #141111;\n    z-index: 1000000";
BgL_sc_const_4z00_hop_exception = "font-size: 9pt; padding-left: 1em";
BgL_sc_const_5z00_hop_exception = "data:image/png;base64,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";
BgL_sc_const_6z00_hop_exception = "font-weight: bold";
BgL_sc_const_7z00_hop_exception = "color: red; font-weight: bold";
BgL_sc_const_8z00_hop_exception = "border-top: 1px dashed #ccc; margin-top: 2ex";
BgL_sc_const_9z00_hop_exception = "font-weight: bold; margin-bttom: 1ex;";
BgL_sc_const_10z00_hop_exception = "font-size: 9pt; padding-left: 1em;";
BgL_sc_const_11z00_hop_exception = "[(]at ([^ ]+) ([^ ]+)[)]";
BgL_sc_const_12z00_hop_exception = "width: 100%; font-size: 9pt; overflow: visible; padding-left: 1em";
BgL_sc_const_13z00_hop_exception = "margin:0; padding: 0";
BgL_sc_const_14z00_hop_exception = "position: fixed;\n    top: 60px; left: 150px; right: 150px; bottom: 60px;\n    opacity: 0.97;\n    background: white;\n    z-index: 10000001;\n    border: 3px dashed red; padding: 4px;\n    color: black;\n    overflow: hidden";
BgL_sc_const_15z00_hop_exception = "font-size: 20pt; padding-bottom: 4px";
BgL_sc_const_16z00_hop_exception = "JavaScript stack:";
BgL_sc_const_17z00_hop_exception = "Hop client stack:";
BgL_sc_const_18z00_hop_exception = "width: 100%; font-family: arial; font-size: 10pt; background: #FFFFF7; border-bottom: 1px solid #ccc; overflow: visible";
BgL_sc_const_19z00_hop_exception = "font-family: monospace; font-size: 10pt";
BgL_sc_const_20z00_hop_exception = "height: 64px; vertical-align: top; padding-top: 10px; text-align: center";
BgL_sc_const_21z00_hop_exception = "font-family: arial; font-size: 10pt; padding: 5px";
BgL_sc_const_22z00_hop_exception = "color: #777; font-weight: bold";
BgL_sc_const_23z00_hop_exception = "HopClientSideError";
sc_tmp = hop_mangledp = function(string) {
      var tmp1319;
      var tmp1318;
      var tmp1317;
      var len;
      len = string.length;
      if (len > 7) {
        tmp1317 = sc_arity_check(sc_isSubstring, 3)(string, "BgL_", 4);
        if ((tmp1317 !== false? tmp1317: sc_arity_check(sc_isSubstring, 3)(string, "BGl_", 4)) !== false) {
          if (sc_arity_check(sc_stringRef, 2)(string, len - 3).val === new sc_Char("z").val) {
            tmp1318 = sc_arity_check(sc_isCharAlphabetic, 1)(sc_arity_check(sc_stringRef, 2)(string, len - 2));
            if ((tmp1318 !== false? tmp1318: SC_NUMBER_CLASS.indexOf(sc_arity_check(sc_stringRef, 2)(string, len - 2).val) != -1) !== false) {
              tmp1319 = sc_arity_check(sc_isCharAlphabetic, 1)(sc_arity_check(sc_stringRef, 2)(string, len - 1));
              if (tmp1319 !== false) {
                return tmp1319;
              } else {
                return SC_NUMBER_CLASS.indexOf(sc_arity_check(sc_stringRef, 2)(string, len - 1).val) != -1;
              }
            } else {
              return false;
            }
          } else {
            return false;
          }
        } else {
          return false;
        }
      } else {
        return false;
      }
    }, sc_tmp.name = "bigloo-mangled?", sc_tmp.location = "(at hop-exception.scm 6803)", sc_tmp.sc_arity = 1, sc_tmp;
sc_tmp = hop_demangle = function(string) {
      var get_8bits_integer;
      var subvector;
      var bigloo_demangle_at;
      var clen;
      var len;
      len = string.length;
      clen = len - 3;
      sc_tmp = get_8bits_integer = function(r) {
            var i2;
            var i1;
            var c2;
            var c1;
            c1 = sc_arity_check(sc_stringRef, 2)(string, r + 1);
            c2 = sc_arity_check(sc_stringRef, 2)(string, r + 2);
            if (SC_NUMBER_CLASS.indexOf(c1.val) != -1) {
              i1 = c1.val.charCodeAt(0) - new sc_Char("0").val.charCodeAt(0);
            } else {
              i1 = 10 + (c1.val.charCodeAt(0) - new sc_Char("a").val.charCodeAt(0));
            }
            if (SC_NUMBER_CLASS.indexOf(c2.val) != -1) {
              i2 = c2.val.charCodeAt(0) - new sc_Char("0").val.charCodeAt(0);
            } else {
              i2 = 10 + (c2.val.charCodeAt(0) - new sc_Char("a").val.charCodeAt(0));
            }
            return i1 + (i2 << 4);
          }, sc_tmp.name = "get-8bits-integer", sc_tmp.location = "(at hop-exception.scm 7748)", sc_tmp.sc_arity = 1, sc_tmp;
      sc_tmp = subvector = function(vec, len) {
            var i;
            var l;
            var g1321;
            var g1320;
            g1320 = len - 1;
            g1321 = null;
            i = g1320;
            l = g1321;
            while (!(i === -1)) {
              l = new sc_Pair(vec[i], l);
              --i;
            }
            return sc_arity_check(sc_list2string, 1)(l);
          }, sc_tmp.name = "subvector", sc_tmp.location = "(at hop-exception.scm 7945)", sc_tmp.sc_arity = 2, sc_tmp;
      sc_tmp = bigloo_demangle_at = function(offset) {
            var nc;
            var i;
            var c;
            var r;
            var w;
            var checksum;
            var new_1;
            new_1 = sc_arity_check(sc_makeVector, 1)(clen);
            r = offset;
            w = 0;
            checksum = 0;
            while (!(r === clen)) {
              c = sc_arity_check(sc_stringRef, 2)(string, r);
              if (c.val === new sc_Char("z").val) {
                if (sc_arity_check(sc_stringRef, 2)(string, r + 1).val === new sc_Char("z").val) {
                  return new sc_Values([sc_arity_check(subvector, 2)(new_1, w - 1), r + 2]);
                } else {
                  i = sc_arity_check(get_8bits_integer, 1)(r);
                  nc = new sc_Char(String.fromCharCode(i));
                  new_1[w] = nc;
                  r += 3;
                  ++w;
                  checksum ^= i;
                }
              } else {
                new_1[w] = c;
                ++r;
                ++w;
              }
            }
            if (checksum === sc_arity_check(get_8bits_integer, 1)(r)) {
              return new sc_Values([sc_arity_check(subvector, 2)(new_1, w), r + 3]);
            } else {
              return new sc_Values([string, r + 3]);
            }
          }, sc_tmp.name = "bigloo-demangle-at", sc_tmp.location = "(at hop-exception.scm 8106)", sc_tmp.sc_arity = 1, sc_tmp;
      if (sc_arity_check(hop_mangledp, 1)(string) === false) {
        return string;
      } else {
        if (sc_arity_check(sc_isSubstring, 3)(string, "BgL_", 4) !== false) {
          return sc_arity_check(sc_callWithValues, 2)((sc_tmp = function() {
                      return sc_arity_check(bigloo_demangle_at, 1)(4);
                    }, sc_tmp.name = "", sc_tmp.location = "#f", sc_tmp.sc_arity = 0, sc_tmp), (sc_tmp = function(str, offset) {
                      return str;
                    }, sc_tmp.name = "", sc_tmp.location = "#f", sc_tmp.sc_arity = 2, sc_tmp));
        } else {
          if (sc_arity_check(sc_isSubstring, 3)(string, "BGl_", 4) !== false) {
            return sc_arity_check(sc_callWithValues, 2)((sc_tmp = function() {
                        return sc_arity_check(bigloo_demangle_at, 1)(4);
                      }, sc_tmp.name = "", sc_tmp.location = "#f", sc_tmp.sc_arity = 0, sc_tmp), (sc_tmp = function(id, offset) {
                        return sc_arity_check(sc_callWithValues, 2)((sc_tmp = function() {
                                    return sc_arity_check(bigloo_demangle_at, 1)(offset);
                                  }, sc_tmp.name = "", sc_tmp.location = "#f", sc_tmp.sc_arity = 0, sc_tmp), (sc_tmp = function(module, offset) {
                                    return id + "@" + module;
                                  }, sc_tmp.name = "", sc_tmp.location = "#f", sc_tmp.sc_arity = 2, sc_tmp));
                      }, sc_tmp.name = "", sc_tmp.location = "#f", sc_tmp.sc_arity = 2, sc_tmp));
          } else {
            return string;
          }
        }
      }
    }, sc_tmp.name = "bigloo-demangle", sc_tmp.location = "(at hop-exception.scm 7482)", sc_tmp.sc_arity = 1, sc_tmp;
sc_tmp = hop_get_stack = function(offset) {
      var depth = null;
      for (var sc_tmp = arguments.length - 1; sc_tmp >= 1; --sc_tmp) {
        depth = sc_cons(arguments[sc_tmp], depth);
      }
      var frame;
      var caller;
      var n;
      var stack;
      var g1324;
      var g1323;
      var proc;
      var offset_2;
      var g1322;
      g1322 = arguments.callee;
      proc = g1322;
      offset_2 = offset;
      while (!(offset_2 === 0)) {
        if (proc !== false) {
          proc = proc.caller;
          --offset_2;
        } else {
          return null;
        }
      }
      if (depth instanceof sc_Pair) {
        g1323 = depth.car;
      } else {
        g1323 = 10;
      }
      g1324 = null;
      caller = proc;
      n = g1323;
      stack = g1324;
      while (caller && n > 0) {
        frame = new sc_Pair(caller, sc_arity_check(sc_vector2list, 1)(caller.arguments));
        caller = caller.caller;
        --n;
        stack = new sc_Pair(frame, stack);
      }
      return sc_arity_check(sc_reverseBang, 1)(stack);
    }, sc_tmp.name = "hop-get-stack", sc_tmp.location = "(at hop-exception.scm 9657)", sc_tmp.sc_arity = -2, sc_tmp;
in_exception_report_hop_exception = "\uEBACno";
sc_tmp = hop_make_exception_frame = function() {
      var args = null;
      for (var sc_tmp = arguments.length - 1; sc_tmp >= 0; --sc_tmp) {
        args = sc_cons(arguments[sc_tmp], args);
      }
      var stmp;
      var g1329;
      var g1332;
      var g1330;
      var g1331;
      var mask;
      g1329 = sc_arity_check(dom_create, 4)("div", "\uEBADstyle", BgL_sc_const_3z00_hop_exception, "");
      mask = g1329;
      g1331 = sc_arity_check(dom_create, 4)("div", "\uEBADstyle", "overflow: auto", args);
      stmp = g1331;
      g1330 = sc_arity_check(dom_create, 4)("div", "\uEBADstyle", BgL_sc_const_14z00_hop_exception, stmp);
      g1332 = sc_arity_check(dom_create, 3)("div", mask, g1330);
      sc_arity_check(hop_add_event_listener, 3)(g1332, "click", (sc_tmp = function(event) {
            in_exception_report_hop_exception = "\uEBACno";
            return this.parentNode.removeChild(this);
          }, sc_tmp.name = "", sc_tmp.location = "#f", sc_tmp.sc_arity = 1, sc_tmp));
      return g1332;
    }, sc_tmp.name = "<EXCEPTION-FRAME>", sc_tmp.location = "(at hop-exception.scm 10896)", sc_tmp.sc_arity = -1, sc_tmp;
sc_tmp = BgL_sc_objzd2ze3string_1z31_hop_exception = function(o, longp) {
      var stmp;
      var g1336;
      var g1338;
      var m;
      var name;
      var g1334;
      if (typeof o === 'function') {
        if (!sc_arity_check(sc_isString, 1)(o.name)) {
          name = sc_arity_check(sc_withOutputToString, 1)((sc_tmp = function() {
                  return sc_arity_check(sc_write, 1)(o);
                }, sc_tmp.name = "", sc_tmp.location = "(at hop-exception.scm 11606)", sc_tmp.sc_arity = 0, sc_tmp));
        } else {
          if (o.name.length > 0) {
            name = o.name;
          } else {
            g1334 = sc_arity_check(dom_create, 2)("i", "anonymous");
            name = g1334;
          }
        }
        if (longp && sc_arity_check(sc_isString, 1)(o.location)) {
          m = sc_arity_check(sc_pregexpMatch, 2)(BgL_sc_const_11z00_hop_exception, o.location);
          if (m !== false) {
            g1338 = sc_arity_check(dom_create, 8)("a", "\uEBADstyle", "color: inherit", "\uEBADhref", m.cdr.car, m.cdr.car, "!", m.cdr.cdr.car);
            stmp = g1338;
            g1336 = sc_arity_check(dom_create, 5)("span", "\uEBADstyle", "color: #777", stmp, ", ");
            return sc_arity_check(sc_list, 4)(g1336, "(", name, " ...)");
          } else {
            return sc_arity_check(sc_list, 3)("(", name, " ...)");
          }
        } else {
          if (longp !== false) {
            return sc_arity_check(sc_list, 3)("(", name, " ...)");
          } else {
            return name;
          }
        }
      } else {
        if (sc_arity_check(sc_isString, 1)(o)) {
          return o;
        } else {
          return sc_arity_check(sc_withOutputToString, 1)((sc_tmp = function() {
                      return sc_arity_check(sc_write, 1)(o);
                    }, sc_tmp.name = "", sc_tmp.location = "(at hop-exception.scm 12179)", sc_tmp.sc_arity = 0, sc_tmp));
        }
      }
    }, sc_tmp.name = "obj->string", sc_tmp.location = "(at hop-exception.scm 11444)", sc_tmp.sc_arity = 2, sc_tmp;
sc_tmp = hop_make_exception_stack = function(stack) {
      var stmp;
      var stmp_3;
      var stmp_4;
      var stmp_5;
      var frame;
      var tail1392;
      var L1388;
      var falseHead1391;
      var g1342;
      var g1340;
      var g1339;
      falseHead1391 = new sc_Pair(null, null);
      tail1392 = falseHead1391;
      L1388 = stack;
      while (!(L1388 === null)) {
        frame = L1388.car;
        stmp_4 = new sc_Pair(sc_arity_check(sc_list, 2)(sc_arity_check(BgL_sc_objzd2ze3string_1z31_hop_exception, 2)(frame.car, true), "\n"), null);
        tail1392.cdr = stmp_4;
        tail1392 = tail1392.cdr;
        L1388 = L1388.cdr;
      }
      stmp_3 = falseHead1391.cdr;
      g1342 = sc_arity_check(dom_create, 4)("pre", "\uEBADstyle", BgL_sc_const_10z00_hop_exception, stmp_3);
      sc_arity_check(hop_add_event_listener, 3)(g1342, "click", (sc_tmp = function(event) {
            return sc_arity_check(hop_stop_propagation, 1)(event);
          }, sc_tmp.name = "", sc_tmp.location = "#f", sc_tmp.sc_arity = 1, sc_tmp));
      stmp = g1342;
      g1340 = sc_arity_check(dom_create, 4)("div", "\uEBADstyle", BgL_sc_const_9z00_hop_exception, BgL_sc_const_17z00_hop_exception);
      stmp_5 = g1340;
      g1339 = sc_arity_check(dom_create, 5)("div", "\uEBADstyle", BgL_sc_const_21z00_hop_exception, stmp_5, stmp);
      return g1339;
    }, sc_tmp.name = "<EXCEPTION-STACK>", sc_tmp.location = "(at hop-exception.scm 12416)", sc_tmp.sc_arity = 1, sc_tmp;
sc_tmp = hop_report_exception = function(exc) {
      var stmp;
      var stmp_6;
      var stmp_7;
      var stmp_8;
      var stmp_9;
      var stmp_10;
      var stmp_11;
      var stmp_12;
      var stmp_13;
      var stmp_14;
      var stmp_15;
      var stmp_16;
      var stmp_17;
      var stmp_18;
      var stmp_19;
      var stmp_20;
      var stmp_21;
      var stmp_22;
      var stmp_23;
      var stmp_24;
      var stmp_25;
      var stmp_26;
      var stmp_27;
      var stmp_28;
      var stmp_29;
      var stmp_30;
      var stmp_31;
      var stmp_32;
      var stmp_33;
      var stmp_34;
      var stmp_35;
      var stmp_36;
      var stmp_37;
      var stmp_38;
      var stmp_39;
      var stmp_40;
      var stmp_41;
      var stmp_42;
      var stmp_43;
      var g1343;
      var g1344;
      var href;
      var l;
      var i;
      var f;
      var tail1397;
      var L1393;
      var falseHead1396;
      var g1348;
      var g1347;
      var g1346;
      var l_44;
      var s;
      var s_45;
      var tail1407;
      var L1403;
      var s_46;
      var tail1402;
      var L1398;
      var g1376;
      var g1383;
      var g1345;
      var stack;
      var tmp1384;
      var g1382;
      var g1377;
      var g1379;
      var g1380;
      var g1381;
      var g1378;
      var g1353;
      var g1359;
      var g1364;
      var g1365;
      var g1374;
      var g1375;
      var g1372;
      var g1373;
      var g1369;
      var g1370;
      var g1371;
      var g1366;
      var g1367;
      var g1368;
      var g1361;
      var g1363;
      var g1355;
      var g1357;
      var src;
      var g1351;
      var g1350;
      var g1349;
      var location;
      var url;
      var name;
      var msg;
      var message;
      var L1403_47;
      var falseHead1406;
      var L1398_48;
      var falseHead1401;
      if (in_exception_report_hop_exception === "\uEBACyes") {
        return sc_arity_check(sc_raise, 1)(exc);
      } else {
        if (document.body && !(document.body === null)) {
          in_exception_report_hop_exception = "\uEBACyes";
          if (sc_arity_check(sc_isString, 1)(exc.message)) {
            falseHead1401 = new sc_Pair(null, null);
            L1398_48 = sc_arity_check(sc_stringSplit, 2)(exc.message, "\n ");
            tail1402 = falseHead1401;
            L1398 = L1398_48;
            while (!(L1398 === null)) {
              s_46 = L1398.car;
              stmp_43 = new sc_Pair(sc_arity_check(hop_demangle, 1)(s_46) + " ", null);
              tail1402.cdr = stmp_43;
              tail1402 = tail1402.cdr;
              L1398 = L1398.cdr;
            }
            stmp_42 = falseHead1401.cdr;
            message = sc_arity_check(sc_apply, 2)(sc_stringAppend, stmp_42);
          } else {
            if (sc_arity_check(sc_isSymbol, 1)(exc.message)) {
              message = exc.message.slice(1);
            } else {
              if (sc_arity_check(sc_isKeyword, 1)(exc.message)) {
                message = exc.message.slice(1);
              } else {
                if (sc_arity_check(sc_isNumber, 1)(exc.message)) {
                  message = exc.message;
                } else {
                  if (!(exc.message === undefined)) {
                    message = sc_arity_check(BgL_sc_objzd2ze3string_1z31_hop_exception, 2)(exc.message, false);
                  } else {
                    if (sc_arity_check(sc_isString, 1)(exc.description)) {
                      falseHead1406 = new sc_Pair(null, null);
                      L1403_47 = sc_arity_check(sc_stringSplit, 2)(exc.description, "\n ");
                      tail1407 = falseHead1406;
                      L1403 = L1403_47;
                      while (!(L1403 === null)) {
                        s_45 = L1403.car;
                        stmp_41 = new sc_Pair(sc_arity_check(hop_demangle, 1)(s_45) + " ", null);
                        tail1407.cdr = stmp_41;
                        tail1407 = tail1407.cdr;
                        L1403 = L1403.cdr;
                      }
                      stmp_40 = falseHead1406.cdr;
                      message = sc_arity_check(sc_apply, 2)(sc_stringAppend, stmp_40);
                    } else {
                      message = "unknwown error";
                    }
                  }
                }
              }
            }
          }
          if ("scObject" in exc) {
            msg = sc_arity_check(sc_list, 3)(message, " -- ", sc_arity_check(BgL_sc_objzd2ze3string_1z31_hop_exception, 2)(exc.scObject, false));
          } else {
            msg = message;
          }
          if (sc_arity_check(sc_isString, 1)(exc.name)) {
            name = exc.name;
          } else {
            if (exc.name === undefined) {
              name = BgL_sc_const_23z00_hop_exception;
            } else {
              name = sc_arity_check(BgL_sc_objzd2ze3string_1z31_hop_exception, 2)(exc.name, false);
            }
          }
          if (sc_arity_check(sc_isString, 1)(exc.fileName)) {
            url = exc.fileName;
          } else {
            url = document.location.href;
          }
          if (sc_arity_check(sc_isString, 1)(exc.hopLocation)) {
            location = exc.hopLocation;
          } else {
            location = "Client Error";
          }
          if (exc.lineNumber && !(exc.lineNumber === undefined)) {
            g1349 = sc_arity_check(dom_create, 4)("a", "\uEBADhref", url, url);
            src = sc_arity_check(sc_list, 3)(g1349, ", line ", exc.lineNumber);
          } else {
            if (exc.line && !(exc.line === undefined)) {
              g1350 = sc_arity_check(dom_create, 4)("a", "\uEBADhref", url, url);
              src = sc_arity_check(sc_list, 3)(g1350, ", line ", exc.line);
            } else {
              g1351 = sc_arity_check(dom_create, 3)("a", "\uEBADhref", url);
              src = g1351;
            }
          }
          if (sc_arity_check(sc_isString, 1)(exc.stack)) {
            stack = exc.stack;
            g1345 = sc_arity_check(sc_stringSplit, 2)(stack, "\n");
            BgL_whilezd2break1455zd2: {
              l_44 = g1345;
              s = 2;
              while (!(l_44 === null)) {
                if (s === 0) {
                  falseHead1396 = new sc_Pair(null, null);
                  tail1397 = falseHead1396;
                  L1393 = l_44;
                  while (!(L1393 === null)) {
                    f = L1393.car;
                    i = sc_arity_check(sc_stringIndex, 2)(f, new sc_Char("@"));
                    l = f.length;
                    if (i !== false) {
                      href = f.substring(i + 1, l);
                      g1344 = sc_arity_check(dom_create, 6)("a", "\uEBADstyle", "color: inherit", "\uEBADhref", href, href);
                      stmp_13 = g1344;
                      g1343 = sc_arity_check(dom_create, 5)("span", "\uEBADstyle", "color: #777", stmp_13, ", ");
                      stmp_12 = sc_arity_check(sc_list, 3)(g1343, f.substring(0, i), "\n");
                    } else {
                      stmp_12 = f;
                    }
                    stmp_11 = new sc_Pair(stmp_12, null);
                    tail1397.cdr = stmp_11;
                    tail1397 = tail1397.cdr;
                    L1393 = L1393.cdr;
                  }
                  stmp_10 = falseHead1396.cdr;
                  g1348 = sc_arity_check(dom_create, 4)("pre", "\uEBADstyle", BgL_sc_const_4z00_hop_exception, stmp_10);
                  sc_arity_check(hop_add_event_listener, 3)(g1348, "click", (sc_tmp = function(event) {
                        return sc_arity_check(hop_stop_propagation, 1)(event);
                      }, sc_tmp.name = "", sc_tmp.location = "#f", sc_tmp.sc_arity = 1, sc_tmp));
                  stmp_9 = g1348;
                  g1347 = sc_arity_check(dom_create, 4)("div", "\uEBADstyle", BgL_sc_const_6z00_hop_exception, BgL_sc_const_16z00_hop_exception);
                  stmp_14 = g1347;
                  g1346 = sc_arity_check(dom_create, 5)("div", "\uEBADstyle", BgL_sc_const_21z00_hop_exception, stmp_14, stmp_9);
                  {
                    stmp_8 = g1346;
                    break BgL_whilezd2break1455zd2;
                  }
                } else {
                  l_44 = l_44.cdr;
                  --s;
                }
              }
              stmp_8 = "";
            }
            if (exc.hopService !== false) {
              tmp1384 = !(exc.hopService === undefined);
            } else {
              tmp1384 = false;
            }
            if ((tmp1384 !== false? tmp1384: exc.hopStack instanceof sc_Pair) !== false) {
              stmp_15 = BgL_sc_const_8z00_hop_exception;
            } else {
              stmp_15 = "margin-top: 2ex";
            }
            g1383 = sc_arity_check(dom_create, 4)("div", "\uEBADstyle", stmp_15, stmp_8);
            stmp_7 = g1383;
          } else {
            stmp_7 = false;
          }
          if (exc.hopStack instanceof sc_Pair) {
            g1382 = sc_arity_check(dom_create, 4)("div", "\uEBADstyle", exc.hopService && !(exc.hopService === undefined)? BgL_sc_const_8z00_hop_exception: "margin-top: 2ex", sc_arity_check(hop_make_exception_stack, 1)(exc.hopStack));
            stmp_17 = g1382;
          } else {
            stmp_17 = false;
          }
          if (exc.hopService && !(exc.hopService === undefined)) {
            g1381 = sc_arity_check(dom_create, 4)("td", "\uEBADstyle", "font-size: 10pt", sc_arity_check(BgL_sc_objzd2ze3string_1z31_hop_exception, 2)(exc.hopService, false));
            stmp_21 = g1381;
            g1380 = sc_arity_check(dom_create, 2)("tr", stmp_21);
            stmp_20 = g1380;
            g1379 = sc_arity_check(dom_create, 4)("table", "\uEBADstyle", BgL_sc_const_12z00_hop_exception, stmp_20);
            stmp_19 = g1379;
            g1378 = sc_arity_check(dom_create, 4)("div", "\uEBADstyle", BgL_sc_const_6z00_hop_exception, "Service:");
            stmp_22 = g1378;
            g1377 = sc_arity_check(dom_create, 5)("div", "\uEBADstyle", BgL_sc_const_21z00_hop_exception, stmp_22, stmp_19);
            stmp_18 = g1377;
          } else {
            stmp_18 = false;
          }
          g1376 = sc_arity_check(dom_create, 6)("div", "\uEBADstyle", const_hop_exception, stmp_18, stmp_17, stmp_7);
          stmp_6 = g1376;
          g1375 = sc_arity_check(dom_create, 4)("td", "\uEBADstyle", BgL_sc_const_2z00_hop_exception, sc_arity_check(hop_properties_to_string, 1)(exc));
          stmp_27 = g1375;
          g1374 = sc_arity_check(dom_create, 2)("tr", stmp_27);
          stmp_26 = g1374;
          g1373 = sc_arity_check(dom_create, 4)("td", "\uEBADstyle", BgL_sc_const_19z00_hop_exception, src);
          stmp_29 = g1373;
          g1372 = sc_arity_check(dom_create, 2)("tr", stmp_29);
          stmp_28 = g1372;
          g1371 = sc_arity_check(dom_create, 4)("span", "\uEBADstyle", BgL_sc_const_22z00_hop_exception, name);
          stmp_32 = g1371;
          g1370 = sc_arity_check(dom_create, 6)("td", "\uEBADstyle", "font-size: 14pt", stmp_32, ": ", msg);
          stmp_31 = g1370;
          g1369 = sc_arity_check(dom_create, 2)("tr", stmp_31);
          stmp_30 = g1369;
          g1368 = sc_arity_check(dom_create, 4)("span", "\uEBADstyle", BgL_sc_const_7z00_hop_exception, location);
          stmp_35 = g1368;
          g1367 = sc_arity_check(dom_create, 4)("td", "\uEBADstyle", BgL_sc_const_15z00_hop_exception, stmp_35);
          stmp_34 = g1367;
          g1366 = sc_arity_check(dom_create, 2)("tr", stmp_34);
          stmp_33 = g1366;
          g1365 = sc_arity_check(dom_create, 7)("table", "\uEBADstyle", "width: 100%", stmp_33, stmp_30, stmp_28, stmp_26);
          stmp_25 = g1365;
          g1364 = sc_arity_check(dom_create, 4)("td", "\uEBADstyle", BgL_sc_const_13z00_hop_exception, stmp_25);
          stmp_24 = g1364;
          g1363 = sc_arity_check(dom_create, 5)("img", "\uEBADsrc", BgL_sc_const_5z00_hop_exception, "\uEBADalt", "Error");
          stmp_37 = g1363;
          g1361 = sc_arity_check(dom_create, 4)("td", "\uEBADstyle", BgL_sc_const_20z00_hop_exception, stmp_37);
          stmp_36 = g1361;
          g1359 = sc_arity_check(dom_create, 3)("tr", stmp_36, stmp_24);
          stmp_23 = g1359;
          g1357 = sc_arity_check(dom_create, 3)("col", "\uEBADwidth", "64px");
          stmp_39 = g1357;
          g1355 = sc_arity_check(dom_create, 2)("colgroup", stmp_39);
          stmp_38 = g1355;
          g1353 = sc_arity_check(dom_create, 5)("table", "\uEBADstyle", BgL_sc_const_18z00_hop_exception, stmp_38, stmp_23);
          stmp = sc_arity_check(hop_make_exception_frame, 2)(g1353, stmp_6);
          return sc_arity_check(dom_append_child, 2)(document.body, stmp);
        } else {
          return sc_arity_check(hop_window_onload_add, 1)((sc_tmp = function(e) {
                      return sc_arity_check(hop_report_exception, 1)(exc);
                    }, sc_tmp.name = "", sc_tmp.location = "(at hop-exception.scm 18167)", sc_tmp.sc_arity = 1, sc_tmp));
        }
      }
    }, sc_tmp.name = "hop-report-exception", sc_tmp.location = "(at hop-exception.scm 17702)", sc_tmp.sc_arity = 1, sc_tmp;
hop_last_exception_hop_exception = false;
sc_tmp = hop_onerror_handler_hop_exception = function(msg, url, line) {
      var tmp1387;
      var i;
      var g1386;
      var exc;
      var exc_49;
      var tmp1385;
      g1386 = hop_config.filtered_errors.length - 1;
      BgL_whilezd2break1456zd2: {
        i = g1386;
        while (i >= 0) {
          tmp1387 = url === hop_config.filtered_errors[i];
          if (tmp1387 !== false) {
            {
              tmp1385 = tmp1387;
              break BgL_whilezd2break1456zd2;
            }
          } else {
            --i;
          }
        }
        tmp1385 = false;
      }
      if (tmp1385 !== false) {
        return tmp1385;
      } else {
        if (hop_last_exception_hop_exception && hop_last_exception_hop_exception.message === msg) {
          exc = hop_last_exception_hop_exception;
        } else {
          exc_49 = new Error();
          exc_49.message = msg;
          exc_49.fileName = url;
          exc_49.lineNumber = line;
          exc_49.hopStack = sc_arity_check(hop_get_stack, 1)(2);
          exc = exc_49;
        }
        sc_arity_check(hop_report_exception, 1)(exc);
        return sc_arity_check(hop_debug, 0)() < 2;
      }
    }, sc_tmp.name = "hop-onerror-handler", sc_tmp.location = "(at hop-exception.scm 19188)", sc_tmp.sc_arity = 3, sc_tmp;
if (sc_arity_check(hop_debug, 0)() > 0) {
  sc_arity_check(sc_errorHookSet, 1)((sc_tmp = function(exc, _) {
        hop_last_exception_hop_exception = exc;
        exc.hopStack = sc_arity_check(hop_get_stack, 1)(3);
        return exc;
      }, sc_tmp.name = "", sc_tmp.location = "(at hop-exception.scm 20022)", sc_tmp.sc_arity = 2, sc_tmp));
  window.onerror = hop_onerror_handler_hop_exception;
}
