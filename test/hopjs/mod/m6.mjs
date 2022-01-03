"use strict";

let buf = "";

export function trace(msg) { buf += " "; buf += msg; }
export function get() { return buf; }
