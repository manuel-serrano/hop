"use hopscript";

import { trace } from "./m6.mjs";
trace("m2");

export const _dummy1 = 1, _dummy2 = 2;
export { v3 as v2 } from "./m3.mjs";
export const _v2 = 222;
import { v3 } from "./m3.mjs";
trace("v3@m2="+ v3);
	  
