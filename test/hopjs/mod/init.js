// test plain module initialization

import { log } from "./init-log.js";
import "./subinit.js";

log( "init.js", "init.js" );
