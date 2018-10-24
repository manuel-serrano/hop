// testing re-exports

import { log } from "./init-log.js";
import "./exporter2.js";

export * from "./exporter3.js";
export * from "./exporter4.js";

log( "exporter.js", "exporter.js" );
