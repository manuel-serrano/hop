// testing redirections

import { log } from "./init-log.js";
import "./exporter2.js";

export * from "./exporter3.js";
export * from "./exporter4.js";

export const dummy = 5555;

log( "exporter.js", "exporter.js" );
