/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/nodejs/modules/process.mod.js       */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Nov 19 08:48:52 2023                          */
/*    Last change :  Fri Feb 23 09:04:38 2024 (serrano)                */
/*    Copyright   :  2023-24 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    PROCESS es6 module                                               */
/*=====================================================================*/
"use hopscript";

/*---------------------------------------------------------------------*/
/*    The nodejs module                                                */
/*---------------------------------------------------------------------*/
const process = require("process");

const abort = process.abort;
const arch = process.arch;
const argv = process.argv;
const chdir = process.chdir;
const compilerOptions = process.compilerOptions;
const cwd = process.cwd;
const env = process.env;
const execArgv = process.execArgv;
const execPath = process.execPath;
const exit = process.exit;
const features = process.features;
const getgid = process.getgid;
const getgroups = process.getgroups;
const getuid = process.getuid;
const hrtime = process.hrtime;
const ioctl = process.ioctl;
const memoryUsage = process.memoryUsage;
const pid = process.pid;
const platform = process.platform;
const reallyExit = process.reallyExit;
const release = process.release;
const setgid = process.setgid;
const setuid = process.setuid;
const stderr = process.stderr;
const stdin = process.stdin;
const stdout = process.stdout;
const title = process.title;
const umask = process.umask;
const uptime = process.uptime;
const version = process.version;
const versions = process.versions;

/*---------------------------------------------------------------------*/
/*    exports ...                                                      */
/*---------------------------------------------------------------------*/
module.exports = process;
export { 
   abort,
   arch,
   argv,
   chdir,
   compilerOptions,
   cwd,
   env,
   execArgv,
   execPath,
   exit,
   features,
   getgid,
   getgroups,
   getuid,
   hrtime,
   ioctl,
   memoryUsage,
   pid,
   platform,
   reallyExit,
   release,
   setgid,
   setuid,
   stderr,
   stdin,
   stdout,
   title,
   umask,
   uptime,
   version,
   versions,
};
   
