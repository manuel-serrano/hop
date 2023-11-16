/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/nodejs/modules/fs.mod.js            */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Feb 20 20:10:40 2023                          */
/*    Last change :  Tue Feb 21 08:34:11 2023 (serrano)                */
/*    Copyright   :  2023 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    fs.js es6 module                                                 */
/*=====================================================================*/
"use hopscript";

/*---------------------------------------------------------------------*/
/*    The nodejs module                                                */
/*---------------------------------------------------------------------*/
const fs = require("fs");

const Stats = fs.Stats;
const exists = fs.exists;
const existsSync = fs.existsSync;
const readFile = fs.readFile;
const readFileSync = fs.readFileSync;
const close = fs.close;
const closeSync = fs.closeSync;
const open = fs.open;
const openSync = fs.openSync;
const read = fs.read;
const readSync = fs.readSync;
const write = fs.write;
const writeSync = fs.writeSync;
const rename = fs.rename;
const renameSync = fs.renameSync;
const truncate = fs.truncate;
const truncateSync = fs.truncateSync;
const ftruncate = fs.ftruncate;
const ftruncateSync = fs.ftruncateSync;
const rmdir = fs.rmdir;
const rmdirSync = fs.rmdirSync;
const fdatasync = fs.fdatasync;
const datasynSync = fs.datasynSync;
const fsync = fs.fsync;
const fsyncSync = fs.fsyncSync;
const mkdir = fs.mkdir;
const mkdirSync = fs.mkdirSync;
const readdir = fs.readdir;
const readdirSync = fs.readdirSync;
const fstat = fs.fstat;
const fstatSync = fs.fstatSync;
const lstat = fs.lstat;
const lstatSync = fs.lstatSync;
const stat = fs.stat;
const statSync = fs.statSync;
const readlink = fs.readlink;
const readlinkSync = fs.readlinkSync;
const symlink = fs.symlink;
const symlinkSync = fs.symlinkSync;
const link = fs.link;
const linkSync = fs.linkSync;
const unlink = fs.unlink;
const unlinkSync = fs.unlinkSync;
const fchmod = fs.fchmod;
const fchmodSync = fs.fchmodSync;
const chmod = fs.chmod;
const chmodSync = fs.chmodSync;
const fchown = fs.fchown;
const fchownSync = fs.fchownSync;
const toUnixTimestamp = fs.toUnixTimestamp;
const utimes = fs.utimes;
const utimesSync = fs.utimesSync;
const futimes = fs.futimes;
const futimesSync = fs.futimesSync;
const writeFile = fs.writeFile;
const writeFileSync = fs.writeFileSync;
const appendFile = fs.appendFile;
const appendFileSync = fs.appendFileSync;
const watch = fs.watch;
const watchFile = fs.watchFile;
const unwatchFile = fs.unwatchFile;
const realpath = fs.realpath;
const realpathSync = fs.realpathSync;
const createReadStream = fs.createReadStream;
const ReadStream = fs.ReadStream;
const fileReadStream = fs.fileReadStream;
const createWriteStream = fs.createWriteStream;
const WriteStream = fs.WriteStream;
const FileWriteStream = fs.FileWriteStream;
const SyncWriteStream = fs.SyncWriteStream;
const copyFile = fs.copyFile;
const copyFileSync = fs.copyFileSync;
const constants = fs.constants;

/*---------------------------------------------------------------------*/
/*    exports ...                                                      */
/*---------------------------------------------------------------------*/
module.exports = fs;
export { 
   Stats, 
   exists, existsSync,
   readFile, readFileSync,
   close, closeSync,
   open, openSync,
   read, readSync,
   write, writeSync,
   rename, renameSync,
   truncate, truncateSync,
   ftruncate, ftruncateSync,
   rmdir, rmdirSync,
   fdatasync, datasynSync,
   fsync, fsyncSync,
   mkdir, mkdirSync,
   readdir, readdirSync,
   fstat, fstatSync,
   lstat, lstatSync,
   stat, statSync,
   readlink, readlinkSync,
   symlink, symlinkSync,
   link, linkSync,
   unlink, unlinkSync,
   fchmod, fchmodSync,
   chmod, chmodSync,
   fchown, fchownSync,
   toUnixTimestamp,
   utimes, utimesSync,
   futimes, futimesSync,
   writeFile, writeFileSync,
   appendFile, appendFileSync,
   watch,
   watchFile,
   unwatchFile,
   realpath, realpathSync,
   createReadStream,
   ReadStream,
   fileReadStream,
   createWriteStream,
   WriteStream,
   FileWriteStream,
   SyncWriteStream,
   copyFile,
   copyFileSync,
   constants
};

