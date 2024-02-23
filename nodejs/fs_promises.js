/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/nodejs/fs_promises.js               */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Feb 23 08:17:55 2024                          */
/*    Last change :  Fri Feb 23 09:18:00 2024 (serrano)                */
/*    Copyright   :  2024 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    FS promise wrappers                                              */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    import                                                           */
/*---------------------------------------------------------------------*/
const fs = require("fs");

exports.appendFile = function(path, data, options) {
   return new Promise((res, rej) => {
      fs.appendFile(path, data, options, err => err ? rej(err) : res(true));
   });
}

exports.lstat = function(path, options) {
   return new Promise((res, rej) => {
      fs.lstat(path, options, (err, stats) => err ? rej(err) : res(stats));
   });
}

exports.readdir = function(path, options) {
   return new Promise((res, rej) => {
      fs.readdir(path, options, (err, data) => err ? rej(err) : res(data));
   });
}

exports.readFile = function(path, options) {
   return new Promise((res, rej) => {
      fs.readFile(path, options, (err, data) => err ? rej(err) : res(data));
   });
}

exports.rename = function(oldpath, newpath) {
   return new Promise((res, rej) => {
      fs.rename(oldpath, newpath, (err) => err ? rej(err) : res(true));
   });
}

exports.writeFile = function(path, data, options) {
   return new Promise((res, rej) => {
      fs.writeFile(path, data, options, (err) => err ? rej(err) : res(true));
   });
}

