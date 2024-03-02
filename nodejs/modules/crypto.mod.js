/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/nodejs/modules/crypto.mod.js        */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Wed Feb 28 15:16:33 2024                          */
/*    Last change :  Wed Feb 28 15:28:08 2024 (serrano)                */
/*    Copyright   :  2024 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Nodejs crypto API                                                */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    Module declaration                                               */
/*---------------------------------------------------------------------*/
const crypto = require("crypto");

const DEFAULT_ENCODING = crypto.DEFAULT_ENCODING;
const Credentials = crypto.Credentials;
const createCredentials = crypto.createCredentials;
const createHash = crypto.createHash;
const createHmac = crypto.createHmac;
const createCipher = crypto.createCipher;
const createCipheriv = crypto.createCipheriv;
const createDecipher = crypto.createDecipher;
const createDecipheriv = crypto.createDecipheriv;
const createSign = crypto.createSign;
const createVerify = crypto.createVerify;
const createDiffieHellman = crypto.createDiffieHellman;
const DiffieHellmanGroup = crypto.DiffieHellmanGroup;
const pbkdf2 = crypto.pbkdf2;
const pbkdf2Sync = crypto.pbkdf2Sync;
const randomBytes = crypto.randomBytes;
const pseudoRandomBytes = crypto.pseudoRandomBytes;
const getCiphers = crypto.getCiphers;
const getHashes = crypto.getHashes;

/*---------------------------------------------------------------------*/
/*    exports                                                          */
/*---------------------------------------------------------------------*/
module.exports = crypto;
export {
   DEFAULT_ENCODING,
   Credentials,
   createCredentials,
   createHash,
   createHmac,
   createCipher,
   createCipheriv,
   createDecipher,
   createDecipheriv,
   createSign,
   createVerify,
   createDiffieHellman,
   DiffieHellmanGroup,
   pbkdf2,
   pbkdf2Sync,
   randomBytes,
   pseudoRandomBytes,
   getCiphers,
   getHashes
}
