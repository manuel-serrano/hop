/*=====================================================================*/
/*    serrano/prgm/project/hop/hop/test/hopts/reactgo.ts               */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Apr 26 12:18:40 2021                          */
/*    Last change :  Thu Apr 29 10:01:37 2021 (serrano)                */
/*    Copyright   :  2021 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Testing core TypeScript features from:                           */
/*       https://reactgo.com/typescript-beginners-tutorial/            */
/*=====================================================================*/
import * as assert from 'assert';

/*---------------------------------------------------------------------*/
/*    dummy ...                                                        */
/*---------------------------------------------------------------------*/
function dummy() {
   let dummy: string = "Hello Boss";

   return dummy === "Hello " + "Boss";
}

console.log( "   dummy()"); assert.ok( dummy(), "dummy" );

/*---------------------------------------------------------------------*/
/*    annotations                                                      */
/*---------------------------------------------------------------------*/
function annotations() {
   let welcome:string = "welcome to reactgo.com";
   let name: string = `Gowtham`;

   let a: number = 1;

   let isActive: boolean = true;

   let fruits:string[] = ['apples','apricots','avocados'];

   let fruits2:Array<string> = ['apples','apricots','avocados'];

   let numbers:Array<number> = [1,2,3,4,5,6,7,8];

   return a === numbers[ 0 ] && fruits[ 1 ] === fruits2[ 1 ];
}

console.log( "   annotations()"); assert.ok( annotations(), "annotations" );

/*---------------------------------------------------------------------*/
/*    tuple ...                                                        */
/*---------------------------------------------------------------------*/
function tuple() {
   // we declared a tuple
   let user:[string,number,string];

   // intializing the values
   user = ['baby',33,'programming']; // TypeScript is happy now

   return user[0] === "baby" && user[1] === 33 && user[2] === "programming";
}

console.log( "   tuple()"); assert.ok( tuple(), "tuple" );

/*---------------------------------------------------------------------*/
/*    any                                                              */
/*---------------------------------------------------------------------*/
function any() {
   let isActive:any =  false; // boolean

   isActive = "Do your work"; // string

   return typeof( isActive ) === "string";
}

console.log( "   any()"); assert.ok( any(), "any" );

/*---------------------------------------------------------------------*/
/*    function                                                         */
/*---------------------------------------------------------------------*/
function fun() {
   function add(a:number,b:number):number{
      return a+b;
   }

   return add( 1, 2.0 ) == 3;
}

console.log( "   fun()"); assert.ok( fun(), "fun" );

/*---------------------------------------------------------------------*/
/*    optional ...                                                     */
/*---------------------------------------------------------------------*/
function optional() {
   function welcome(name: string, age?: number): string {
    if (name && age) {
        return `Welcome ${name} and ${age}`
    } else {
        return `Welcome ${name}`
    }
}

   welcome('gowtham'); // ok
   welcome('gowtham',22); // ok

   return welcome('gowtham', 22) === `Welcome gowtham and 22`;
}

console.log( "   optional()"); assert.ok( optional(), "optional" );

/*---------------------------------------------------------------------*/
/*    def ...                                                          */
/*---------------------------------------------------------------------*/
function def() {
   function getLength(arr: number[], length = arr.length) {
    return length
   }

   return getLength([1,2,3,4,5]) === 5;
}

console.log( "   def()"); assert.ok( def(), "def" );

/* {*---------------------------------------------------------------------*} */
/* {*    never ...                                                        *} */
/* {*---------------------------------------------------------------------*} */
/* function never() {                                                  */
/*    // throw an exception                                            */
/*    function error(err:string):never{                                */
/*       throw new Error(err);                                         */
/*    }                                                                */
/*                                                                     */
/*    try {                                                            */
/*       error( "foo" );                                               */
/*    } catch( e ) {                                                   */
/*       return true;                                                  */
/*    }                                                                */
/* }                                                                   */
/*                                                                     */
/* console.log( "   never()"); assert.ok( never(), "never" );          */
/*                                                                     */
/* {*---------------------------------------------------------------------*} */
/* {*    intf ...                                                         *} */
/* {*---------------------------------------------------------------------*} */
/* function intf() {                                                   */
/*    interface User {                                                 */
/*       name: string,                                                 */
/*       active: boolean                                               */
/*    }                                                                */
/*                                                                     */
/*    let user: User = {                                               */
/*       name: "gowtham",                                              */
/*       active: true                                                  */
/*    }                                                                */
/*                                                                     */
/*    // extending the User interface                                  */
/*    interface AdminUser extends User {                               */
/*       id: string                                                    */
/*    }                                                                */
/*                                                                     */
/*    // admin object  should have properties                          */
/*    //present in User interface + AdminUser interface                */
/*    let admin: AdminUser = {                                         */
/*       name: "john",                                                 */
/*       active: true,                                                 */
/*       id: "3232hejdjdjf"                                            */
/*    }                                                                */
/*                                                                     */
/*    return user.name === "gowtham" && user.active && typeof( admin.id ) === "string"; */
/* }                                                                   */
/*                                                                     */
/* console.log( "   intf()"); assert.ok( intf(), "intf" );             */
/*                                                                     */
/* {*---------------------------------------------------------------------*} */
/* {*    enum ...                                                         *} */
/* {*---------------------------------------------------------------------*} */
/* function enu() {                                                    */
/* {*    enum Day {                                                       *} */
/* {*       Monday,                                                       *} */
/* {*       Tuesday,                                                      *} */
/* {*       Wednesday                                                     *} */
/* {*    };                                                               *} */
/* {*                                                                     *} */
/* {*    enum Colors {                                                    *} */
/* {*      Red = "#FF0000",                                               *} */
/* {*      Green= "#008000",                                              *} */
/* {*      Blue = "#0000FF"                                               *} */
/* {*    }                                                                *} */
/* {*                                                                     *} */
/* {*    return Day.Tuesday === 1 && Colors.Blue === "#0000FF";           *} */
/* }                                                                   */

/* console.log( "   enu()"); assert.ok( enu(), "enu" );                */
