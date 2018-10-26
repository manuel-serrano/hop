// used to test variable name exports

const KONST = 990;
let VAR = 0;

VAR++; VAR = 9;

const mutator = function( v ) { VAR = v };
   
export const KONST2 = 45;
export let VAR2 = 44;

export function GET() { return KONST2 + VAR2 };

const checksum = KONST + VAR + GET();

export { KONST, VAR, KONST as K2, mutator };
export { checksum };

const def = {a: 1, b: 2};
export default def;
def.a = 10;
