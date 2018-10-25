// test named default export

let o = { x: 0 };

export { o as oo }

export { o as default };

export function setX( v ) { o.z = v; }

o = { z: 10 };
