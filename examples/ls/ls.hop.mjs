import { Hop } from "@hop/hop";
import * as fs from "node:fs";
import * as path from "node:path";

const hopConfig = {
   ports: { http: 8888 },
   users:  [ { name: "anonymous", services: ["/ls"] } ]
};

const hop = new Hop(hopConfig);
const R = hop.Resolver();

async function ls(o) {
   const dir = o.dir || "/tmp";
   const files = await fs.readdirSync(dir);
   
   return <html>
      <script type="module" src=${R.url('./client.mjs')}/>
      
      ${files
	 .filter(p => !p.match(/^\.|^#.*#$/))
	 .sort((x, y) => x >= y ? 1 : -1 )
	 .map(p => {
	    const ap = path.join(dir, p);

	    if (fs.statSync(ap).isDirectory()) {
	       return <div onclick=~{location = ${Ls}({ dir: ${ap} })}>${p}/</div>;
	    } else {
	       return <div>${p}</div>;
	    }
	 })}
   
   </html>;
}

const Ls = hop.Service(ls);
hop.listen().then(() => console.log(`${Ls()} ready...`));

