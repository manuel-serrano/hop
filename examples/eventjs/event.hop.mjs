import * as hop from "@hop/hop";

hop.init({
   ports: { http: 8888 },
   users: [ {
      name: "anonymous", 
      services: ["/hop/server/event", "/hop/broadcast", "/hop/getServerDate"], 
      directories: ["@hop/hop"]
   } ]
});

service broadcast() {
   return <html>
      <script type="importmap"> {
	 "imports": {
	    "@hop/hop": "${hop.register('./client.mjs')}"
	 }
      }
      </script>
      <script type="module">
         import * as hop from "@hop/hop";
         hop.server.addEventListener("date", e => {
            document.getElementById("console").innerHTML =
	       e.value.getHours() + ":" + e.value.getMinutes() + ":" + e.value.getSeconds();
         });
         hop.server.addEventListener("open",
           e => document.getElementById("status").innerHTML = "connection open");
         hop.server.addEventListener("close",
           e => document.getElementById("status").innerHTML = "connection close");
         globalThis.getServerDate = ${getServerDate};
      </script>
      <body>
         <button onclick=~{getServerDate().post()}>refresh date</button>
         <div id="console">-</div>
         <div id="status">-</div>
      </body>
    </html>
}

service getServerDate() {
   hop.broadcast("date", new Date(Date.now()));
}

hop.listen(`"${broadcast()}" ready...%c%c`);

