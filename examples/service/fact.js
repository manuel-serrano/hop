/*=====================================================================*/
/*    colin/phd/hop/examples/service/fact.js                           */
/*    -------------------------------------------------------------    */
/*    Author      :  Colin Vidal                                       */
/*    Creation    :  Tue Sep 08 13:59:31 2015                          */
/*    Last change :  Tue Sep 08 13:59:31 2015 (colin)                  */
/*    Copyright   :  2015 Colin Vidal                                  */
/*    -------------------------------------------------------------    */
/*    Service example                                                  */
/*    -------------------------------------------------------------    */
/*    run: hop -v -g fact.js                                           */
/*    browser: http://localhost:8080/hop/url                           */
/*=====================================================================*/

function computeFact(n) {
   if (n <= 1)
      return n;
   return computeFact(n - 1) * n;
}

service fact({n: 5}) {
   return computeFact(n);
}

service form() {
   var input = <input size="5" />;
   var result = <div></div>;

   var button = <button onclick=~{
      var inputVal = ${input}.value;
      ${fact}({n: inputVal}).post(function(res) {
	 ${result}.innerHTML = "fact(" + inputVal + ") = " + res
      })}>
      compute!
   </button>;

   return <html>
      ${input}
      ${button}
      ${result}
   </html>
}
