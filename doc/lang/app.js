function TODOLIST( attrs, body ) {
   return <ul>
     <react>~{${attrs.items}.map( createItem )}</react>
   </ul>;
}

service todolist() {
   return <html>
     ~{
	function handleSubmit( e ) {
	   e.preventDefault();
	   state.items.push( { text: state.text, id: Date.now() } );
	}

	function onChange( e ) {
	   state.text = e.target.value;
	}

	function createItem( item ) {
	   return <li key=${item.id}> ${item.text} </li>;
	}
	
	var state = ( { items: new hop.reactProxy( [] ), text: '' } );
     }
	
     <div>
       <h3>TODO</h3>
       <TodoList items=~{state.items}/>
       <form onSubmit=~{handleSubmit( event )}>
	 <input onchange=~{onChange( event )}/>
	 <button>Add #<react>~{state.items.length + 1}</react></button>
       </form>
     </div>
   </html>
}
