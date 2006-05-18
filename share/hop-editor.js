/*=====================================================================*/
/*    serrano/prgm/project/hop/share/hop-edit.js                       */
/*    -------------------------------------------------------------    */
/*    Author      :  Erick Gallesio                                    */
/*    Creation    :  Mon Apr 10 11:43:00 2006                          */
/*    Last change :  Tue May  9 15:38:12 2006 (eg)                     */
/*    -------------------------------------------------------------    */
/*    <EDITOR> JavaScript support                                      */
/*=====================================================================*/

var hop_edit_popups_dir;
var hop_edit_src;
var hop_edit_in_iframe = true; 

/*---------------------------------------------------------------------*/
/*    hop_edit_init ...                                                */
/*---------------------------------------------------------------------*/
function hop_edit_init(id, popups_dir, submit, cancel)
{
    var txt    = document.getElementById(id);
    var iframe = document.getElementById("hop-edit" + id);
    var doc    = iframe.contentWindow.document;

    hop_edit_src = false;

    // Copy the textarea content into the iframe
    doc.open()
    doc.write(txt.value);
    doc.close();

    // Store the submit/cancel function 
    iframe.hop_edit_submit = submit;
    iframe.hop_edit_cancel = cancel;

    // Make the iframe editable 
    doc.designMode = "on";
    
    // Save the popups directory
    hop_edit_popups_dir = popups_dir;

    // Install a listener fo KeyPress events
    iframe.contentDocument.addEventListener("keypress", 
					    function(e) {
						hop_edit_keypress_hdlr(e, id);
					    },
					    true);
    // Install an event hdler to copy back iframe content to textarea if submitting
    for (var i=0; i < document.forms.length; i++) {
    	document.forms[i].addEventListener("submit",
					   function (e) { 
					       hop_edit_update_textarea(id); 
					   },
					   true);
    }
    // Initialize Selection boxes
    hop_edit_style_set(id, "p");
    hop_edit_font_set(id, "Sans Serif");
    hop_edit_fontsize_set(id, 1);
    hop_edit_update_interface(id);
    
    // Launch the handler in charge of interface updating
    window.setInterval(function() {hop_edit_update_interface(id)}, 250);
}

/*---------------------------------------------------------------------*/
/*    hop_edit_keypress_hdlr ...                                       */
/*---------------------------------------------------------------------*/
function hop_edit_keypress_hdlr(e, id)
{
    if (e.ctrlKey) {
	var propagate=false,
	    char=e.charCode;
	switch (char) {
	  case 98:  hop_edit_action(id, "bold"); break;
	  case 105: hop_edit_action(id, "italic"); break;
	  case 117: hop_edit_action(id, "underline"); break;
	  case 113: hop_edit_update_interface2(id); break;
	  default: propagate = true;
	}
	if (!propagate) {
	    e.preventDefault();
	    e.stopPropagation();
	}
    }
}

/*---------------------------------------------------------------------*/
/*    hop_edit_action ...                                              */
/*---------------------------------------------------------------------*/
function hop_edit_action(id, action, value)
{
    var iframe = document.getElementById("hop-edit" + id);

    if (action == "viewsource")
	hop_edit_view_source(id);
    else {
	if (hop_edit_src) return;

	if (action == "insertlink")
	    hop_edit_insert_link(id);
	else if (action == "insertimage")
	    hop_edit_insert_image(id);
	else if (action == "inserttable") 
	    alert("TODO");
	else if (action == "forecolor" || action == "hilitecolor")
	    hop_edit_palette(id, action);
	else if (action == "submit") {
	    hop_edit_update_textarea(id); 
	    (eval(iframe.hop_edit_submit)) ( id );
	} else  if (action == "cancel") {
	    (eval(iframe.hop_edit_cancel)) ( id );
	} else
	    iframe.contentDocument.execCommand(action, false, value);
    }
}


/*---------------------------------------------------------------------*/
/*    hop_edit_style_set ...                                           */
/*---------------------------------------------------------------------*/
function hop_edit_style_set(id, value)
{
    var iframe = document.getElementById("hop-edit" + id);
    
    iframe.contentDocument.execCommand("formatblock", false, '<'+value+'>');
}

/*---------------------------------------------------------------------*/
/*    hop_edit_font_set ...                                            */
/*---------------------------------------------------------------------*/
function hop_edit_font_set(id, value)
{
    var iframe = document.getElementById("hop-edit" + id);
    var action = (value == " --font--") ? "removeformat" : "fontname";

    iframe.contentDocument.execCommand(action, false, value);
}

/*---------------------------------------------------------------------*/
/*    hop_edit_fontsize_set ...                                            */
/*---------------------------------------------------------------------*/
function hop_edit_fontsize_set(id, value)
{
    var iframe = document.getElementById("hop-edit" + id);

    iframe.contentDocument.execCommand("fontsize", false, value);
}

/*---------------------------------------------------------------------*/
/*    hop_edit_open_window ...                                         */
/*---------------------------------------------------------------------*/
function hop_edit_open_window(url, name, width, height)
{
    window.open(url, name, "location=0,status=0,scrollbars=0,width=" + 
		width + ',height=' + height);
}

/*---------------------------------------------------------------------*/
/*    hop_edit_insert_link ...                                         */
/*---------------------------------------------------------------------*/
function hop_edit_insert_link(id) 
{
    var iframe = document.getElementById("hop-edit" + id);

    if (iframe.contentDocument.getSelection() == "")
	alert("Selection is not available");
    else {
	document.hop_edit_id_link=id; // Retain the id which opens the window
	hop_edit_open_window(hop_edit_popups_dir + "/link.hop", 
			     "hop_edit_link", 400, 200);
    }
}

/*---------------------------------------------------------------------*/
/*    hop_edit_accept_link ...                                         */
/*---------------------------------------------------------------------*/
function hop_edit_accept_link(url)
{
    var value  = document.getElementById("hop-edit-url").value;
    var id     = window.opener.document.hop_edit_id_link;
    var iframe = window.opener.document.getElementById("hop-edit" + id);
    var cmd    = (value == "") ? "unlink" : "createlink";

    iframe.contentDocument.execCommand(cmd, false, value);
    window.close();
}

/*---------------------------------------------------------------------*/
/*    hop_edit_insert_image ...                                        */
/*---------------------------------------------------------------------*/
function hop_edit_insert_image(id) 
{
    var iframe = document.getElementById("hop-edit" + id);
    document.hop_edit_id_image=id; // Retain the id which opens the window
    hop_edit_open_window(hop_edit_popups_dir + "/image.hop", 
			 "hop_edit_image", 400, 200);
}


/*---------------------------------------------------------------------*/
/*    hop_edit_accept_image ...                                        */
/*---------------------------------------------------------------------*/
function hop_edit_accept_image(url)
{
    var value  = document.getElementById("hop-edit-url").value;
    var id     = window.opener.document.hop_edit_id_image;
    var iframe = window.opener.document.getElementById("hop-edit" + id);

    iframe.contentDocument.execCommand("insertimage", false, value);
    window.close();
}

/*---------------------------------------------------------------------*/
/*    hop_edit_palette ...                                             */
/*---------------------------------------------------------------------*/
function hop_edit_palette(id, action)
{
    var iframe = document.getElementById("hop-edit" + id);
    var pal    = document.getElementById(id + "-palette");
    var but    = document.getElementById(id + "-but-" + action);

    pal.style.display = "block";
    pal.style.left = hop_element_x(but) - 20;
    pal.style.top  = hop_element_y(but) + but.height ;
    // Retain the action to do
    iframe.hop_colorize_action = action;
}

/*---------------------------------------------------------------------*/
/*    hop_edit_colorize ...                                            */
/*---------------------------------------------------------------------*/
function hop_edit_colorize(id, color)
{
    var iframe = document.getElementById("hop-edit" + id);
    var action = iframe.hop_colorize_action;
    
    iframe.contentDocument.execCommand(action, false, color);
}

/*---------------------------------------------------------------------*/
/*    hop_edit_submit/cancel ...                                       */
/*---------------------------------------------------------------------*/
function hop_edit_submit_cancel(id, abort)
{
    var iframe = document.getElementById("hop-edit" + id);
    
    for (el = iframe; el ; el = el.parentNode) {
	if (el instanceof HTMLFormElement) { 
	    //	var but = document.createElement("button"); 
	    //	but.value = value;
	    //	but.name = "abort";
	    //	but.style.display = "none"; 
	    //	el.appendChild(but);
	    //	but.click();
	    if (abort)
		iframe.contentDocument.body.innerHTML = "";
	    el.submit();
	    break;
	} 
    }
}

function hop_edit_submit(id) { hop_edit_submit_cancel(id, false); }

function hop_edit_cancel(id) { hop_edit_submit_cancel(id, true); }


/*---------------------------------------------------------------------*/
/*    hop_edit_update_textarea ...                                     */
/*---------------------------------------------------------------------*/
function hop_edit_update_textarea(id)
{
    var iframe = document.getElementById("hop-edit" + id);
    var txt    = document.getElementById(id);

    txt.value = iframe.contentDocument.body.innerHTML;
}

/*---------------------------------------------------------------------*/
/*    hop_edit_view_source ...                                         */
/*---------------------------------------------------------------------*/
function hop_edit_view_source(id)
{
    var iframe = document.getElementById("hop-edit" + id);
    var txt    = document.getElementById(id);
    var but    = document.getElementById(id + "-but-viewsource");
    var butAct = but.onclick;
    var butSrc = but.src;

    // Swap Iframe and Textarea
    iframe.style.display = "none";
    txt.value = iframe.contentDocument.body.innerHTML;
    txt.style.display = "block";

    // Make select boxes and buttons inative
    var styles = document.getElementById("hop-edit-styles-" + id);
    var fonts  = document.getElementById("hop-edit-fonts-" + id);
    var fsizes = document.getElementById("hop-edit-fs-" + id);

    styles.disabled  = true;
    fonts.disabled   = true;
    fsizes.disabled  = true;
    hop_edit_src     = true;	// sufficient for all the buttons

    // Change the button for restauring text
    but.src = hop_edit_popups_dir + "/ed-nohtml.png";
    
    // Change the binding of the view source button
    but.onclick = function(event) {
	iframe.contentDocument.body.innerHTML = txt.value;
	txt.style.display = "none";
	iframe.style.display = "block";

	but.onclick      = butAct;
	but.src          = butSrc;

	styles.disabled  = false;
	fonts.disabled   = false;
	fsizes.disabled  = false;
	hop_edit_src     = false;
    }
}

/*---------------------------------------------------------------------*/
/*    hop_edit_update_interface ...                                    */
/*---------------------------------------------------------------------*/

function hop_edit_update_interface(id)
{
    if (hop_edit_in_iframe) {
	var doc  = document.getElementById("hop-edit" + id).contentDocument;
	var font = doc.queryCommandValue("fontname");
	var size = doc.queryCommandValue("fontsize"); 
	var fmt  = doc.queryCommandValue("formatblock"); 
	
	// Get Back Style, Font and Size ans manage Firefox irregularities
	if (fmt  == "" || fmt == "p") fmt  = "Paragraph";
	if (fmt == "address") fmt = "Address";
	if (fmt[0] == "h") fmt = "Title " + fmt[1];

	if (font == "" || font == "sans-serif") font = "Sans Serif";
	if (font == "tt") font = "Monospace";
	
	if (size == "") size = "3";
	
	// Set the selection boxes to the correct values
	document.getElementById("hop-edit-fs-" + id).value = size;
	document.getElementById("hop-edit-fonts-" + id).value = font;
	document.getElementById("hop-edit-styles-" + id).value = fmt;

	//    alert("FMT /" +font+ "/" + size + "/" + fmt + "/");
    }
}


function hop_edit_update_interface2(id)
{
    if (hop_edit_in_iframe) {
	var doc  = document.getElementById("hop-edit" + id).contentDocument;
	var font = doc.queryCommandValue("fontname");
	var size = doc.queryCommandValue("fontsize"); 
	var fmt  = doc.queryCommandValue("formatblock"); 
	
	alert("FMT /" +font+ "/" + size + "/" + fmt + "/");
    }
}