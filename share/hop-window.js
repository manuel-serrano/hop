/*=====================================================================*/
/*    serrano/prgm/project/hop/share/hop-window.js                     */
/*    -------------------------------------------------------------    */
/*    Author      :  Erick Gallesio                                    */
/*    Creation    :  Wed Mar  1 14:09:36 2006                          */
/*    Last change :  Mon Mar 20 19:54:45 2006 (eg)                     */
/*    -------------------------------------------------------------    */
/*    FLOAT-WINDOW implementation                                      */
/*=====================================================================*/

/* ====================================================================== *\
 * 	hop_float_window_init ...
\* ====================================================================== */
function hop_float_window_init(id, inframe)
{
    var el  = document.getElementById(id);
    var h   = document.getElementById(id + "-handle");
    var rsz = document.getElementById(id + "-rsz");

    HopDrag.init(el, h, rsz);
    el.inFrame = inframe;
}

/* ====================================================================== *\
 * 	hop_open_float_window ...
\* ====================================================================== */
function hop_open_float_window(serv, id, x, y)
{
    var win    = document.getElementById(id);
    var h      = document.getElementById(id + "-handle");
    var el     = document.getElementById(id + "-content");
    var around = id + "-around";
    var iframe = id + "-frame";


    function change_style() {
	win.style.display = "block";
	win.style.left = x;
	win.style.top  = y;
	win.style.zIndex = ++HopDrag.zIndex;
    }

    function compute_height() {
	var shadow_height = 8;
	return win.offsetHeight - h.offsetHeight - shadow_height;
    }

    function start_resize(w, h) {
	var id = (win.inFrame) ? iframe : around;

	document.getElementById(id).style.display= "none";
	el.style.overflow = "auto";
	el.style.opacity= 0.8;
    }
    
    function resize(w, h) {
	el.style.setProperty("height",  compute_height() +"px", "");
	el.style.setProperty("overflow",  "auto", "");	
    }

    function end_resize(w, h) {
	var id  = (win.inFrame) ? iframe : around;
	var tmp = document.getElementById(id);
	var sz  = compute_height() + "px";

	tmp.style.display= "block"; 
	tmp.style.height = sz;
	el.style.height = sz;
	el.style.opacity= 1;
    }

	    
    /* Set the resize functions */
    win.onResizeStart = start_resize;
    win.onResize      = resize;
    win.onResizeEnd   = end_resize;

//    if (serv == null) {
//	change_style();
//	return
//    }
    if (!win.inFrame) {
	hop(serv, 
	    function( http ) {
		if (http.responseText != null) {
		    document.getElementById(around).innerHTML = http.responseText;
		    hop_js_eval( http );
		    change_style();
		}
	    });
    } else {
	win.onDragStart = function(x, y) { 
	    document.getElementById(iframe).style.display= "none";  
	    el.style.setProperty("height", compute_height()+"px", "");
	    /* el.style.opacity= 0.8; */
	}

	win.onDragEnd   = function(x, y) {
	    document.getElementById(iframe).style.display= "block"; 
	    /* el.style.opacity= 1;  */
	}
	
	change_style();	
	el.innerHTML= "<iframe class=hop-float-iframe id='" + iframe + "' " +
	    "src='" + serv + "' " + 
	    "' height='" + compute_height() + "'></iframe>";
    }
}

/* ====================================================================== *\
 * 	hop_close_float_window ...
\* ====================================================================== */
function hop_close_float_window(id)
{
    var el = document.getElementById(id);
    el.style.display = "none";
}


/* ====================================================================== *\
 *
 * HopDrag Object
 * 
 * The following code is an adaptation of the dom-drag.js Drag object 
 * available at "http://www.youngpup.net/2001/domdrag/project"
 *
\* ====================================================================== */
var HopDrag = {
    obj     : null,
    opacity : 1,
    zIndex  : 100,

    init : function(o, h, rsz, oRoot, minX, maxX, minY, maxY, 
		    bSwapHorzRef, bSwapVertRef, fXMapper, fYMapper)
    {
	o.hop_handle    = h;
	o.rsz_handle	= rsz;
	h.hop_window    = o;
	rsz.hop_window  = o;
	h.onmousedown	= HopDrag.start;
	rsz.onmousedown = HopDrag.startResize;
	
	o.hmode		= bSwapHorzRef ? false : true ;
	o.vmode		= bSwapVertRef ? false : true ;
	
	o.root = oRoot && oRoot != null ? oRoot : o ;
	
	if (o.hmode  && isNaN(parseInt(o.root.style.left  ))) 
	    o.root.style.left   = "0px";
	if (o.vmode  && isNaN(parseInt(o.root.style.top   )))
	    o.root.style.top    = "0px";
	if (!o.hmode && isNaN(parseInt(o.root.style.right ))) 
	    o.root.style.right  = "0px";
	if (!o.vmode && isNaN(parseInt(o.root.style.bottom))) 
	    o.root.style.bottom = "0px";
	
	o.minX	= typeof minX != 'undefined' ? minX : null;
	o.minY	= typeof minY != 'undefined' ? minY : null;
	o.maxX	= typeof maxX != 'undefined' ? maxX : null;
	o.maxY	= typeof maxY != 'undefined' ? maxY : null;
	
	o.xMapper = fXMapper ? fXMapper : null;
	o.yMapper = fYMapper ? fYMapper : null;

	o.root.onDragStart	= new Function();
	o.root.onDrag		= new Function();
	o.root.onDragEnd	= new Function();
	o.root.onResizeStart	= new Function();
	o.root.onResize	        = new Function();
	o.root.onResizeEnd      = new Function();
    },
    
    start : function(e)
    {
	var o = this.hop_window;
	var y = parseInt(o.vmode ? o.root.style.top  : o.root.style.bottom);
	var x = parseInt(o.hmode ? o.root.style.left : o.root.style.right );

	e = HopDrag.fixE(e);
	HopDrag.obj = o;

	o.root.style.zIndex = ++HopDrag.zIndex;
	o.root.onDragStart(x, y);
	
	o.lastMouseX	= e.clientX;
	o.lastMouseY	= e.clientY;
	
	if (o.hmode) {
	    if (o.minX != null)	o.minMouseX	= e.clientX - x + o.minX;
	    if (o.maxX != null)	o.maxMouseX	= o.minMouseX + o.maxX - o.minX;
	} else {
	    if (o.minX != null) o.maxMouseX = -o.minX + e.clientX + x;
	    if (o.maxX != null) o.minMouseX = -o.maxX + e.clientX + x;
	}
	
	if (o.vmode) {
	    if (o.minY != null)	o.minMouseY	= e.clientY - y + o.minY;
	    if (o.maxY != null)	o.maxMouseY	= o.minMouseY + o.maxY - o.minY;
	} else {
	    if (o.minY != null) o.maxMouseY = -o.minY + e.clientY + y;
	    if (o.maxY != null) o.minMouseY = -o.maxY + e.clientY + y;
	}
	
	document.onmousemove	= HopDrag.drag;
	document.onmouseup	= HopDrag.end;
	
	return false;
    },

    drag : function(e)
    {
	e = HopDrag.fixE(e);
	var o = HopDrag.obj;
	
	var ey	= e.clientY;
	var ex	= e.clientX;
	var y = parseInt(o.vmode ? o.root.style.top  : o.root.style.bottom);
	var x = parseInt(o.hmode ? o.root.style.left : o.root.style.right );
	var nx, ny;
	
	if (o.minX != null) 
	    ex = o.hmode ? Math.max(ex, o.minMouseX) : Math.min(ex, o.maxMouseX);
	if (o.maxX != null) 
	    ex = o.hmode ? Math.min(ex, o.maxMouseX) : Math.max(ex, o.minMouseX);
	if (o.minY != null) 
	    ey = o.vmode ? Math.max(ey, o.minMouseY) : Math.min(ey, o.maxMouseY);
	if (o.maxY != null) 
	    ey = o.vmode ? Math.min(ey, o.maxMouseY) : Math.max(ey, o.minMouseY);

	nx = x + ((ex - o.lastMouseX) * (o.hmode ? 1 : -1));
	ny = y + ((ey - o.lastMouseY) * (o.vmode ? 1 : -1));
	
	if (o.xMapper)	     nx = o.xMapper(y)
	else if (o.yMapper)  ny = o.yMapper(x);
				    
	HopDrag.obj.root.style[o.hmode ? "left" : "right"] = nx + "px";
	HopDrag.obj.root.style[o.vmode ? "top" : "bottom"] = ny + "px";
	HopDrag.obj.lastMouseX	= ex;
	HopDrag.obj.lastMouseY	= ey;
	
	HopDrag.obj.root.onDrag(nx, ny);
	return false;
    },

    end : function()
    {
	document.onmousemove = null;
	document.onmouseup   = null;
	HopDrag.obj.root.onDragEnd(
	   parseInt(HopDrag.obj.root.style[HopDrag.obj.hmode ? "left" : "right"]), 
	   parseInt(HopDrag.obj.root.style[HopDrag.obj.vmode ? "top" : "bottom"]));
	HopDrag.obj = null;
    },

    /* ============== *\
     *    Resize 
    \* ============== */       
    startResize : function(e)
    {
	var o = this.hop_window;
	var x = parseInt(o.hmode ? o.root.style.left : o.root.style.right );
	var y = parseInt(o.vmode ? o.root.style.top  : o.root.style.bottom);

	e = HopDrag.fixE(e);
	HopDrag.obj = o;

	o.root.style.zIndex = ++HopDrag.zIndex;
	o.root.onResizeStart(o.root.style["width"],
			     o.root.style["height"]);
	
	o.lastMouseX	= e.clientX;
	o.lastMouseY	= e.clientY;
	
	document.onmousemove	= HopDrag.resize;
	document.onmouseup	= HopDrag.endResize;
	return false;
    },

    resize: function (e)
    {
	e = HopDrag.fixE(e);
	var o = HopDrag.obj;

	var ex	= e.clientX;
	var ey	= e.clientY;
	var x = parseInt(o.hmode ? o.root.style.left : o.root.style.right );	
	var y = parseInt(o.vmode ? o.root.style.top  : o.root.style.bottom);
	var w, h; 
	
	w = HopDrag.obj.offsetWidth  + ex - HopDrag.obj.lastMouseX;
	h = HopDrag.obj.offsetHeight + ey - HopDrag.obj.lastMouseY; 
	
	HopDrag.obj.root.style["width"]  = w + "px";
	HopDrag.obj.root.style["height"] = h + "px";
	
	HopDrag.obj.lastMouseX	= ex;
	HopDrag.obj.lastMouseY	= ey;
	
	HopDrag.obj.root.onResize(w, h);
	return false;
    },

    endResize : function()
    {
	document.onmousemove = null;
	document.onmouseup   = null;
	HopDrag.obj.root.onResizeEnd(parseInt(HopDrag.obj.root.style["width"]), 
				     parseInt(HopDrag.obj.root.style["height"]));
	HopDrag.obj = null;
	return false;
    },

    fixE : function(e)
    {
	if (typeof e == 'undefined') e = window.event;
	if (typeof e.layerX == 'undefined') e.layerX = e.offsetX;
	if (typeof e.layerY == 'undefined') e.layerY = e.offsetY;
	return e;
    }
};
