/*=====================================================================*/
/*    serrano/prgm/project/hop/linux/maemo/hop-launcher/gui.h          */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Sun Dec 16 07:58:09 2007                          */
/*    Last change :  Wed Jan 16 08:35:08 2008 (serrano)                */
/*    Copyright   :  2007-08 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Functions that deal with the GUI.                                */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    Exports                                                          */
/*---------------------------------------------------------------------*/
extern HildonWindow *make_hop_gui();
extern void clear_hop_console();
extern void hop_console_insert( char *, int );
extern void hop_console_adjust();
extern void gui_start_waiting( char * ), gui_stop_waiting();
extern void gui_message( char * );

extern HildonProgram *program;
extern HildonWindow *win;
