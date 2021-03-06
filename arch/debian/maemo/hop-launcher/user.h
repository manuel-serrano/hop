/*=====================================================================*/
/*    serrano/prgm/project/hop/linux/maemo/hop-launcher/user.h         */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Mon Dec 17 11:27:58 2007                          */
/*    Last change :  Mon Dec 17 12:01:17 2007 (serrano)                */
/*    Copyright   :  2007 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Dealing with users                                               */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    user type                                                        */
/*---------------------------------------------------------------------*/
typedef struct user {
   char *name;
   uid_t uid;
} user_t;

/*---------------------------------------------------------------------*/
/*    Global declarations                                              */
/*---------------------------------------------------------------------*/
extern user_t *current_user;
extern user_t *hop_user;
extern user_t *selected_user;

extern void init_users();


   
