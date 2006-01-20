;*=====================================================================*/
;*    serrano/prgm/project/hop/weblets/dired/param.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jan  6 18:15:05 2005                          */
;*    Last change :  Fri Oct  7 06:01:36 2005 (serrano)                */
;*    Copyright   :  2005 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Dired params                                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    User configurations                                              */
;*---------------------------------------------------------------------*/
(define *dired-suffix-aliases*
   '(("bgl" "scm")
     ("tar.gz" "tgz")
     ("tar.bz2" "tbz")
     ("zip" "tgz")
     ("ftex" "tex")
     ("zsh" "sh")
     ("fig" "fig")))

(define *dired-commands*
   '(("scm" (local ("emacs" "edit.png")))
     ("hop" (local ("emacs" "edit.png")))
     ("css" (local ("emacs" "edit.png")))
     ("avi" (local ("mplayer" "mplayer.png")))
     ("mp3" (local ("xmms" "xmms.png")))
     ("ogg" (local ("xmms" "xmms.png")))
     ("ps" (local ("gv" "gv.png") ("lpr" "print.png")))
     ("pdf" (local ("acroread" "display.png") ("xpdf" "misc.png") ("lpr" "print.png")))
     ("c" (local ("emacs" "edit.png")))
     ("h" (local ("emacs" "edit.png")))
     ("png" (local ("gimp-2.0" "draw.png")))
     ("gif" (local ("gimp-2.0" "draw.png")))))

(define *dired-hidden-directories*
   '())
