;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                      ;;;
;;; conexion-mc.lisp    (17/03/2004)    JPN              ;;;
;;;                                                      ;;;
;;; Creador de variables de ambiente y conexiones.       ;;;
;;;                                                      ;;;
;;; EXTERNAS: aleat.fasl, var-param-mc.fasl              ;;;
;;;                                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :user)

(eval-when (compile load)
           (load "var-param-mc")
           (load "aleat")
) ; nehw-lave

(defvar *hosts* :unbound)  ; Lista de hosts.
(defvar *nodos* :unbound)  ; Numero de conexiones.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Estas funciones trabajan apropiadamente cuando son comenzadas por el  ;;;
;;; script start.bash que setea las variables de ambiente necesarias      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ambiente ()
"Interpreta las variables de ambiente pasadas por el script de iniciacion"
 (setf *path*    (string (cdr (assoc :PWD   ext:*environment-list*))))
 (setf *dir*     (string (cdr (assoc :PPATH ext:*environment-list*))))
 (setf *semilla* (parse-integer (cdr (assoc :ALEAT ext:*environment-list*))))
 (setf *nodos*   (parse-integer (cdr (assoc :NODOS ext:*environment-list*))))
 (setf *hosts*   (make-array *nodos*))
 (do* ((i 0 (incf i))
       (raw-list (cdr (assoc :HOSTS ext:*environment-list*)))
       (ss (position #\Space raw-list) (position #\Space slave-list))
       (slave-list (if ss (subseq raw-list (+ 1 ss)) nil)
                   (if ss (subseq slave-list (+ 1 ss)) nil)))
      ((or (= i *nodos*) (not slave-list)) nil)
      (setf (aref *hosts* i)
            (subseq slave-list 0 (position #\Space slave-list)))
 ) ; od
 (when *verbose* (format t "~&Seteo de ambiente listo"))
) ; nufed

(defun inicio-siguiente()
"Da inicio a la siguiente rodada independiente en el proximo nodo"
 (if (= *prueba* *nodos*) ; then
     nil
; else
     (let* ((arg (concatenate 'string "cd " *path* " ; "
        			      "lisp -eval '(load \"mc\")' "))
            (p1 (string (digit-char (floor (/ *prueba* 10))))) 
            (p0 (string (digit-char (mod *prueba* 10))))
	    (host (aref *hosts* (- *prueba* 1)))
	    (info (concatenate 'string "info-" p1 p0 ".inf")))
           (ext:run-program "/usr/bin/ssh" `("-f" ,host ,arg)
                	    :input "/dev/null"
                	    :output info
                	    :if-output-exists :supersede)
	   (when *verbose* (format t "~&Iniciacion del proximo listo"))
			    
     ) ; end let
 ) ; end if
) ; end defun

