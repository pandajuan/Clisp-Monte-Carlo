;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                   ;;;
;;; sistema0-mc.lisp    (18/03/2004)    JPN                           ;;;
;;;                                                                   ;;;
;;; Sistema inicial o recuperacion de rodada anterior.                ;;;
;;;                                                                   ;;;
;;; EXTERNAS: var-param-mc.fasl definiciones-mc.fasl aleat.fasl       ;;; 
;;;           redux-mc.fasl                                           ;;;
;;;                                                                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :user)

(eval-when (compile load)
           (load "aleat")
           (load "var-param-mc")
           (load "definiciones-mc")
           (load "redux-mc")
) ; nehw-lave

(defun zero ()
"Crea un sistema apropriado aleatoriamente. Todos los individuos estan a la
misma temperatura de manera que el annealing se aplique en el pre-calentamiento"
 (if (probe-file *recover*) ; then
     (progn
        (on-swt)
          (with-open-file (stream *recover*)
            (dotimes (i *Ntmp*)
              (let* ((g0 (read stream)) (g1 (read stream))
	             (g2 (read stream)) (g3 (read stream))
		     (bt (read stream)) (nb (read stream)) 
		     (nv (read stream)) (lista (read stream))
		     (nnb (nbeta bt)) (f (+ g0 g1 g2 g3)))
	            (setf (aref *gama0* i) g0)
	            (setf (aref *gama1* i) g1)
	            (setf (aref *gama2* i) g2)
	            (setf (aref *gama3* i) g3)
                    (setf (aref *sistema* i)
	                  (make-individuo :modula lista :sfit f :nbeta nnb
		                          :ncmb nb :ncmv nv :beta bt))
	      ) ; end let
            ) ; end dotimes
          ) ; close stream
	(off-swt)
        (when *verbose* (format t "~&Sistema0 de Recover listo"))
     ) ; end progn
; else
     (do* ((i 0 (incf i)) (listc (lista0 *prof0*) (lista0 *prof0*))
           (listb (red listc) (red listc))
	   (lista (funcall *fitness* listb) (funcall *fitness* listb))
	   (f (apply #'+ lista) (apply #'+ lista)))
          ((= i (- *Ntmp* 1))
	   (progn
	    (setf (aref *gama0* i) (nth 0 lista))
	    (setf (aref *gama1* i) (nth 1 lista))
	    (setf (aref *gama2* i) (nth 2 lista))
	    (setf (aref *gama3* i) (nth 3 lista))
            (setf (aref *sistema* i)
	          (make-individuo :modula listc :sfit f :beta *bmin*))
            (when *verbose* (format t "~&Sistema0 de Scratch listo"))
	   ) ; end progn
          )
	  (setf (aref *gama0* i) (nth 0 lista))
	  (setf (aref *gama1* i) (nth 1 lista))
	  (setf (aref *gama2* i) (nth 2 lista))
	  (setf (aref *gama3* i) (nth 3 lista))
          (setf (aref *sistema* i)
	        (make-individuo :modula listc :sfit f :beta *bmin*))
     ) ; end do
 ) ; end if

 (dotimes (n *Ntmp*)
   (unless (aref *mejores* 0 n) ; then
     (setf (aref *mejores* 0 n) (individuo-sfit (aref *sistema* n)))
     (setf (aref *mejores* 1 n) (individuo-modula (aref *sistema* n)))
   ) ; end unless
 ) ; end dotimes

) ; end defun

(defun lista0 (&optional (n *profn*) (ind t))
"Crea listas de profundidad n aleatoriamente a partir de terminales en
*listat* y funciones en *listaf*"
 (if (= 0 n) ; then
     (nth (aleat (length *listat*)) *listat*)
; else
     (let* ((na    (if ind (aleat 2) (aleat (length *listaf*)))) 
            (nda   (cdr (nth na *listaf*)))
            (lista (cons (car (nth na *listaf*)) nil)))
            (do ((i 1 (+ 1 i))
                 (nt (aleat (length *listat*)) (aleat (length *listat*))))
                ((> i nda) lista) 
                (if (= 0 (aleat n)) ; then 
                    (setf lista (append lista (cons (nth nt *listat*) nil)))
               ; else
                    (setf lista (append lista (cons (lista0 (- n 1) nil) nil)))
                ) ; end if
	    ) ; end do
     ) ; end let
 ) ; end if
) ; end defun
