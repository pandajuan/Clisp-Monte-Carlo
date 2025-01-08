;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                      ;;;
;;; redux-mc.lisp    (16/03/2004)    JPN                 ;;;
;;;                                                      ;;;
;;; Simplificaciones y reducciones.                      ;;;
;;;                                                      ;;;
;;; EXTERNAS: var-param-mc.fasl, aleat.fasl,             ;;;
;;;           fprotected-mc.fasl                         ;;;
;;;                                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :user)

(eval-when (compile load)
           (load "var-param-mc")
           (load "aleat")
           (load "fprotected-mc")
) ; nehw-lave

(defvar xl :unbound)
(defvar xr :unbound)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                         ;;;
;;; Ediciones y reducciones ;;;
;;;                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun reduccion (lista)
"Edicion completa de lista"
 (if (listp lista) ; then
     (progn
       (setf xl *xi*)
       (setf xr *xf*)
       (do* ((fa (nth (aleat (length *lista-fitness*)) *lista-fitness*))
             (listb (subst fa 'f (copy-seq lista)) listc)
             (listc (redux listb) (redux listc))
             (vb (eval listb)) (vc (eval listc) (eval listc))
             (tt (< 1.0e-2 (abs (% (- vb vc) vb)))
                 (< 1.0e-2 (abs (% (- vb vc) vb)))))
            ((or (equal listb listc) tt)
             (progn
               (makunbound 'xl)
               (makunbound 'xr)
               (if tt (subst 'f fa listb) (subst 'f fa listc))
             ) ; end progn
            )
       ) ; end do
     ) ; end progn
; else
     lista
 ) ; end if
) ; end defun

(defun red (lista)
"Edicion completa de lista"
 (if (listp lista) ; then
     (do* ((listb lista listc) (listc (redux listb) (redux listc)))
     	  ((equal listb listc) listb)
     ) ; end do
; else
     lista
 ) ; end if
) ; end defun

(defun redux (lista)
"Edicion general de listas sin considerar operaciones vectoriales"
 (cond ((atom lista) lista)
       ((and (eql 'DV (first lista)) (eql 'X (second lista))) 1)
       ((and (eql 'DV (first lista))
             (or (eql 0 (second lista)) (eql 1 (second lista))
	         (and (listp (second lista))
		      (or (eql 'FL (first (second lista)))
		          (eql 'FR (first (second lista))))))) 0)
       ((and (or (eql 'PSQR (first lista)) (eql 'FL (first lista))
                 (eql 'FR (first lista)))
             (eql 0 (second lista))) 0)
       ((and (or (eql 'PSQR (first lista)) (eql 'FL (first lista))
                 (eql 'FR (first lista)))
	     (eql 1 (second lista))) 1)     
       ((and (eql '- (first lista)) (eql (second lista) (third lista))) 0)
       ((and (eql '* (first lista)) (member 0 lista)) 0)
       ((and (eql '% (first lista)) (eql (second lista) (third lista))) 1)
       ((and (eql '% (first lista)) (eql 0 (second lista))) 0)
       ((and (eql '% (first lista)) (eql 0 (second lista))
             (eql 0 (third lista))) 1)
       ((and (eql '% (first lista)) (eql 0 (third lista)))
        (list '* '*Grande* (list 'SIGNUM (redux (second lista)))))
       ((and (eql '* (first lista)) (member 1 lista))
	(if (eql 1 (third lista)) (redux (second lista)) (redux (third lista))))
       ((and (eql '+ (first lista)) (member 0 lista))
	(if (eql 0 (third lista)) (redux (second lista)) (redux (third lista))))
       ((or (and (eql '- (first lista)) (eql 0 (third lista)))
            (and (eql '% (first lista)) (eql 1 (third lista))))
        (redux (second lista)))
       ((and (or (eql 'FR (first lista)) (eql 'FL (first lista)))
             (listp (second lista))
             (or (eql 'FR (first (second lista)))
	         (eql 'FL (first (second lista)))))
        (redux (second lista)))
       ((and (eql '% (first lista))
             (listp (second lista)) (eql '% (first (second lista)))
	     (listp (third  lista)) (eql '% (first (third  lista))))
	(list '% (list '* (redux (second (second lista)))
	                  (redux (third  (third  lista))))
	         (list '* (redux (third  (second lista)))
		          (redux (second (third lista))))))
       ((and (eql '% (first lista))
             (listp (second lista)) (eql '% (first (second lista))))
	(list '% (redux (second (second lista)))
	         (list '* (redux (third (second lista)))
		          (redux (third lista)))))
       ((and (eql '% (first lista))
	     (listp (third  lista)) (eql '% (first (third  lista))))
	(list '% (list '* (redux (second lista)) 
	                  (redux (third (third lista))))
	         (redux (second (third lista)))))
       ((and (eql '* (first lista))
             (listp (second lista)) (eql '% (first (second lista)))
	     (listp (third  lista)) (eql '% (first (third  lista))))
	(list '% (list '* (redux (second (second lista)))
	                  (redux (second (third  lista))))
	         (list '* (redux (third  (second lista))) 
		          (redux (third  (third lista))))))
       ((and (eql '* (first lista))
             (listp (second lista)) (eql '% (first (second lista))))
	(list '% (list '* (redux (second (second lista)))
	                  (redux (third lista)))
	         (redux (third (second lista)))))
       ((and (eql '* (first lista))
	     (listp (third  lista)) (eql '% (first (third  lista))))
	(list '% (list '* (redux (second lista))
	                  (redux (second (third lista))))
	         (redux (third (third lista)))))
       ((and (eql 'PSQR (first lista))
             (listp (second lista)) (eql '% (first (second lista))))
	(list '% (list (first lista) (redux (second (second lista))))
	         (list (first lista) (redux (third  (second lista))))))
       ((null (third lista)) (list (first lista) (redux (second lista))))
       ( t  (list (first lista) (redux (second lista)) (redux (third lista))))
 ) ; end cond
) ; end defun

