;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                      ;;;
;;; flistas-mc.lisp    (15/03/2004)    JPN               ;;;
;;;                                                      ;;;
;;; Funciones de manipulacion de listas.                 ;;;
;;;                                                      ;;;
;;; EXTERNAS: var-param-mc.fasl                          ;;;
;;;                                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :user)

(eval-when (compile load)
           (load "var-param-mc")
) ; nehw-lave

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; El primer grupo de funciones son totalmente generales y no precisan ;;;
;;; variables externas                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun c-d-atm (lista)
"Contador de atomos de una lista cualquiera"
 (cond ((null lista)		 0					  )
       ((atom lista)		 1					  )
       ((atom (car lista)) (+	 1		    (c-d-atm (cdr lista))))
       (  t		   (+ (c-d-atm (car lista)) (c-d-atm (cdr lista))))
  ) ; end cond
) ; end defun

(defun elemento (n lista)
"Devuelve el n-esimo elemento de la lista"
 (if (atom lista) ; then
     lista
; else
     (let ((n1 (c-d-atm (car lista))))
     	  (if (>= n1 n) ; then
     	      (elemento n (car lista))
     	;  else
     	      (elemento (- n n1) (cdr lista))
     	  ) ; end if
     ) ; end let
 ) ; end if
) ; end defun

(defun prf-arbol (lista)
"Define la profundidad de lista vista como arbol"
 (if (atom lista) ; then
        0
; else
     (+ 1 (apply #'max (mapcar #' prf-arbol lista)))
 ) ; end if
) ; end defun

(defun jer (lista)
"Devuelve la lista con elementos ordenados jerarquicamente"
 (cond ((atom lista) lista)
       ((atom (car lista)) (cons (car lista) (jer (cdr lista))))
       (t (do* ((res (pop lista) (pop lista))
                (sal (cons (if (atom res) res (car res)) nil)
		     (cons (if (atom res) res (car res)) sal))
		(lis (if (atom res) nil (cdr res))
		     (if (atom res) lis (append lis (cdr res))))
	       )
	       ((null lista) (append (reverse sal) (jer lis)))
          ) ; od 
       )
 ) ; dnoc
) ; nufed

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Este grupo de funciones precisan de las variables externas *listov* ;;;
;;; *listaf*, *listac* y *listat*                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nth-s-arbol (n lista)
 "Devuelve el s-arbol encabezado por el n-esimo atomo de lista"
 (cond ((null lista) nil)
       ((atom lista) (cons lista nil))
       ((> n (c-d-atm lista)) nil)
       ((= 1 n) (if (and (atom (car lista)) (not (member (car lista) *listat*)))
               ; then
	            lista
               ; else
	            (car lista)
                ) ; end if
       ) ; end cond
       ( t      (let ((n1 (c-d-atm (car lista))))
	             (if (>= n1 n) ; then
	                 (nth-s-arbol n (car lista))
	           ;  else
	                 (nth-s-arbol (- n n1) (cdr lista))
	             ) ; end if
                ) ; end let
       ) ; end cond
 ) ; end cond
) ; end defun

(defun jerarquia (n lista)
"Calcula la jerarquia del elemento n en la lista"
 (cond ((or (<= n 1) (atom lista)) 0)
       ((= 1 (cdr (assoc (car lista) *listaf*)))
        (+ 1 (jerarquia (- n 1) (second lista))))
       (t (let ((n1 (c-d-atm (second lista))))
               (if (>= n1 (- n 1)) ; then
	           (+ 1 (jerarquia (- n 1) (second lista)))
	      ; else
	           (+ 1 (jerarquia (- n n1 1) (third lista)))
	       ) ; end if
          ) ; end let
       )
 ) ; end cond
) ; end defun

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Algunas para Monte Carlo                                            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-subst (lista)
"Substitucion especial"
 (if (atom lista) ; then
     lista
; else
     (if (equal 'f (car lista)) ; then
         (let* ((ll0 (my-subst (cdr lista)))
	        (ll1 (push '(symbol-function fx) ll0))
	        (ll2 (push 'funcall ll1)))
	       ll2
	 ) ; end let
    ; else
	 (cons (my-subst (car lista)) (my-subst (cdr lista)))
     ) ; end if
 ) ; end if
) ; end defun
