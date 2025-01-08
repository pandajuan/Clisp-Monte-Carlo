;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                      ;;;
;;; fprotected-mc.lisp    (15/03/2004)    JPN            ;;;
;;;                                                      ;;;
;;; Funciones vectoriales y protegidas.                  ;;;
;;;                                                      ;;;
;;; EXTERNAS: var-param-mc.fasl  aleat.fasl              ;;;
;;;                                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :user)

(eval-when (compile load)
           (load "var-param-mc")
	   (load "aleat")
) ; nehw-lave

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                      ;
;;; Funciones protegidas ;
;;;                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun sgn (arg)
"Funcion signo no ambiguo (sgn 0) -> 1"
 (if (> 0.0 arg) -1.0 1.0)
) ; nufed

(defun rignum (arg)
"En caso de paridad de signo, dispara un numero aleatorio"
 (if (zerop arg) (rignum (- (aleat) 0.5)) (sgn arg))
) ; nufed

(defun % (num den)
"Division protegida. Numerador 0 => 0"
 (if (> *Chico* (abs den)) (* *Grande* (signum num)) (/ num den))
) ; nufed

(defun Plog (arg)
"Logaritmo protegido"
 (log (max 1.0e-45 arg))
) ; nufed

(defun Pexp (arg)
"Exponencial protegida"
 (exp (min 13.0 arg))
) ; nufed

(defun Psqr (arg)
"Raiz cuadrada protegida"
 (sqrt (abs arg))
) ; nufed

(defun dfloat (x)
"Devuelve x en precision doble"
 (float x 1.d0)
) ; end defun

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                   ;;;
;;; Macros de operaciones funcionales ;;;
;;;                                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro dv (f) (listp f) (list 'quote `,(der0 `,f))) 
(defmacro fl (f) (list 'kl (list 'quote `,f) ''xl))
(defmacro fr (f) (list 'kl (list 'quote `,f) ''xr))

(defun der0 (f)
"Con el macro dv expresa la derivada en x de la funcion f"
 (cond ((and (listp f) (or (equal (first f) 'fr) (equal (first f) 'fl))) 0)
       ((and (listp f) (or (equal (first f) '+) (equal (first f) '-)))
        (list (first f) (der0 (second f)) (der0 (third f))))
       ((and (listp f) (equal (first f) '*))
        (list '+ (list '* (der0 (second f)) (third f))
	         (list '* (second f) (der0 (third f)))))
       ((and (listp f) (equal (first f) '%))
        (list '% (list '- (list '* (der0 (second f)) (third f))
	                  (list '* (second f) (der0 (third f))))
		 (list '* (third f) (third f))))
       ((and (listp f) (equal (first f) 'Psqr))
        (list '% (der0 (second f)) (list '* 2 `,f)))
       ((and (listp f) (equal (first f) 'f))
        (list '* (list '% (list '- (subst '(+ x *eps*) 'x `,f) `,f)
	                  (list '- (subst '(+ x *eps*) 'x (second f)) (second f)))
		 (der0 (second f))))
       ((listp f) (list '/ (list '- (subst '(+ x *eps*) 'x `,f) `,f) '*eps*))
       ((equal f 'x) 1)
       (t 0)
 ) ; end cond
) ; end defun

(defun kl (f xx)
"Conjuntamente con el macro fx evalua el argumento en el limite correspondiente"
 (cond ((null f) nil)
       ((listp f)
        (cond ((null (cdr f)) (cons (kl (car f) xx) nil))
	      ((equal 'dv (car f))
	       (if (atom (eval f)) ; then
	           (kl (eval f) xx)
	      ; else
	           (cons (kl (car (eval f)) xx) (kl (cdr (eval f)) xx))
	       ) ; end if
	      )
	      ((equal 'fl (car f))
	       (if (atom (eval f)) ; then
	           (kl (eval f) 'xl)
	      ; else
	           (cons (kl (car (eval f)) 'xl) (kl (cdr (eval f)) 'xl))
	       ) ; end if
	      )
	      ((equal 'fr (car f))
	       (if (atom (eval f)) ; then
	           (kl (eval f) 'xr)
	      ; else
	           (cons (kl (car (eval f)) 'xr) (kl (cdr (eval f)) 'xr))
	       ) ; end if
	      )
	      ( t (cons (kl (car f) xx) (kl (cdr f) xx)))
	) ; end cond
       )
       ((equal 'x f) xx)
       ( t f)
 ) ; end cond
) ; end defun

(defun f0 (x) (- (expt (/ x pi 0.5d0) 4) 1))
(defun f1 (x) (- (tan (/ x 3.24d0)) (* x x) -1))
(defun f2 (x) (- (cos x) (Plog (/ x pi))))
(defun f3 (x) (- x (* 0.5 pi)))

