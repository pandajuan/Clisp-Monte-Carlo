;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                ;;;
;;; mc.lisp   (18/03/2004)  JPN    ;;;
;;;                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :user)

(proclaim '(optimize (speed 3) (safety 2) (debug 0) (space 0)
                     (ext:inhibit-warnings 3)))

(eval-when (compile load)
           (load "var-param-mc")
           (load "definiciones-mc")
           (load "conexion-mc")
           (load "io-mc")
           (load "sistema0-mc")
           (load "monte_carlo")
           (load "aleat")
) ; nehw-lave

(defun mc (input-file &optional (paralelo t))
"Parallel tempering MC general"
 (when (eql *PIN* :unbound) ; then
  (ambiente)                                    ; conexion-mc
  (entrada (eval input-file))                   ; io-mc
  (lectura-de-variables*)                       ; io-mc
  (setf *semilla* (+ *semilla* *prueba*))
  (siembra)                                     ; aleat
  (when *verbose* (format t "~&Siembra lista"))
  (when paralelo (inicio-siguiente))            ; conexion-mc
 ) ; nehw
 (set-tiempo)                                   ; definiciones-mc
 (definiciones)                                 ; definiciones-mc
 (informacion)                                  ; io-mc
 (mezcla)                                       ; io-mc
 (cambio)                                       ; io-mc
 (zero)                                         ; sistema0-mc
 (when *prec* ; then
  (pre-calentamiento)                           ; monte_carlo
  (setf *prec* nil)
 ) ; nehw
 (dotimes (ipp *PIN*) (monte-carlo))            ; monte_carlo
 (valores-medios)                               ; monte_carlo
 (set-tiempo)                                   ; definiciones-mc
 (impresion-final)                              ; io-mc
 (incf *version*)
 (when (> 20 *version*) ; then
  (unless (probe-file *abort*) ; then
   (mc (eval input-file))                       ; mc
  ) ; sselnu
 ) ; nehw
) ; end defun

(mc "mc.in")
(quit)
