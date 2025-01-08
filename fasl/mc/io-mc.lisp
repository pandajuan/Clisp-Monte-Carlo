;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                   ;;;
;;; io-mc.lisp    (18/03/2004)    JPN                                 ;;;
;;;                                                                   ;;;
;;; Funciones de entrada y salida.                                    ;;;
;;;                                                                   ;;;
;;; EXTERNAS: var-param-mc.fasl definiciones-mc.fasl conexion-mc.fasl ;;; 
;;;                                                                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :user)

(eval-when (compile load)
           (load "var-param-mc")
           (load "conexion-mc")
           (load "definiciones-mc")
;           (load "flistas")
) ; nehw-lave

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;                                       ;;;
;;; Lectura de parametros                 ;;;
;;;                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(defun lectura-de-variables* ()
"Lectura de las variables naturales del problema a resolver con PG"
 (let ((file (concatenate 'string *path* "/stdin.in")))
 (with-open-file  (stream file)
  (setf *listat*  (read stream))
  (setf *listaf*  (read stream))
  (setf *prof0*   (read stream))
  (setf *profn*   (read stream))
  (setf *Chico*   (read stream))
  (setf *Grande*  (read stream))
  (setf *verbose* (read stream))
 ) ; close stream
 ) ; end let
 (when *verbose* (format t "~&Lectura-de-variables* lista"))
) ; end defun

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;                                       ;;;
;;; Lectura de datos de entrada           ;;;
;;;                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(defun entrada (name)
"Lee del file name los argumentos del Monte Carlo"
 (let ((file (string name)))
      (with-open-file (stream file)
       (setf *PIN*           (read stream))
       (setf *PMC*           (read stream))
       (setf *bmax*          (read stream))
       (setf *bmin*          (read stream))
       (setf *eps*           (read stream))
       (setf *Ntmp*          (read stream))
       (setf *version*       (read stream))
       (setf *prueba*        (read stream))
       (setf *fitness*       (read stream))
       (setf *lista-fitness* (read stream))
       (setf *xi*            (read stream))
       (setf *xf*            (read stream))
       (setf *prec*          (read stream))
      ) ; close stream
      (with-open-file (stream file :direction :output :if-exists :overwrite)
       (format stream "~& ~S" *PIN*)
       (format stream "~& ~S" *PMC*)
       (format stream "~& ~S" *bmax*)
       (format stream "~& ~S" *bmin*)
       (format stream "~& ~S" *eps*)
       (format stream "~& ~S" *Ntmp*)
       (format stream "~& ~S" *version*)
       (format stream "~& ~S" (+ 1 *prueba*))
       (format stream "~& ~S" *fitness*)
       (format stream "~& ~S" *lista-fitness*)
       (format stream "~& ~S" *xi*)
       (format stream "~& ~S" *xf*)
       (format stream "~& ~S" *prec*)
       (format stream "~&")
      ) ; close stream
 ) ; end let
 (when *verbose* (format t "~&Entrada lista"))
) ; end defun

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                   ;;;
;;; Impresion de datos de informacion inicial y final ;;;
;;;                                                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pr-info (file &rest body)
"Macro de impresion de acuerdo a la existencia del archivo de salida"
 (let ((lista (list 'stream `,file ':direction ':output))
       (listb (list ':if-exists ':append))
       (separador "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"))
      (if (probe-file file) ; then
          (append (list 'with-open-file (append lista listb) 
                        '(format stream "~%") '(format stream "~%")
	                `(format stream ,separador)
                        '(format stream "~%") '(format stream "~%"))
		   body)
     ; else
          (append (list 'with-open-file lista) body)
      ) ; end if
 ) ; end let
) ; end defun

(defun informacion ()
"Maneja el archivo de informacion"
 (unless (probe-file *inf*) ; then
   (let* ((time (multiple-value-list (get-decoded-time)))
          (mm (nth 1 time)) (hh (nth 2 time)) (d (nth 3 time))
          (m  (nth 4 time)) (a  (nth 5 time)))
         (eval (pr-info *inf*
           '(format stream "Version  ~S   Prueba numero ~S" *version* *prueba*)
           `(format stream "~%Hora y fecha de entrada: ~S:~S  ~S/~S/~S" ,hh ,mm ,d ,m ,a)
           '(format stream "~%~%")
           '(format stream "~&Terminales: ~S" *listat*)
           '(format stream "~&Funciones:  ~S" *listaf*)
           '(format stream "~&*prof0*:    ~S" *prof0*)
           '(format stream "~&*profn*:    ~S" *profn*)
           '(format stream "~&*PIN*:      ~S" *PIN*)
           '(format stream "~&*PMC*:      ~S" *PMC*)
           '(format stream "~&*bmax*:     ~S" *bmax*)
           '(format stream "~&*bmin*:     ~S" *bmin*)
           '(format stream "~&*Ntmp*:     ~S" *Ntmp*)
           '(format stream "~&*semilla*:  ~S" *semilla*)
           '(format stream "~&*nodos*:    ~S" *nodos*)
           '(format stream "~%~%")
         )) ; end pr-info
   ) ; end let
  (when *verbose* (format t "~&Informacion lista"))
 ) ; sselnu
) ; end defun

(defun impresion-final ()
"Impresiones finales de resultados e informaciones"
 (with-open-file (stream *out* :direction :output)
   (do* ((i 0 (incf i))
         (bt (individuo-beta (aref *sistema* i))
	     (individuo-beta (aref *sistema* i)))
	 (vv (aref *F11* i) (aref *F11* i))
	 (sv (aref *F12* i) (aref *F12* i))
	 (cv (aref *C11* i) (aref *C11* i))
	 (sc (aref *C12* i) (aref *C12* i))
	 (dd (aref *D11* i) (aref *D11* i))
	 (sd (aref *D12* i) (aref *D12* i))
	 (dn (aref *N11* i) (aref *N11* i))
	 (sn (aref *N12* i) (aref *N12* i))
	 (aa (aref *Acp* i) (aref *Acp* i))
	 (aj (aref *Jcp* i) (aref *Jcp* i))
	)
        ((= i (- *Ntmp* 1))
	 (format stream *string* bt vv sv cv sc dd sd dn sn aa aj)
	)
	(format stream *string* bt vv sv cv sc dd sd dn sn aa aj)
   ) ; end do
 ) ; close stream
 (with-open-file (stream *inf* :direction :output :if-exists :append)
   (format stream "~%")
   (let* ((time (multiple-value-list (get-decoded-time)))
 	  (mm (nth 1 time)) (hh (nth 2 time)) (d (nth 3 time))
          (m  (nth 4 time)) (a  (nth 5 time))
          (ch (floor (/ *cpu-tm*  3600))) (ca (mod *cpu-tm*  3600)) 
          (cm (floor (/  ca	  60  ))) (cs (mod  ca         60)) 
          (rh (floor (/ *real-tm* 3600))) (ra (mod *real-tm* 3600))
          (rm (floor (/  ra	  60  ))) (rs (mod  ra         60)))
 	 (format stream "~%Hora y fecha de salida: ~S:~S  ~S/~S/~S" hh mm d m a)
 	 (format stream "~%Tiempo de CPU	 : ~S:~S:~S" ch cm cs)
 	 (format stream "~%Tiempo Real  	 : ~S:~S:~S" rh rm rs)
   ) ; end let
 ) ; close stream
 (ext:run-program "/bin/tar" `("-czf" ,*tmp* ,*tmp0* ,*tmp1* ,*tmp2* ,*tmp3*))
 (delete-file *tmp0*)
 (delete-file *tmp1*)
 (delete-file *tmp2*)
 (delete-file *tmp3*)
 (when *verbose* (format t "~&Impresion Final lista"))
) ; end defun

(defun recover ()
"Genera el archivo recover en el cual se guarda la funcion modulacion de cada
individuo al final de cada rodada independiente"
 (on-swt)
   (when (probe-file *recover*) (delete-file *recover*))
   (with-open-file (stream *recover* :direction :output)
     (dotimes (i *Ntmp*)
       (format stream "~&~5D"    (aref *gama0* i))
       (format stream "~&~5D"    (aref *gama1* i))
       (format stream "~&~5D"    (aref *gama2* i))
       (format stream "~&~5D"    (aref *gama3* i))
       (format stream "~&~15,6F" (individuo-beta (aref *sistema* i)))
       (format stream "~&~5D"    (individuo-ncmb (aref *sistema* i)))
       (format stream "~&~5D"    (individuo-ncmv (aref *sistema* i)))
       (format stream "~&~S"     (individuo-modula (aref *sistema* i)))
     ) ; end dotimes
   ) ; close stream
 (off-swt)
 (when (probe-file *mejor*) (delete-file *mejor*))
 (with-open-file (stream *mejor* :direction :output)
   (dotimes (i *Ntmp*)
     (format stream "~&~S" (aref *mejores* 0 i))
     (format stream "~&~S" (aref *mejores* 1 i))
   ) ; end dotimes
 ) ; close stream
 (when *verbose* (format t "~&Recover listo"))
) ; end defun

(defun indices (n &optional (listb nil))
"Genera una lista de n entradas con numeros de 0 a n-1 aleatoriamente
distribuidos"
 (if (= n (length listb)) ; then
     listb
; else
     (let ((nn (aleat n)))
          (if (member nn listb) nil (push nn listb))
	  (indices n listb)
     ) ; end let
  ) ; end if
) ; end defun

(defun mezcla ()
"Mezcla las listas de rodadas independientes a misma temperatura"
 (unless (probe-file *mezcla*) ; then
  (with-open-file (st *mezcla* :direction :output))
  (when (probe-file *recover*) ; then 
   (unless (probe-file *recover0*) ; then
    (do* ((gama0-inf (make-array `(,*nodos* ,*Ntmp*) :initial-element nil))
 	  (gama1-inf (make-array `(,*nodos* ,*Ntmp*) :initial-element nil))
 	  (gama2-inf (make-array `(,*nodos* ,*Ntmp*) :initial-element nil))
 	  (gama3-inf (make-array `(,*nodos* ,*Ntmp*) :initial-element nil))
 	  (beta-inf (make-array `(,*nodos* ,*Ntmp*) :initial-element nil))
 	  (ncmb-inf (make-array `(,*nodos* ,*Ntmp*) :initial-element nil))
 	  (ncmv-inf (make-array `(,*nodos* ,*Ntmp*) :initial-element nil))
 	  (lista-inf (make-array `(,*nodos* ,*Ntmp*) :initial-element nil))
 	  (i 0 (incf i))
 	  (v1 (string (digit-char (floor (/ i 10))))
 	      (string (digit-char (floor (/ i 10)))))
 	  (v0 (string (digit-char (mod i 10))) (string (digit-char (mod i 10))))
 	  (rc (concatenate 'string *path* "/recover" v1 v0 ".rec")
 	      (concatenate 'string *path* "/recover" v1 v0 ".rec"))
 	  (sw (concatenate 'string *path* "/recover" v1 v0 ".swt")
 	      (concatenate 'string *path* "/recover" v1 v0 ".swt")))
 	 ((= i *nodos*)
 	  (do ((g0-fin (make-array `(,*nodos* ,*Ntmp*) :initial-element nil))
 	       (g1-fin (make-array `(,*nodos* ,*Ntmp*) :initial-element nil))
 	       (g2-fin (make-array `(,*nodos* ,*Ntmp*) :initial-element nil))
 	       (g3-fin (make-array `(,*nodos* ,*Ntmp*) :initial-element nil))
 	       (bt-fin (make-array `(,*nodos* ,*Ntmp*) :initial-element nil))
 	       (nb-fin (make-array `(,*nodos* ,*Ntmp*) :initial-element nil))
 	       (nv-fin (make-array `(,*nodos* ,*Ntmp*) :initial-element nil))
 	       (l-fin (make-array `(,*nodos* ,*Ntmp*) :initial-element nil))
 	       (i1 0 (incf i1))
 	       (ind (indices *nodos*) (indices *nodos*)))
 	      ((= i1 *Ntmp*)
               (do* ((i2 0 (incf i2)) (i3 (+ 1 i2) (+ 1 i2))
        	     (p1 (string (digit-char (floor (/ i3 10))))
 			 (string (digit-char (floor (/ i3 10)))))
 		     (p0 (string (digit-char (mod i3 10)))
 			 (string (digit-char (mod i3 10))))
 		     (rd (concatenate 'string *path* "/recover" p1 p0 ".rec0")
 			 (concatenate 'string *path* "/recover" p1 p0 ".rec0")))
        	    ((= i2 *nodos*) nil)
        	    (when (probe-file rd) (delete-file rd))
        	    (with-open-file (st rd :direction :output)
        	     (dotimes (kk *Ntmp*)
        	       (format st "~&~5D"    (aref g0-fin i2 kk))
        	       (format st "~&~5D"    (aref g1-fin i2 kk))
        	       (format st "~&~5D"    (aref g2-fin i2 kk))
        	       (format st "~&~5D"    (aref g3-fin i2 kk))
        	       (format st "~&~15,6F" (aref bt-fin i2 kk))
        	       (format st "~&~5D"    (aref nb-fin i2 kk))
        	       (format st "~&~5D"    (aref nv-fin i2 kk))
        	       (format st "~&~S"     (aref l-fin i2 kk))
        	     ) ; end dotimes
        	    ) ; close st
               ) ; end do
              )
              (dotimes (jj *nodos*)
               (setf (aref g0-fin jj i1) (aref gama0-inf (nth jj ind) i1))
               (setf (aref g1-fin jj i1) (aref gama1-inf (nth jj ind) i1))
               (setf (aref g2-fin jj i1) (aref gama2-inf (nth jj ind) i1))
               (setf (aref g3-fin jj i1) (aref gama3-inf (nth jj ind) i1))
               (setf (aref bt-fin jj i1) (aref beta-inf (nth jj ind) i1))
               (setf (aref nb-fin jj i1) (aref ncmb-inf (nth jj ind) i1))
               (setf (aref nv-fin jj i1) (aref ncmv-inf (nth jj ind) i1))
               (setf (aref l-fin jj i1)  (aref lista-inf (nth jj ind) i1))
              ) ; end dotimes
 	 ) ; end do
 	)
 	(on-swt sw)
 	(with-open-file (st rc)
 	   (dotimes (j *Ntmp*)
             (setf (aref gama0-inf i j) (read st))
 	     (setf (aref gama1-inf i j) (read st))
 	     (setf (aref gama2-inf i j) (read st))
 	     (setf (aref gama3-inf i j) (read st))
 	     (setf (aref beta-inf i j) (read st))
 	     (setf (aref ncmb-inf i j) (read st))
 	     (setf (aref ncmv-inf i j) (read st))
 	     (setf (aref lista-inf i j) (read st))
           ) ; end dotimes
 	) ; close st
 	(off-swt sw)
    ) ; end do
   ) ; sselnu
  ) ; nehw
  (delete-file *mezcla*)
 ) ; sselnu
 (when *verbose* (format t "~&Mezcla lista"))
) ; end defun

(defun cambio()
"Cambia el archivo recover por el nuevo si es que existe"
 (let* ((p1 (string (digit-char (floor (/ *prueba* 10))))) 
        (p0 (string (digit-char (mod *prueba* 10))))
	(nuevo (concatenate 'string *path* "/recover" p1 p0 ".rec0")))
       (when (probe-file nuevo) ; then
         (delete-file *recover*)
	 (rename-file nuevo *recover*)
       ) ; end when
 ) ; end let 
 (when *verbose* (format t "~&Cambio listo"))
) ; end defun

(defun datos-temporales ()
"Calculos de final de generacion"
 (with-open-file (stream *tmp0* :direction :output :if-exists :append)
   (dotimes (i (/ *Ntmp* 2)) (format stream "~10,6F " (aref *kapa0* (* 2 i))))
   (format stream "~%")
 ) ; close stream
 (with-open-file (stream *tmp1* :direction :output :if-exists :append)
   (dotimes (i (/ *Ntmp* 2)) (format stream "~10,6F " (aref *kapa1* (* 2 i))))
   (format stream "~%")
 ) ; close stream
 (with-open-file (stream *tmp2* :direction :output :if-exists :append)
   (dotimes (i (/ *Ntmp* 2)) (format stream "~10,6F " (aref *kapa2* (* 2 i))))
   (format stream "~%")
 ) ; close stream
 (with-open-file (stream *tmp3* :direction :output :if-exists :append)
   (dotimes (i (/ *Ntmp* 2)) (format stream "~10,6F " (aref *kapa3* (* 2 i))))
   (format stream "~%")
 ) ; close stream
) ; end defun

