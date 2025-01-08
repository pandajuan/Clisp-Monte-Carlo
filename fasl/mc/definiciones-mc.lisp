;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                      ;;;
;;; definiciones-mc.lisp    (18/03/2004)    JPN          ;;;
;;;                                                      ;;;
;;; Definiciones de variables.                           ;;;
;;;                                                      ;;;
;;; EXTERNAS: var-param-mc.fasl                          ;;;
;;;                                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :user)

(eval-when (compile eval)
           (load "var-param-mc")
) ; nehw-lave

(defun nbeta (bt)
"Da un valor apropiado para la jerarquia de cambio"
 (floor (* *profn* (- 1.0 (exp (* (/ 60.0 *profn*)
                                  (/ (- *bmin* bt) (- *bmax* *bmin*)))))))
) ; end defun

(defun set-tiempo ()
"Define las variables necesarias para la estadistica de rendimiento en tiempo"
 (if (eql *cpu-tm* :unbound) ; then
     (setf *cpu-tm* (get-internal-run-time))
; else
     (setf *cpu-tm* (round (/ (- (get-internal-run-time)  *cpu-tm* ) 100)))
 ) ; fi
 (if (eql *real-tm* :unbound) ; then
     (setf *real-tm* (get-internal-real-time))
; else
     (setf *real-tm* (round (/ (- (get-internal-real-time)  *real-tm* ) 100)))
 ) ; fi
 (when *verbose* (format t "~&Seteo del tiempo listo"))
) ; nufed

;;;;;;;;;;;;;;;;;;;;;;;;
;;;                  ;;;
;;; Switchs          ;;;
;;;                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defun on-swt (&optional (sw *recswitch*))
"Mata el switch para impedir que alguien mas manipule el archivo principal"
 (do ((sww (probe-file sw) (probe-file sw))) (sww (delete-file sw)))
) ; end defun

(defun off-swt (&optional (sw *recswitch*))
"Crea el switch para permitir la manipulacion del archivo principal"
 (with-open-file (st sw :direction :output))
) ; end defun

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;                                       ;;;
;;; Definiciones de variables             ;;;
;;;                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(defun definiciones ()
"Definiciones de variables y 'arrays'"
 (when (eql :unbound *mejores*) ; then
       (setf *mejores* (make-array `(2 ,*Ntmp*) :initial-element nil))
 ) ; end when
 (when (eql :unbound *contador*) (setf *contador* 0))
 (setf *sistema* (make-array *Ntmp*))
 (setf *gama0*   (make-array *Ntmp* :initial-element 0.d0))
 (setf *gama1*   (make-array *Ntmp* :initial-element 0.d0))
 (setf *gama2*   (make-array *Ntmp* :initial-element 0.d0))
 (setf *gama3*   (make-array *Ntmp* :initial-element 0.d0))
 (when (eql :unbound *kapa0*) ; then
       (setf *kapa0*   (make-array *Ntmp* :initial-element 0.d0))
       (setf *kapa1*   (make-array *Ntmp* :initial-element 0.d0))
       (setf *kapa2*   (make-array *Ntmp* :initial-element 0.d0))
       (setf *kapa3*   (make-array *Ntmp* :initial-element 0.d0))
 ) ; end when
 (setf *Ac*      (make-array *Ntmp* :initial-element 0))
 (setf *Acp*     (make-array *Ntmp* :initial-element 0.d0))
 (setf *Act*     (make-array *Ntmp* :initial-element 0.d0))
 (setf *Jcp*     (make-array *Ntmp* :initial-element 0.d0))
 (setf *Jtt*     (make-array *Ntmp* :initial-element 0.d0))
 (setf *F11*     (make-array *Ntmp* :initial-element 0.d0))
 (setf *F12*     (make-array *Ntmp* :initial-element 0.d0))
 (setf *C11*     (make-array *Ntmp* :initial-element 0.d0))
 (setf *C12*     (make-array *Ntmp* :initial-element 0.d0))
 (setf *D11*     (make-array *Ntmp* :initial-element 0.d0))
 (setf *D12*     (make-array *Ntmp* :initial-element 0.d0))
 (setf *N11*     (make-array *Ntmp* :initial-element 0.d0))
 (setf *N12*     (make-array *Ntmp* :initial-element 0.d0))

 (unless (typep *fitness* 'compiled-function) ; then
         (setf *fitness* (compile nil (list 'lambda '(lista) *fitness*)))
 ) ; end unless

 (do ((i 0 (incf i)) (st "~12,6,2E " (concatenate 'string st "~17,9,2E ")))
     ((= i 8) (setf *string* (concatenate 'string st "~12,6,2E ~12,6,2E~%")))
 ) ; end do
 (let ((v1 (string (digit-char (floor (/ *version* 10)))))
       (v0 (string (digit-char (mod *version* 10))))
       (p1 (string (digit-char (floor (/ (- *prueba* 1) 10))))) 
       (p0 (string (digit-char (mod (- *prueba* 1) 10)))))
     (setf *inf* (concatenate 'string *dir* "/inf/info" v1 v0 "-" p1 p0 ".inf"))
     (setf *out* (concatenate 'string *dir* "/out/data" v1 v0 "-" p1 p0 ".out"))
     (setf *tmp* (concatenate 'string *dir* "/tmp/tp" v1 v0 "-" p1 p0 ".tgz"))
     (setf *tmp0* (concatenate 'string *dir* "/tmp/tp0" v1 v0 "-" p1 p0 ".tmp"))
     (setf *tmp1* (concatenate 'string *dir* "/tmp/tp1" v1 v0 "-" p1 p0 ".tmp"))
     (setf *tmp2* (concatenate 'string *dir* "/tmp/tp2" v1 v0 "-" p1 p0 ".tmp"))
     (setf *tmp3* (concatenate 'string *dir* "/tmp/tp3" v1 v0 "-" p1 p0 ".tmp"))
     (setf *abort* (concatenate 'string *path* "/abort" p1 p0))
     (setf *mezcla* (concatenate 'string *path* "/mezcla"))
     (setf *recover* (concatenate 'string *path* "/recover" p1 p0 ".rec"))
     (setf *recswitch* (concatenate 'string *path* "/recover" p1 p0 ".swt"))
     (setf *recover0* (concatenate 'string *path* "/recover" p1 p0 ".rec0"))
     (setf *mejor* (concatenate 'string *path* "/mejor" p1 p0 ".mej"))
  ) ; end let
 (when (probe-file *abort*) (delete-file *abort*))
 (when (probe-file *tmp0*) (delete-file *tmp0*))
 (with-open-file (st *tmp0* :direction :output))
 (when (probe-file *tmp1*) (delete-file *tmp1*))
 (with-open-file (st *tmp1* :direction :output))
 (when (probe-file *tmp2*) (delete-file *tmp2*))
 (with-open-file (st *tmp2* :direction :output))
 (when (probe-file *tmp3*) (delete-file *tmp3*))
 (with-open-file (st *tmp3* :direction :output))
 (when (probe-file *recswitch*) (delete-file *recswitch*))
 (with-open-file (st *recswitch* :direction :output))
 (when *verbose* (format t "~&Definiciones listas"))
) ; end defun
