;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; monte_carlo.lisp   (18/03/2004)  JPN                             ;;;
;;;                                                                  ;;;
;;; Tareas propias de Monte Carlo.                                   ;;;
;;;                                                                  ;;;
;;; EXTERNAS: var-param-mc.fasl definiciones-mc.fasl flistas-mc.fasl ;;;
;;;           fprotected-mc.fasl io-mc.fasl sistema0-mc.fasl         ;;;
;;;           redux-mc.fasl                                          ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :user)

(eval-when (compile load)
           (load "var-param-mc")
           (load "definiciones-mc")
           (load "flistas-mc")
           (load "fprotected-mc")
           (load "io-mc")
           (load "sistema0-mc")
           (load "redux-mc")
) ; nehw-lave

(defun n0 (jj lista)
"Encuentra el nodo apropiado para implante"
 (do* ((i jj (incf i)) (lng (c-d-atm lista))
       (listp (if (= jj (jerarquia i lista)) (list i) nil)
 	      (if (= jj (jerarquia i lista)) (push i listp) listp)))
      ((= i lng) (nth (aleat (length listp)) listp))
 ) ; end do
) ; end defun

(defun estado-nuevo (lista hefc &optional (h< *profn*))
"Genera un nuevo estado a partir de lista"
 (let* ((jl (prf-arbol lista))
	(jj (min *profn* (+ hefc (aleat h<))))
        (k0 (if (>= jl jj) (- *profn* jj) (- jj jl)))
        (k1 (if (>= jl jj) jj jl))
        (rama (lista0 k0 (= k0 *profn*)))
        (n0 (n0 k1 lista)))
       (if (= k1 0) (lista0 *prof0*) (reemplazo n0 n0 lista rama))
 ) ; end let
) ; end defun

(defun reemplazo (n m lista s-arbol)
"Extrae el subarbol de lista en el nodo n y lo reemplaza por s-arbol"
 (cond ((or (= 1 m) (not (listp lista))) s-arbol)
       ((= 1 n) (if (or (member (car lista) *listat*)
                        (listp  (car lista))) ; then
                    (cons s-arbol (cdr lista))
               ; else
                    (list s-arbol)
                ) ; end if
       )
       ( t  (let ((n1 (c-d-atm (car lista))))
                 (if (>= n1 n) ; then
                     (cons (reemplazo n m (car lista) s-arbol) (cdr lista))
                ; else
                     (cons (car lista)
		           (reemplazo (- n n1) m (cdr lista) s-arbol))
                 ) ; end if
	   ) ; end let
       )
 ) ; end cond 
) ; end defun

;;;;;;;;;;;;;;;;;;;
;;;             ;;;
;;; Monte Carlo ;;;
;;;             ;;;
;;;;;;;;;;;;;;;;;;;

(defun pre-calentamiento (&optional (M *PMC*))
"Preparacion del sistema para la simulacion Monte Carlo"
 (do* ((kM 1 (incf kM))
       (op (or (> kM M) (probe-file *abort*))
           (or (> kM M) (probe-file *abort*))))
     (op (dotimes (ktmp *Ntmp*)
      	   (let ((bt (individuo-beta (aref *sistema* ktmp))))
      	 	(setf (individuo-nbeta (aref *sistema* ktmp)) (nbeta bt))
	   ) ; end let
	 ) ; end dotimes 
     )
     (format t "~&Inicio ~s" kM);##
     (if (= 0 (mod kM 10)) ; then
         (do* ((ind (mod (/ kM 10) 2) (incf ind 2))
	       (jnd (+ ind 1) (+ ind 1))
	       (bt> (individuo-beta (aref *sistema* (mod ind *Ntmp*)))
	     	    (individuo-beta (aref *sistema* (mod ind *Ntmp*))))
	       (bt< (individuo-beta (aref *sistema* (mod jnd *Ntmp*)))
	     	    (individuo-beta (aref *sistema* (mod jnd *Ntmp*))))
               (en> (individuo-sfit (aref *sistema* (mod ind *Ntmp*)))
	     	    (individuo-sfit (aref *sistema* (mod ind *Ntmp*))))
	       (en< (individuo-sfit (aref *sistema* (mod jnd *Ntmp*)))
	     	    (individuo-sfit (aref *sistema* (mod jnd *Ntmp*))))
	       (nb> (individuo-ncmb (aref *sistema* (mod ind *Ntmp*)))
	     	    (individuo-ncmb (aref *sistema* (mod ind *Ntmp*))))
	       (nv> (individuo-ncmv (aref *sistema* (mod ind *Ntmp*)))
	     	    (individuo-ncmv (aref *sistema* (mod ind *Ntmp*))))
	       (g0> (aref *gama0* (mod ind *Ntmp*))
	     	    (aref *gama0* (mod ind *Ntmp*)))
	       (g1> (aref *gama1* (mod ind *Ntmp*))
	     	    (aref *gama1* (mod ind *Ntmp*)))
	       (g2> (aref *gama2* (mod ind *Ntmp*))
	     	    (aref *gama2* (mod ind *Ntmp*)))
	       (g3> (aref *gama3* (mod ind *Ntmp*))
	     	    (aref *gama3* (mod ind *Ntmp*)))
	       (ll> (individuo-modula (aref *sistema* (mod ind *Ntmp*)))
	     	    (individuo-modula (aref *sistema* (mod ind *Ntmp*))))
	       (del (* (- bt> bt<) (- en> en<)) (* (- bt> bt<) (- en> en<))))
              ((>= jnd *Ntmp*) nil)
	      (when (or (<= 0 del) (< (aleat) (exp del)))
	 	    (setf (individuo-sfit (aref *sistema* ind)) en<)
	 	    (setf (individuo-sfit (aref *sistema* jnd)) en>)
	 	    (setf (individuo-modula (aref *sistema* ind))
	 		  (individuo-modula (aref *sistema* jnd)))
	 	    (setf (individuo-modula (aref *sistema* jnd)) ll>)
	 	    (setf (individuo-ncmb (aref *sistema* ind))
	 		  (individuo-ncmb (aref *sistema* jnd)))
	 	    (setf (individuo-ncmb (aref *sistema* jnd)) nb>)
	 	    (setf (individuo-ncmv (aref *sistema* ind))
	 		  (individuo-ncmv (aref *sistema* jnd)))
	 	    (setf (individuo-ncmv (aref *sistema* jnd)) nv>)
	     	    (setf (aref *gama0* (mod ind *Ntmp*))
	     		  (aref *gama0* (mod jnd *Ntmp*)))
	     	    (setf (aref *gama0* (mod jnd *Ntmp*)) g0>)
	     	    (setf (aref *gama1* (mod ind *Ntmp*))
	     		  (aref *gama1* (mod jnd *Ntmp*)))
	     	    (setf (aref *gama1* (mod jnd *Ntmp*)) g1>)
	     	    (setf (aref *gama2* (mod ind *Ntmp*))
	     		  (aref *gama2* (mod jnd *Ntmp*)))
	     	    (setf (aref *gama2* (mod jnd *Ntmp*)) g2>)
	     	    (setf (aref *gama3* (mod ind *Ntmp*))
	     		  (aref *gama3* (mod jnd *Ntmp*)))
	     	    (setf (aref *gama3* (mod jnd *Ntmp*)) g3>)
	     	    (when (> (aref *mejores* 0 ind) en<)
         		  (setf (aref *mejores* 0 ind) en<)
         		  (setf (aref *mejores* 1 ind)
	     			(individuo-modula (aref *sistema* ind)))
	     	    ) ; end when
	     	    (when (> (aref *mejores* 0 jnd) en>)
         		  (setf (aref *mejores* 0 jnd) en>)
         		  (setf (aref *mejores* 1 jnd) ll>)
	     	    ) ; end when
	      ) ; end when
	 ) ; end do temp
    ; else
	 (do* ((kNtmp 0 (incf kNtmp))
	       (id (mod kNtmp *Ntmp*) (mod kNtmp *Ntmp*))
	       (bt (individuo-beta (aref *sistema* id))
	 	   (individuo-beta (aref *sistema* id)))
	       (e0 (individuo-sfit (aref *sistema* id))
	           (individuo-sfit (aref *sistema* id)))
	       (nn (individuo-ncmb (aref *sistema* id))
	           (individuo-ncmb (aref *sistema* id)))
	       (ll (individuo-modula (aref *sistema* id))
	           (individuo-modula (aref *sistema* id)))
	       (ln (estado-nuevo ll nn) (estado-nuevo ll nn))
	       (lr (red ln) (red ln))
	       (f0 (funcall *fitness* lr) (funcall *fitness* lr))
	       (ef (apply #'+ f0) (apply #'+ f0))
	       (dd (* bt (- e0 ef)) (* bt (- e0 ef)))
	       (op (or (<= 0 dd) (< (aleat) (Pexp dd)))
	           (or (<= 0 dd) (< (aleat) (Pexp dd)))))
	      ((= kNtmp *Ntmp*) nil)
	      (when op ; then
	 	    (setf (individuo-sfit (aref *sistema* id)) ef)
	 	    (setf (individuo-modula (aref *sistema* id)) ln)
	     	    (setf (aref *gama0* id) (nth 0 f0))
	     	    (setf (aref *gama1* id) (nth 1 f0))
	     	    (setf (aref *gama2* id) (nth 2 f0))
	     	    (setf (aref *gama3* id) (nth 3 f0))
	     	    (when (> (aref *mejores* 0 id) ef)
         		  (setf (aref *mejores* 0 id) ef)
         		  (setf (aref *mejores* 1 id) lr)
	     	    ) ; end when
	      ) ; end when
	 ) ; end do temp
     ) ; end if
     (format t "~&Fin ~s" kM);##
     (format t "~&gamma3 ~s" (individuo-modula (aref *sistema* 0)));##
     (when (= 0 (mod kM 10)) (datos-temporales))
     (when (= 0 (mod kM 50)) (recover))
     (do* ((kNtmp 1 (incf kNtmp))
           (delb (* (/ kNtmp (* M (- *Ntmp* 1))) (- *bmax* *bmin*))
	         (* (/ kNtmp (* M (- *Ntmp* 1))) (- *bmax* *bmin*))))
	  ((= kNtmp *Ntmp*) nil)
	  (incf (individuo-beta (aref *sistema* kNtmp)) delb)
     ) ; end do temp
 ) ; end do kM
 (when *verbose* (format t "~&Precalentamiento listo"))
) ; end defun

(defun monte-carlo ()
"Loop Monte Carlo"
 (do* ((kPMC 0 (incf kPMC))
       (op (or (= kPMC *PMC*) (probe-file *abort*))
           (or (= kPMC *PMC*) (probe-file *abort*)))
       (*F01* (make-array *Ntmp* :initial-element 0.d0))
       (*F02* (make-array *Ntmp* :initial-element 0.d0))
       (*F03* (make-array *Ntmp* :initial-element 0.d0))
       (*F04* (make-array *Ntmp* :initial-element 0.d0)))
      (op (do* ((k 0 (incf k)) (i (mod k *Ntmp*) (mod k *Ntmp*))
          	(v1 (/ (aref *F01* i) *PMC*) (/ (aref *F01* i) *PMC*))
          	(v2 (/ (aref *F02* i) *PMC*) (/ (aref *F02* i) *PMC*))
          	(v3 (/ (aref *F03* i) *PMC*) (/ (aref *F03* i) *PMC*))
          	(v4 (/ (aref *F04* i) *PMC*) (/ (aref *F04* i) *PMC*))
          	(bt (individuo-beta (aref *sistema* i))
          	    (individuo-beta (aref *sistema* i)))
      	  	(a1 (* 2.d0 (+ 1.d0 (* bt v1)))
          	    (* 2.d0 (+ 1.d0 (* bt v1))))
      	  	(a2 (* -1.d0 (+ 2.d0 (* 3.d0 bt v1)))
          	    (* -1.d0 (+ 2.d0 (* 3.d0 bt v1))))
      	  	(cv (* (- v2 (* v1 v1)) bt bt) (* (- v2 (* v1 v1)) bt bt))
          	(dd (* (+ (* bt v3) (* a2 v2) (* a1 v1 v1)) bt bt bt)
          	    (* (+ (* bt v3) (* a2 v2) (* a1 v1 v1)) bt bt bt)))
      	  	((= k *Ntmp*) nil)
          	(incf (aref *F11* i) v1)
          	(incf (aref *F12* i) (* v1 v1))
          	(incf (aref *C11* i) cv)
          	(incf (aref *C12* i) (* cv cv))
          	(incf (aref *D11* i) dd)
          	(incf (aref *D12* i) (* dd dd))
          	(incf (aref *N11* i) v4)
          	(incf (aref *N12* i) (* v4 v4))
          ) ; end do update
      )
     (if (= 0 (mod kPMC 10)) ; then
         (progn
	 (do* ((ii 0 (incf ii)) (ij (mod ii *Ntmp*) (mod ii *Ntmp*))
	       (nb (individuo-nbeta (aref *sistema* ij))
	  	   (individuo-nbeta (aref *sistema* ij)))
	       (nn (individuo-ncmb (aref *sistema* ij))
	  	   (individuo-ncmb (aref *sistema* ij)))
	       (nv (individuo-ncmv (aref *sistema* ij))
	  	   (individuo-ncmv (aref *sistema* ij)))
	       (rac (- (aref *Ac* ij) 5) (- (aref *Ac* ij) 5))
	       (cnn (if (< 1 (abs rac)) (floor (signum rac)) 0)
	  	    (if (< 1 (abs rac)) (floor (signum rac)) 0))
;	       (cnf (max 0 (min (- *profn* 1) (- nn cnn) nb))
;	  	    (max 0 (min (- *profn* 1) (- nn cnn) nb)))
	       (cnf (max 0 (min (- *profn* 1) (- nn cnn)))
	  	    (max 0 (min (- *profn* 1) (- nn cnn))))
	       (cnv (max 0 (min (- *profn* cnf) (- nv cnn)))
	  	    (max 0 (min (- *profn* cnf) (- nv cnn)))))
	      ((or (= 0 kPMC) (= ii *Ntmp*)) nil)
	      (setf (individuo-ncmb (aref *sistema* ij)) cnf)
	      (setf (individuo-ncmv (aref *sistema* ij)) cnv)
	      (setf (aref *Ac* ij) 0)
	 ) ; end do
         (do* ((ind (mod (/ kPMC 10) 2) (incf ind 2))
	       (jnd (+ ind 1) (+ ind 1))
	       (bt> (individuo-beta (aref *sistema* (mod ind *Ntmp*)))
	     	    (individuo-beta (aref *sistema* (mod ind *Ntmp*))))
	       (bt< (individuo-beta (aref *sistema* (mod jnd *Ntmp*)))
	     	    (individuo-beta (aref *sistema* (mod jnd *Ntmp*))))
               (en> (individuo-sfit (aref *sistema* (mod ind *Ntmp*)))
	     	    (individuo-sfit (aref *sistema* (mod ind *Ntmp*))))
	       (en< (individuo-sfit (aref *sistema* (mod jnd *Ntmp*)))
	     	    (individuo-sfit (aref *sistema* (mod jnd *Ntmp*))))
	       (nb> (individuo-ncmb (aref *sistema* (mod ind *Ntmp*)))
	     	    (individuo-ncmb (aref *sistema* (mod ind *Ntmp*))))
	       (nv> (individuo-ncmv (aref *sistema* (mod ind *Ntmp*)))
	     	    (individuo-ncmv (aref *sistema* (mod ind *Ntmp*))))
	       (g0> (aref *gama0* (mod ind *Ntmp*))
	     	    (aref *gama0* (mod ind *Ntmp*)))
	       (g1> (aref *gama1* (mod ind *Ntmp*))
	     	    (aref *gama1* (mod ind *Ntmp*)))
	       (g2> (aref *gama2* (mod ind *Ntmp*))
	     	    (aref *gama2* (mod ind *Ntmp*)))
	       (g3> (aref *gama3* (mod ind *Ntmp*))
	     	    (aref *gama3* (mod ind *Ntmp*)))
	       (ll> (individuo-modula (aref *sistema* (mod ind *Ntmp*)))
	     	    (individuo-modula (aref *sistema* (mod ind *Ntmp*))))
	       (del (* (- bt> bt<) (- en> en<)) (* (- bt> bt<) (- en> en<))))
              ((>= jnd *Ntmp*) nil)
	      (incf (aref *Jtt* ind))
	      (when (or (<= 0 del) (< (aleat) (exp del)))
		    (incf (aref *Jcp* ind))
	 	    (setf (individuo-sfit (aref *sistema* ind)) en<)
	 	    (setf (individuo-sfit (aref *sistema* jnd)) en>)
	 	    (setf (individuo-modula (aref *sistema* ind))
	 		  (individuo-modula (aref *sistema* jnd)))
	 	    (setf (individuo-modula (aref *sistema* jnd)) ll>)
	 	    (setf (individuo-ncmb (aref *sistema* ind))
	 		  (individuo-ncmb (aref *sistema* jnd)))
	 	    (setf (individuo-ncmb (aref *sistema* jnd)) nb>)
	 	    (setf (individuo-ncmv (aref *sistema* ind))
	 		  (individuo-ncmv (aref *sistema* jnd)))
	 	    (setf (individuo-ncmv (aref *sistema* jnd)) nv>)
	     	    (setf (aref *gama0* (mod ind *Ntmp*))
	     		  (aref *gama0* (mod jnd *Ntmp*)))
	     	    (setf (aref *gama0* (mod jnd *Ntmp*)) g0>)
	     	    (setf (aref *gama1* (mod ind *Ntmp*))
	     		  (aref *gama1* (mod jnd *Ntmp*)))
	     	    (setf (aref *gama1* (mod jnd *Ntmp*)) g1>)
	     	    (setf (aref *gama2* (mod ind *Ntmp*))
	     		  (aref *gama2* (mod jnd *Ntmp*)))
	     	    (setf (aref *gama2* (mod jnd *Ntmp*)) g2>)
	     	    (setf (aref *gama3* (mod ind *Ntmp*))
	     		  (aref *gama3* (mod jnd *Ntmp*)))
	     	    (setf (aref *gama3* (mod jnd *Ntmp*)) g3>)
	     	    (when (> (aref *mejores* 0 ind) en<)
         		  (setf (aref *mejores* 0 ind) en<)
         		  (setf (aref *mejores* 1 ind)
	     			(individuo-modula (aref *sistema* ind)))
	     	    ) ; end when
	     	    (when (> (aref *mejores* 0 jnd) en>)
         		  (setf (aref *mejores* 0 jnd) en>)
         		  (setf (aref *mejores* 1 jnd) ll>)
	     	    ) ; end when
	      ) ; end when
	 ) ; end do temp
	 ) ; progn
    ; else
	 (do* ((kNtmp 0 (incf kNtmp))
	       (id (mod kNtmp *Ntmp*) (mod kNtmp *Ntmp*))
	       (bt (individuo-beta (aref *sistema* id))
	 	   (individuo-beta (aref *sistema* id)))
	       (e0 (individuo-sfit (aref *sistema* id))
	           (individuo-sfit (aref *sistema* id)))
	       (nn (individuo-ncmb (aref *sistema* id))
	           (individuo-ncmb (aref *sistema* id)))
	       (nv (individuo-ncmv (aref *sistema* id))
	           (individuo-ncmv (aref *sistema* id)))
	       (ll (individuo-modula (aref *sistema* id))
	           (individuo-modula (aref *sistema* id)))
	       (ln (estado-nuevo ll nn nv) (estado-nuevo ll nn nv))
	       (lr (red ln) (red ln))
	       (f0 (funcall *fitness* lr) (funcall *fitness* lr))
	       (ef (apply #'+ f0) (apply #'+ f0))
	       (dd (* bt (- e0 ef)) (* bt (- e0 ef)))
	       (op (or (<= 0 dd) (< (aleat) (Pexp dd)))
	           (or (<= 0 dd) (< (aleat) (Pexp dd)))))
	      ((= kNtmp *Ntmp*) nil)
	      (incf (aref *Act* id))
	      (when op ; then
		    (incf (aref *Ac* id))
		    (incf (aref *Acp* id))
	 	    (setf (individuo-sfit (aref *sistema* id)) ef)
	 	    (setf (individuo-modula (aref *sistema* id)) ln)
	     	    (setf (aref *gama0* id) (nth 0 f0))
	     	    (setf (aref *gama1* id) (nth 1 f0))
	     	    (setf (aref *gama2* id) (nth 2 f0))
	     	    (setf (aref *gama3* id) (nth 3 f0))
	     	    (when (> (aref *mejores* 0 id) ef)
         		  (setf (aref *mejores* 0 id) ef)
         		  (setf (aref *mejores* 1 id) lr)
	     	    ) ; end when
	      ) ; end when
	 ) ; end do temp
     ) ; end if
     (incf *contador*)
     (do* ((k 0 (incf k)) (id (mod k *Ntmp*) (mod k *Ntmp*))
           (new (dfloat *contador*))
	   (h0 (/ (- (aref *gama0* id) (aref *kapa0* id)) new)
	       (/ (- (aref *gama0* id) (aref *kapa0* id)) new))
	   (h1 (/ (- (aref *gama1* id) (aref *kapa1* id)) new)
	       (/ (- (aref *gama1* id) (aref *kapa1* id)) new))
	   (h2 (/ (- (aref *gama2* id) (aref *kapa2* id)) new)
	       (/ (- (aref *gama2* id) (aref *kapa2* id)) new))
	   (h3 (/ (- (aref *gama3* id) (aref *kapa3* id)) new)
	       (/ (- (aref *gama3* id) (aref *kapa3* id)) new))
           (e (/ (individuo-sfit (aref *sistema* id)) 400.0)
	      (/ (individuo-sfit (aref *sistema* id)) 400.0))
	   (m (* 0.5 (+ (individuo-ncmv (aref *sistema* id))
	                (* 2 (individuo-ncmb (aref *sistema* id)))))
	      (* 0.5 (+ (individuo-ncmv (aref *sistema* id))
	                (* 2 (individuo-ncmb (aref *sistema* id)))))))
          ((= k *Ntmp*) nil)
	  (incf (aref *F01* id) e)
     	  (incf (aref *F02* id) (* e e))
     	  (incf (aref *F03* id) (* e e e))
	  (incf (aref *F04* id) m)
	  (incf (aref *kapa0* id) h0)
	  (incf (aref *kapa1* id) h1)
	  (incf (aref *kapa2* id) h2)
	  (incf (aref *kapa3* id) h3)
     ) ; end do
     (when (= 0 (mod kPMC 10)) (datos-temporales))
     (when (= 0 (mod kPMC 50)) (recover))
 ) ; end do kPMC
 (when *verbose* (format t "~&Monte Carlo listo"))
) ; end defun

(defun valores-medios ()
"Valores medios sobre rodadas independientes"
 (dotimes (i *Ntmp*)
   (let* ((v1 (/ (aref *F11* i) *PIN*)) (v2 (/ (aref *F12* i) *PIN*))
          (c1 (/ (aref *C11* i) *PIN*)) (c2 (/ (aref *C12* i) *PIN*))
	  (d1 (/ (aref *D11* i) *PIN*)) (d2 (/ (aref *D12* i) *PIN*))
	  (n1 (/ (aref *N11* i) *PIN*)) (n2 (/ (aref *N12* i) *PIN*))
	  (sv (* 2.d0 (Psqr (/ (- v2 (* v1 v1)) (- *PIN* 1)))))
	  (sc (* 2.d0 (Psqr (/ (- c2 (* c1 c1)) (- *PIN* 1)))))
	  (sd (* 2.d0 (Psqr (/ (- d2 (* d1 d1)) (- *PIN* 1)))))
	  (sn (* 2.d0 (Psqr (/ (- n2 (* n1 n1)) (- *PIN* 1))))))
	 (setf (aref *F11* i) v1)
	 (setf (aref *F12* i) sv)
	 (setf (aref *C11* i) c1)
	 (setf (aref *C12* i) sc)
	 (setf (aref *D11* i) d1)
	 (setf (aref *D12* i) sd)
	 (setf (aref *N11* i) n1)
	 (setf (aref *N12* i) sn)
	 (setf (aref *Acp* i) (/ (aref *Acp* i) (aref *Act* i)))
	 (setf (aref *Jcp* i) (/ (aref *Jcp* i) (max 1 (aref *Jtt* i))))
   ) ; end let
 ) ; end dotimes
 (when *verbose* (format t "~&Valores Medios listo"))
) ; end defun

