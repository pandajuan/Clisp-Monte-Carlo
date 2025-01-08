;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                      ;;;
;;; var-param-mc.lisp    (18/03/2004)    JPN             ;;;
;;;                                                      ;;;
;;; Variables y parametros del problema.                 ;;;
;;;                                                      ;;;
;;; EXTERNAS:                                            ;;;
;;;                                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;
;;;                ;;;
;;; Constantes     ;;;
;;;                ;;;
;;;;;;;;;;;;;;;;;;;;;;

(defconstant *E* (exp 1.d0))  ; Usada en las simplificaciones
 
;;;;;;;;;;;;;;;;;;;;;;
;;;                ;;;
;;; Parametros     ;;;
;;;                ;;;
;;;;;;;;;;;;;;;;;;;;;;

(defvar *listat*  :unbound) ; Lista de terminales
(defvar *listaf*  :unbound) ; Lista (punteada) de funciones
(defvar *prof0*   :unbound) ; Maxima jerarquia permitida en generacion 0
(defvar *profn*   :unbound) ; Maxima jerarquia permitida en generacion n
(defvar *Chico*   :unbound) ; Minimo valor permitido para algunas operaciones
(defvar *Grande*  :unbound) ; Maximo valor permitido para algunas operaciones
(defvar *verbose* :unbound) ; Si t aparecen mas mensajes en los infos

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                            ;;;
;;; Variables                  ;;;
;;;                            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *contador*      :unbound) ; Cuenta numero de pasos MC totales lleva.
(defvar *prec*          :unbound) ; Indica cuando hay precalientamiento.
(defvar *eps*           :unbound) ; Parametro de la derivada.
(defvar *PIN*           :unbound) ; Numero de rodadas independientes.
(defvar *PMC*           :unbound) ; Numero de pasos MC por rodada.
(defvar *Ntmp*          :unbound) ; Numero de temperaturas.
(defvar *xi*            :unbound) ; Valor del limite inferior en x.
(defvar *xf*            :unbound) ; Valor del limite superior en x.
(defvar *bmax*          :unbound) ; Temperatura minima.
(defvar *bmin*          :unbound) ; Temperatura maxima.
(defvar *version*       :unbound) ; Version del programa.
(defvar *prueba*        :unbound) ; Numero de prueba de la misma version.
(defvar *path*          :unbound) ; Path al $PWD.
(defvar *dir*           :unbound) ; Path al temporario.
(defvar *gama0*         :unbound) ; Estas ocho calculan diferentes partes
(defvar *gama1*         :unbound) ; del hamiltoniano.
(defvar *gama2*         :unbound)
(defvar *gama3*         :unbound)
(defvar *kapa0*         :unbound)
(defvar *kapa1*         :unbound)
(defvar *kapa2*         :unbound)
(defvar *kapa3*         :unbound)
(defvar *string*        :unbound) ; Formateo de impresion.
(defvar *cpu-tm*        :unbound) ; Tiempo de cpu.
(defvar *real-tm*       :unbound) ; Tiempo real.
(defvar *fitness*       :unbound) ; Fitness compilado.
(defvar *lista-fitness* :unbound) ; Lista de funciones usadas en el fitness.
(defvar *abort*         :unbound) ; Variable de suspencion de la rodada.
(defvar *mezcla*        :unbound) ; Archivo flag para mezcla.
(defvar *recover*       :unbound) ; Guarda el nombre del archivo de rec.
(defvar *recswitch*     :unbound) ; Switch de uso o traba de *recover*.
(defvar *recover0*      :unbound) ; Copia vieja de *recover*.
(defvar *mejor*         :unbound) ; Archivo con los mejores individuos por temp.
(defvar *tmp0*          :unbound) ; Archivos de datos temporarios.
(defvar *tmp1*          :unbound)
(defvar *tmp2*          :unbound)
(defvar *tmp3*          :unbound)
(defvar *out*           :unbound) ; Archivo de salida.
(defvar *inf*           :unbound) ; Archivo info.
(defvar *tmp*           :unbound) ; Algun otro temporario.
(defvar *sistema*       :unbound) ; Conjunto de estructuras (individuos).
(defvar *mejores*       :unbound) ; Array de los mejores.
(defvar *Ac*            :unbound) ; Aceptacion temporaria.
(defvar *Acp*           :unbound) ; Aceptacion acumulada.
(defvar *Act*           :unbound) ; Aceptacion total.
(defvar *Jcp*           :unbound) ; Saltos aceptados.
(defvar *Jtt*           :unbound) ; Saltos totales.
(defvar *F11*           :unbound) ; Acumuladores de diferentes tipos para 
(defvar *F12*           :unbound) ; los valores medios calculados durante
(defvar *C11*           :unbound) ; la rodada.
(defvar *C12*           :unbound)
(defvar *D11*           :unbound)
(defvar *D12*           :unbound)
(defvar *N11*           :unbound)
(defvar *N12*	        :unbound)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                           ;;;
;;; Definicion de la estructura del individuo ;;;
;;;                                           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (individuo)
 (sfit 0.d0)  ; Fitness
 (beta 0.d0)  ; Temperatura
 (ncmb 0)     ; Limites de variacion de profundidad para el paso de Metropolis
 (ncmv 6)
 (nbeta 0)    ; Numero calculado por nbeta que da la jerarquia de transicion.
 (modula nil) ; Algoritmo.
) ; end structure

