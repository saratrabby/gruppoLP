;;;; 899988 Alari Matteo
;;;; 909567 Caronni Andrea
;17/07 aggiunte fz is-base-si-unit, is-si-unit, si-unit-name, si-unit-symbol
;si-unit-base-expansion, make-unit-prefixes e vari defparameter. Occhio
;nelle varie defparameter le unita' dovrebbero essere tutte |simboli|
;DA SISTEMARE MULTIPLI DEI KILOGRAMMI

;in Common Lisp la forma canonica `e una lista con operatore * e con
;operandi delle unit`a o delle espressioni del tipo (expt U E).


; Definizione unità SI

(defparameter *si-unit-defs*
  '((Bq . (* (expt s -1)))
    (DC . (* K))
    (C . (* s A))
    (F . (* (expt kg -1) (expt m -2) (expt s 4) (expt A 2)))
    (Gy . (* (expt m 2) (expt s -2)))
    (Hz . (* (expt s -1)))
    (H . (* kg (expt m 2) (expt s -2) (expt A -2)))
    (J . (* kg (expt m 2) (expt s -2)))
    (kat . (* mol (expt s -1)))
    (lm . (* cd sr))
    (lx . (* cd sr (expt m -2)))
    (N . (* kg m (expt s -2)))
    (omega . (* kg (expt m 2) (expt s -3) (expt A -2)))
    (Pa . (* kg (expt m -1) (expt s -2)))
    (rad . (* ))
    (S . (* (expt kg -1) (expt m -2) (expt s 3) (expt A 2)))
    (Sv . (* (expt m 2) (expt s -2)))
    (sr . (* ))
    (T . (* kg (expt s -2) ()expt A -1))
    (V . (* kg (expt m 2) (expt s -3) (expt A -1)))
    (W . (* kg (expt m 2) (expt s -3)))
    (Wb . (* kg (expt m 2) (expt s -2) (expt A -1)))
    ))

;(defparameter *si-base-units*
;  (mapcar (lambda (x) (intern (string-upcase (symbol-name x)) :siunits))
;          '(kg m s A K mol cd)))

(defparameter base-si-units '(kg m s A K cd mol))

;lista di cons-cell simbolo-nome per tutte le unita base e derivate.
(defparameter units-symbol-name
  '((kg . kilogram)
    (m . metre)
    (s . second)
    (A . Ampere)
    (K . Kelvin)
    (cd . candela)
    (mol . mole)
    (Bq . Becquerel)
    (DC . degreecelsius)
    (C . Coulomb)
    (F . Farad)
    (Gy . Gray)
    (Hz . Hertz)
    (H . Henry)
    (J . Joule)
    (kat . Katal)
    (lm . lumen)
    (lx. lux)
    (N . Newton)
    (omega . Ohm)
    (Pa . Pascal)
    (rad . radian)
    (S . Siemens)
    (Sv . Sievert)
    (sr . steradian)
    (T . Tesla)
    (V . Volt)
    (W . Watt)
    (Wb . Weber)
    )
  )

;lista di coppie simbolo prefisso - valore
;NB il simbolo di micro e' uguale al suo nome
(defparameter si-prefixes
 '((|Q| . (expt 10 30))
   (|R| . (expt 10 27))
   (|Y| . (expt 10 24))
   (|Z| . (expt 10 21))
   (|E| . (expt 10 18))
   (|P| . (expt 10 15))
   (|T| . (expt 10 12))
   (|G| . (expt 10 9))
   (|M| . (expt 10 6))
   (|k| . (expt 10 3))
   (|h| . (expt 10 2))
   (|da| .(expt 10 1))
   (|d| . (expt 10 -1))
   (|c| . (expt 10 -2))
   (|m| . (expt 10 -3))
   (|micro| . (expt 10 -6))
   (|n| . (expt 10 -9))
   (|p| . (expt 10 -12))
   (|f| . (expt 10 -15))
   (|a| . (expt 10 -18))
   (|z| . (expt 10 -21))
   (|y| . (expt 10 -24))
   (|r| . (expt 10 -27))
   (|q| . (expt 10 -30))))

;lista contenente tutti i possibili multipli di metro come simboli
(defparameter metro-prefixes
  (make-unit-prefixes 'metre))

;data una unita' SI per nome, restituisce la lista con tutti i
;possibili multipli di quell'unita' come simboli.
;NB questa funzione NON VA BENE PER I KG.
(defun make-unit-prefixes (unit)
  (mapcar 'make-symbol
	  (mapcar
	   (lambda (x)(concatenate 'string x (string (si-unit-symbol unit))))
	   (mapcar 'string (mapcar 'car si-prefixes)))))

;ritorna T se il suo argomento e' un simbolo (la sigla) delle unita' di base
;non e' case sensitive (ovviamente) e non accetta i multipli delle unita'
(defun is-base-si-unit (unit)
  (not
   (null (find unit base-si-units))))

;ritorna T se il suo argomento e' un simbolo che denota una unita SI base o
;derivata. Non accetta i multipli delle unita'.

(defun is-si-unit (unit)
  (or
   (is-base-si-unit unit)
   (not
    (null (find unit (mapcar 'car units-symbol-name))))))

;ritorna il nome del simbolo passato come argomento, altrimenti NIL
;Non accetta i multipli delle unita'.
(defun si-unit-name (unit &optional (i 0))
  (cond
   ((null (nth i units-symbol-name)) NIL)
   ((equal unit
	   (car (nth i units-symbol-name)))
    (cdr (nth i units-symbol-name)))
   (T (si-unit-name unit (+ i 1)))))

;ritorna il simbolo del nome di una unita SI passata come argomento, altrimenti
;NIL. Non accetta i multipli delle unita'.

(defun si-unit-symbol (unit &optional (i 0))
  (cond
   ((null (nth i units-symbol-name)) NIL)
   ((equal unit
	   (cdr (nth i units-symbol-name)))
    (car (nth i units-symbol-name)))
   (T (si-unit-symbol unit (+ i 1))))
  )

; Normalizzazione delle dimensioni

(defun normalize (dim)
;Normalizza una lista di unità: somma esponenti, ordina, rimuove esponenti
;nulli.
  (let ((table (make-hash-table :test #'equal)))
    (dolist (pair dim)
      (let* ((u (car pair))
             (e (cdr pair))
             (old (gethash u table 0)))
        (setf (gethash u table) (+ old e))))
    (let (result)
      (maphash (lambda (u e)
                 (unless (= e 0)
                   (push (cons u e) result)))
               table)
      (sort result #'string< :key (lambda (x) (symbol-name (car x)))))))

; Operazioni tra quantità

(defun same-dim-p (dim1 dim2)
  "Controlla se due dimensioni sono equivalenti dopo normalizzazione."
  (equal (normalize dim1) (normalize dim2)))

(defun make-quantity (value dimension)
  "Crea una quantità composta da valore e dimensione normalizzata."
  (list value (normalize dimension)))

(defun qadd (q1 q2)
  "Somma due quantità con la stessa dimensione."
  (let ((v1 (first q1))
        (d1 (second q1))
        (v2 (first q2))
        (d2 (second q2)))
    (if (same-dim-p d1 d2)
        (list (+ v1 v2) (normalize d1))
        (error "Dimensioni incompatibili in qadd"))))

(defun qsub (q1 q2)
  "Sottrae due quantità con la stessa dimensione."
  (let ((v1 (first q1))
        (d1 (second q1))
        (v2 (first q2))
        (d2 (second q2)))
    (if (same-dim-p d1 d2)
        (list (- v1 v2) (normalize d1))
        (error "Dimensioni incompatibili in qsub"))))
