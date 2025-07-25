;;;; 899988 Alari Matteo
;;;; 909567 Caronni Andrea
;;;; 914295 Trabattoni Sara
;17/07 aggiunte fz is-base-si-unit, is-si-unit, si-unit-name, si-unit-symbol
;si-unit-base-expansion, make-unit-prefixes e vari defparameter. Occhio
;nelle varie defparameter le unita' dovrebbero essere tutte |simboli|
;19/07 create fz check-prefixed-si-unit e affini SISTEMO VAL RITORNO.
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

(defparameter base-si-units '(|kg| |m| |s| |A| |K| |cd| |mol|))

;lista di cons-cell simbolo-nome per tutte le unita base e derivate.
(defparameter units-symbol-name
  '((|kg| . kilogram)
    (|m| . metre)
    (|s| . second)
    (|A| . Ampere)
    (|K| . Kelvin)
    (|cd| . candela)
    (|mol| . mole)
    (|Bq| . Becquerel)
    (|DC| . degreecelsius)
    (|C| . Coulomb)
    (|F| . Farad)
    (|Gy| . Gray)
    (|Hz| . Hertz)
    (|H| . Henry)
    (|J| . Joule)
    (|kat| . Katal)
    (|lm| . lumen)
    (|lx|. lux)
    (|N| . Newton)
    (|omega| . Ohm)
    (|Pa| . Pascal)
    (|rad| . radian)
    (|S| . Siemens)
    (|Sv| . Sievert)
    (|sr| . steradian)
    (|T| . Tesla)
    (|V| . Volt)
    (|W| . Watt)
    (|Wb| . Weber)
    )
  )

;lista di tutte le unita si come stringhe (i simboli) in minuscolo
;con kg.

(defparameter units-symbol-string-withkg
  (mapcar 'string-downcase (mapcar 'string (mapcar 'car units-symbol-name))))

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




;data una unita' SI per nome, restituisce la lista con tutti i
;possibili multipli di quell'unita' come simboli.
;NB questa funzione NON VA BENE PER I KG.
(defun make-unit-prefixes (unit)
  (mapcar 'make-symbol
	  (mapcar
	   (lambda (x)(concatenate 'string x
				    (string (si-unit-symbol unit))))
	   (mapcar 'string (mapcar 'car si-prefixes)))))

;dato un simbolo, restituisce la lista con tutti i prefissi applicati al
;simbolo. Da usare con g (grammo).
(defun make-symbol-prefixes (symb)
    (mapcar 'make-symbol
	  (mapcar
	   (lambda (x)(concatenate 'string x
				    (string symb)))
	   (mapcar 'string (mapcar 'car si-prefixes)))))

;ritorna T se il suo argomento e' un simbolo (la sigla) delle unita' di base
;non e' case sensitive (ovviamente) e non accetta i multipli delle unita'
;da wikipedia: "The grouping formed by a prefix symbol attached to a unit symbol
;(e.g. 'km', 'cm') constitutes a new inseparable unit symbol."
;allora i multipli non sono unita' di base. 
(defun is-base-si-unit (unit)
  (not
   (null (find unit base-si-units))))

;ritorna T se il suo argomento e' un simbolo che denota una unita SI base o
;derivata. Non accetta i multipli delle unita', solo unita' si base e
;derivate.

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
   ((equal (string unit)
	   (string(car (nth i units-symbol-name))))
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

;ritorna l'espansione canonica dell'unità S (anche con multipli) in termini di
;unità base. Se S è già un'unità base, restituisce una lista.
(defun si-unit-base-expansion (S)
  (labels ((strip-prefix (unit)
            (let ((unit-str (string-downcase (string unit))))
              (loop for (prefix . val) in si-prefixes
                    for prefix-str = (string-downcase (string prefix))
                    when (and (>= (length unit-str) (length prefix-str))
                              (string= prefix-str (subseq unit-str 0 (length prefix-str))))
                    do (return (intern (subseq unit-str (length prefix-str)) :siunits))
                    finally (return unit)))))
    (let* ((base-unit (strip-prefix S))
           (def (cdr (assoc base-unit *si-unit-defs*))))
      (cond
        ((null def)
         (if (is-base-si-unit base-unit)
             (list (cons base-unit 1)) 
             (error "Unità non riconosciuta: ~A" S)))
        ((symbolp def) (si-unit-base-expansion def)) 
        ((and (listp def) (eq (car def) '*))
         (normalize
          (apply #'append
                 (mapcar (lambda (term)
                           (cond
                             ((and (listp term) (eq (car term) 'expt))
                              (let ((u (cadr term))
                                    (e (caddr term)))
                                (mapcar (lambda (x)
                                          (cons (car x) (* (cdr x) e)))
                                        (si-unit-base-expansion u))))
                             ((symbolp term)
                              (si-unit-base-expansion term))
                             (t nil)))
                         (cdr def)))))
        (t (error "Forma non gestita per l'unità: ~A" S))))))

;Confronta due unità, restituendo come result uno dei simboli <, >, o =.
(defun compare-units (u1 u2)
  (labels ((strip-prefix (unit)
            (let ((unit-str (string-downcase (string unit))))
              (loop for (prefix . val) in si-prefixes
                    for prefix-str = (string-downcase (string prefix))
                    when (and (>= (length unit-str) (length prefix-str))
                              (string= prefix-str (subseq unit-str 0 (length prefix-str))))
                    do (return (intern (subseq unit-str (length prefix-str)) :siunits))
                    finally (return unit)))))
    (let* ((base-u1 (strip-prefix u1))
           (base-u2 (strip-prefix u2))
           (idx1 (position base-u1 (mapcar 'car units-symbol-name)))
           (idx2 (position base-u2 (mapcar 'car units-symbol-name))))
      (cond
        ((= idx1 idx2) '=)
        ((< idx1 idx2) '<)
        (t '>)))))

;ritorna T se qt e' una quantita' (ovvero un numero)

(defun is-quantity (q)
  (and (listp q)
       (eq 'q (first q))
       (let ((n (second q))
	     (d (third q))
	     )
	 (and (numberp n)
	      (is-dimension d)))))

;ritorna T se dim e' una dimensione, ovvero un simbolo di unita' base o derivat
;a oppure una lista con operatore * e operandi unita' o espressioni
;(expt u e)
;NB (expt m 2) non e' valido (da solo), (* (expt m 2 )) si.

(defun is-dimension (dim)
  (cond ((and (not (listp dim))(not (null (decompose-si-unit dim))) t))
	((and (listp dim)) (equal (first dim) '*)
	 (and-list (mapcar 'is-unit-operand
			      (rest dim))))))

;restituisce t se tutti gli elementi di una lista sono T,altrimenti NIL.

(defun and-list (boolist)
  (cond ((null boolist) T)
	(t (and (car boolist) (and-list (rest boolist))))))

;ritorna T se dim e' un unita' SI (multiplo o non) o se e' una espressione
;del tipo (expt u e) con u unita' si (multiplo o non) ed e

(defun is-unit-operand (operand)
  (cond ((listp operand) (and (equal (first operand) 'expt)
			     (not (null (decompose-si-unit (second operand))))
			     (numberp (third operand))))
	((not (null (decompose-si-unit operand))) t)))

;restituisce l'unita' se l argomento e' un unita' base e derivata senza prefisso
;se ha il prefisso restituisce ('unitabase expt 10 val) altrimenti rest NIL.

(defun decompose-si-unit (dim)
  (cond ((equal dim '|kg|) dim)
	((is-si-unit dim) dim)
	((not (null (check-prefixed-si-unit dim)))
	(cons (make-symbol (check-prefixed-si-unit dim))
	      (rest (car
		     (decompose-prefixed-si-unit dim
					    (check-prefixed-si-unit dim))))))))

;restituisce simbolo pref e potenza di dieci dato argomento unita' con prefisso
;e unita' corrispondente senza prefisso es. cm m, microomega omega
;NB decompone kg in g * 10 exp 3

(defun decompose-prefixed-si-unit (prefixed base)
  (remove-if-not (lambda (x)
	       ;cerca il prefisso
		   (and (not (null x))
			(equal (string (car x))
		      (subseq (string prefixed) 0
			      (min (length (string prefixed))
				   (length (string (car x))))))
	       ;assicurati che dopo il prefisso ci sia l'unita'
		    (equal (string base)
			   (subseq
			    (string prefixed)
			    (min (length (string prefixed))
				  (length (string (car x))))))))
	     si-prefixes))

;ritorna l'unita senza prefisso se l'argomento e' un unita' con prefisso
;altrimenti NIL.
;prima e' saggio verificare che l'argomento sia un simbolo si senza prefisso.
;NB se l'argomento e' kg restituisce g.

(defun check-prefixed-si-unit (dim)
  (first (remove-if
	  (lambda (x)
	    (null (find-if
		   (lambda (y) (equal y (string dim)))
		   (mapcar 'string (if (equal x "g")
				      (make-symbol-prefixes (make-symbol x))
				      (make-unit-prefixes
				       (si-unit-name
					(make-symbol x))))))))
	   (find-units-list (string dim)))))

;ritorna una lista contenente tutti i simboli unita si senza prefissi
;che compaiono nella stringa argomento
;nb al posto di kg cerca g
(defun find-units-list (dim)
  (remove-if (lambda (x) (null(search x dim)))
	     ;sostituisci kg con g
	     (substitute-if "g" (lambda (x) (equal x "kg"))
			    (mapcar 'string
				    (mapcar 'car units-symbol-name)))))

; Normalizzazione delle dimensioni

;(defun normalize (dim)
;Normalizza una lista di unità: somma esponenti, ordina, rimuove esponenti
;nulli.
;  (let ((table (make-hash-table :test #'equal)))
;    (dolist (pair dim)
;      (let* ((u (car pair))
;             (e (cdr pair))
;             (old (gethash u table 0)))
;        (setf (gethash u table) (+ old e))))
;    (let (result)
;      (maphash (lambda (u e)
;                 (unless (= e 0)
;                   (push (cons u e) result)))
;               table)
;      (sort result #'string< :key (lambda (x) (symbol-name (car x)))))))


;NB normalize tratta unita che differiscono solo per il prefisso come se
;fossero uguali es. (* (expt |cm| 1) (expt |m| 1)) diventa (* (expt |m|

(defun normalize (dim)
  ;se dim e' un unita' con o senza prefisso restituisce (* dim)
  (cond ((and
	  (not (listp dim))
	  (or (is-si-unit dim)
	      (not (null (check-prefixed-si-unit dim)))))
	 (list '* dim))
	 ((and (listp dim) (equal '* (first dim))
	       (cons '*
		     (simplify-base-units-list (sort (mapcar 'first
			     (sum-base-units
			      (mapcar 'expt-base-unit
				      (rest dim))))
					(lambda (x y) (string<=
					  (string-downcase (string (second x)))
				       	    (string-downcase
						  (string (second y))))))))))))

;prende come argomento una lista di (expt u e) e restituisce una lista con
;alcune modiche. per ogni elemento della lista, se e = 1, sostituisci con
;solo l'unita', se e = 0, rimuovi.

(defun simplify-base-units-list (ls)
  (mapcar
   (lambda (x)
     (cond ((equal 1 (third x)) (second x)) (t x)))
   (remove-if (lambda (x) (equal 0 (third x))) ls)))

;prende come argomento una lista di ((expt u e) esp) (ottenuta da fz
;expt-base-unit. Prende tutte gli elementi corrispondenti ad un unita si e
;calcola il prodotto. es. ((expt |m| 2) 2) ((expt |m| 1) 2) diventa
;((expt |m| 3) 4)

(defun sum-base-units (ls)
  (let ((base-units
	 (remove-duplicates (mapcar (lambda (x) (second (first x))) ls)
			    :test (lambda (x y)
				    (equal (string x) (string y))))))
  (mapcar 'sum-same-base-units
	  (mapcar (lambda (x)
		    (remove-if-not
		     (lambda (y)
		       (equal (string x) (string (second (first y)))))
		     ls))
	  base-units))))

;prende come argomento una lista di ((expt u e) esp) e da per scontato che siano
;tutte della stessa unita'.
;calcola il prodotto. es. ((expt |m| 2) 2) ((expt |m| 1) 2) diventa
;((expt |m| 3) 4)

(defun sum-same-base-units (ls)
  (if (not (null ls))
      (list (list 'expt (second (first (first ls)))
		  (reduce #'+ (mapcar (lambda (x) (third (first x))) ls)))
			    (reduce #'* (mapcar (lambda (x) (second x)) ls)))))


;se l'argomento � un unita' base restituisce (expt dim 1)
;se l'argomento � un unita' con prefisso rest. ((expt dim 1) esp) dove esp
;e' l'esponente corrispondente al prefisso.

;se l'argomento � una lista (expt u e) restituisce
;((expt u1 e) num) dove u1 e' l' unita base corrispondente a u
;e num e' la potenza di dieci per fare la conversione tra unita' mantenendo la
;quantita' equivalente es. (expt cm 1) diventa ((expt m 1) -2)
;per calcolare num bisogna moltiplicare e per l'esponente corrispondente al
;prefisso es (expt |cm| 3) diventa ((expt |m| 3) -6) ovvero -2 * 3.

(defun expt-base-unit (dim)
  (cond
    ((listp dim) (let ((ex (first dim)) (u (second dim)) (e (third dim))
		       (ubase
			       ((lambda (x) (if (is-si-unit x) x
						(check-prefixed-si-unit
						  x)))
				(second dim))))
		   (if (and (equal ex 'expt) (numberp e))
		       (cond ((is-si-unit u) (list dim 1))
			     ((not (null ubase))
			      (list (list 'expt ubase e)
				    (* e (car (last
					       (first
						(decompose-prefixed-si-unit
						 u ubase)))))))))))
    ((is-si-unit dim) (list (list 'expt dim 1) 1))
    (t (let ((ubase (make-symbol (check-prefixed-si-unit dim))))
	 (if
	  (not (null ubase))
	  (list (list 'expt ubase 1)
		(car
		 (last
		  (first (decompose-prefixed-si-unit dim ubase))))))))))

; Operazioni tra quantità

(defun q (N D)
  "Costruttore di quantità: restituisce (Q N D') con dimensione normalizzata."
  (list 'Q N (normalize D)))

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

(defun qmul (q1 q2)
  "Moltiplica due quantità, sommando le dimensioni."
  (let ((v1 (first q1))
        (d1 (second q1))
        (v2 (first q2))
        (d2 (second q2)))
    (list (* v1 v2)
          (normalize (append d1 d2)))))

(defun qdiv (q1 q2)
  "Divide due quantità, sottraendo le dimensioni."
  (let ((v1 (first q1))
        (d1 (second q1))
        (v2 (first q2))
        (d2 (second q2)))
    (list (/ v1 v2)
          (normalize (append d1
                             (mapcar (lambda (x)
                                       (cons (car x) (- (cdr x))))
                                     d2))))))

(defun qexp (q n)
  "Eleva una quantità a potenza intera n."
  (let ((v (first q))
        (d (second q)))
    (list (expt v n)
          (normalize (mapcar (lambda (x)
                               (cons (car x) (* n (cdr x))))
                             d)))))
