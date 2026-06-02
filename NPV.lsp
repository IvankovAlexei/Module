;;; ===================================================
;;; Отрисовка матрицы переходов из одного технического
;;; состояния в другое на основе csv файла
;;; ===================================================



;; Read CSV  -  Lee Mac
;; Parses a CSV file into a matrix list of cell values.
;; csv - [str] filename of CSV file to read

(defun LM:readcsv (csv / des lst sep str)
  (if (setq des (open csv "r"))
    (progn
      (setq
	sep (cond ((vl-registry-read
		     "HKEY_CURRENT_USER\\Control Panel\\International"
		     "sList"
		   )
		  )
		  (",")
	    )
      )
      (while (setq str (read-line des))
	(setq lst (cons (LM:csv->lst str sep 0) lst))
      )
      (close des)
    )
  )
  (reverse lst)
)

;; CSV -> List  -  Lee Mac
;; Parses a line from a CSV file into a list of cell values.
;; str - [str] string read from CSV file
;; sep - [str] CSV separator token
;; pos - [int] initial position index (always zero)

(defun LM:csv->lst (str sep pos / s)
  (cond
    ((not (setq pos (vl-string-search sep str pos)))
     (if (wcmatch str "\"*\"")
       (list
	 (LM:csv-replacequotes (substr str 2 (- (strlen str) 2)))
       )
       (list str)
     )
    )
    ((or (wcmatch (setq s (substr str 1 pos)) "\"*[~\"]")
	 (and (wcmatch s "~*[~\"]*") (= 1 (logand 1 pos)))
     )
     (LM:csv->lst str sep (+ pos 2))
    )
    ((wcmatch s "\"*\"")
     (cons
       (LM:csv-replacequotes (substr str 2 (- pos 2)))
       (LM:csv->lst (substr str (+ pos 2)) sep 0)
     )
    )
    ((cons s (LM:csv->lst (substr str (+ pos 2)) sep 0)))
  )
)

(defun LM:csv-replacequotes (str / pos)
  (setq pos 0)
  (while (setq pos (vl-string-search "\"\"" str pos))
    (setq str (vl-string-subst "\"" "\"\"" str pos)
	  pos (1+ pos)
    )
  )
  str
)
(defun c:NPV (/ data file pt i j n m k p1 p2 p3 p4)
  (if
    (and
      (setq file (getfiled "Выберите CSV файл" "" "csv" 16))
      (setq data (LM:readcsv file))
    )
     (progn
       (setq pt	(getpoint "Укажите точку вставки:\n")
	     i	0
	     n	(length data)
	     m	(length (car data))
       )
       (while (< i n)
	 (setq j 0)
	 (while	(< j m)
	   (if (/= (nth j (nth i data)) "")
	     (progn
	       (entmake	(list (cons 0 "circle")
			      (cons 10
				    (list (+ (car pt) (* j 70.0))
					  (- (cadr pt) (* i 50.0))
				    )
			      )
			      (cons 40 7)
			      (cons 62 0)
			)
	       )
	       (entmake	(list '(0 . "TEXT")
			      (cons 1 (nth j (nth i data)))
			      (cons 10
				    (list (+ (car pt) (* j 70.0))
					  (- (cadr pt) (* i 50.0))
				    )
			      )
			      (cons 11
				    (list (+ (car pt) (* j 70.0))
					  (- (cadr pt) (* i 50.0))
				    )
			      )
			      (cons 40 3.5)
			      (cons 41 0.75)
			      (cons 72 1)
			      (cons 73 2)
			      (cons 370 50)
			)
	       )
	       (setq k i)
	       (while (< k n)
		 (if
		   (and (/= (nth (1+ j) (nth k data)) "") (< j (1- m)))
		    (progn
		      (setq p1 (list (+ (car pt) (* j 70.0))
				     (- (cadr pt) (* i 50.0))
			       )
			    p2 (list (+ (car pt) (* (1+ j) 70.0))
				     (- (cadr pt) (* k 50.0))
			       )
			    p3 (polar p1 (angle p1 p2) 7)
			    p4 (polar p2 (angle p2 p1) 7)
		      )
		      (entmake (list '(0 . "LINE")
				     (cons 10 p3)
				     (cons 11 p4)
				     (cons 370 50)
				     (cons 62 0)
			       )
		      )
		    )
		 )
		 (setq k (1+ k))
	       )
	     )
	   )
	   (setq j (1+ j))
	 )
	 (setq i (1+ i))
       )
     )
  )
  (princ)
)
(princ)