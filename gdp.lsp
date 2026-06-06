;;;=================================================
;;; Построение графика движения поездов для участка 
;;; по данным таблицы *.csv                         
;;;=================================================

(command "_style" "standard" "simplex.shx" "" 0.75 "" "" "" "")
(if (= (tblsearch "ltype" "ACAD_ISO02W100" t) nil)
  (command "_.linetype"	"_Load"	"ACAD_ISO02W100" "acadiso.lin" "")
)
(if (= (tblsearch "ltype" "ACAD_ISO04W100" t) nil)
  (command "_.linetype"	"_Load"	"ACAD_ISO04W100" "acadiso.lin" "")
)

;;; s = string d = delimiter p = position delimiter (thanx Lee Mac)
(defun SplitStr	(s d / p)
  (if (setq p (vl-string-search d s))
    (cons (substr s 1 p)
	  (SplitStr (substr s (+ p 1 (strlen d))) d)
    )
    (list s)
  )
)

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

;;; ***************************************************************
;;; *           Отрисовка полилинии по списку точек               *
;;; ***************************************************************

(defun LWPoly (lst color cls thikn LT / exv)
					; Draw lwpolyline in current UCS
					; version for pre-visual lisp.
					; lst - list of points ((X1 Y1)(X2 Y2) ... (Xn Yn)) in current UCS
					; cls - DXF group 70 flag  to indicate closure : 0 = open, 1 = closed 
  (setq exv (trans (list 0 0 1) 1 0 T))
  (entmakex
    (append (list (cons 0 "LWPOLYLINE")
		  (cons 6 LT)		  
		  (cons 38 (caddr (trans (car lst) 1 exv)))
		  (cons 100 "AcDbEntity")
		  (cons 100 "AcDbPolyline")
		  (cons 90 (length lst))
		  (cons 62 color)
		  (cons 70 cls)
		  (cons 210 exv)
		  (cons 370 thikn)
	    )
	    (mapcar '(lambda (p) (cons 10 (trans p 1 exv))) lst)
    )
  )
)

(defun ProgressiveTotal	(lst / a)
  (setq a 0)
  (mapcar '(lambda (x) (setq a (+ a x))) lst)
)

(defun DrawGrid	(st dst / i p1 p2 lst kv)
  (setq	i  0
	kv (/ 900.0 (car dst))
  )
  (while (<= i 1440)
    (setq p1  (list (float i) 0.0)
	  p2  (list (float i) 900.0)
	  lst (cond
		((= (rem i 60) 0) (list (cons 370 50)))
		((= (rem i 30) 0)
		 (list (cons 6 "ACAD_ISO02W100") (cons 370 35))
		)
		(t nil)
	      )
    )
    (entmake (append (list '(0 . "LINE")
			   (cons 8 "Сетка")
			   (cons 10 p1)
			   (cons 11 p2)
			   (cons 62 106)
		     )
		     lst
	     )
    )
    (if	(= (rem i 60) 0)
      (progn
	(entmake (list '(0 . "TEXT")
		       (cons 1 (itoa (/ i 60)))
		       (cons 8 "Сетка")
		       (cons 10 (polar p1 (- (/ pi 2.0)) 2))
		       (cons 11 (polar p1 (- (/ pi 2.0)) 2))
		       (cons 40 7.0)
		       (cons 41 0.75)
		       (cons 72 1)
		       (cons 73 3)
		 )
	)
	(entmake (list '(0 . "TEXT")
		       (cons 1 (itoa (/ i 60)))
		       (cons 8 "Сетка")
		       (cons 10 p2)
		       (cons 11 p2)
		       (cons 40 7.0)
		       (cons 41 0.75)
		       (cons 72 1)
		       (cons 73 1)
		 )
	)
      )
    )
    (setq i (+ 10 i))
  )
  (setq i 0)
  (foreach x dst
    (setq p1 (list 0.0 (* x kv))
	  p2 (list 1440.0 (* x kv))
    )
    (entmake (list '(0 . "LINE")
		   (cons 8 "Сетка")
		   (cons 10 p1)
		   (cons 11 p2)
		   (cons 62 106)
		   (cons 370 50)
	     )
    )
    (setq p1 (polar p1 pi 5.0)
	  p2 (polar p2 0.0 5.0)
    )
    (entmake (list '(0 . "TEXT")
		   (cons 1 (nth i st))
		   (cons 8 "Сетка")
		   (cons 10 p1)
		   (cons 11 p1)
		   (cons 40 7.0)
		   (cons 41 0.75)
		   (cons 72 2)
		   (cons 73 2)
	     )
    )
    (entmake (list '(0 . "TEXT")
		   (cons 1 (nth i st))
		   (cons 8 "Сетка")
		   (cons 10 p2)
		   (cons 11 p2)
		   (cons 40 7.0)
		   (cons 41 0.75)
		   (cons 72 0)
		   (cons 73 2)
	     )
    )
    (setq i (1+ i))
  )
)

(defun midpt (p1 p2)
  (list (/ (+ (car p1) (car p2)) 2.0) (/ (+ (cadr p1) (cadr p2)) 2.0))
)

(defun DrawTrain (train	dist / lst TrNum i kv l	pt a ind p1 p2 pf lst1 lst2 tjv tjh minute even odd LineColor LineType ang)
  (setq	kv    (/ 900.0 (car dist))
	TrNum (car train)
	train (cdr train)
	lst   nil
	i     0
	pf nil ; Обнуляем переменную, отвечающую за переход через начало (конец) суток 
  )
  (while (< i (length train))
    (if	(and (/= (nth i train) "") (/= (nth i train) nil))
      (setq l	(mapcar 'atoi (SplitStr (nth i train) ":"))
	    lst	(cons (list (+ (* (car l) 60) (cadr l))
			    (* (nth (/ i 2) dist) kv)) lst))
    )
    (setq i (1+ i))
  )
  (setq	i   0
	ind nil
  )
  (while (< i (1- (length lst)))
    (setq p1 (nth i lst)
	  p2 (nth (1+ i) lst)
    )
    (if	(> (distance p1 p2) 1000.0)
      (setq pf	(list 0.0
		      (- (cadr p1)
			 (* (/ (- (cadr p1) (cadr p2))
			       (+ (- (car p1) (car p2)) 1440.0)
			    )
			    (car p1)
			 )
		      )
		)
	    ind	i
      )
    )
    (setq i (1+ i))
  )
  (setq LineColor
	 (cond
	   ((< (atoi TrNum) 1000) 1)
	   ((< (atoi TrNum) 6000) 0)
	   (t 74)
	 )
	LineType
	 (cond
	   ((and (> (atoi TrNum) 3400) (< (atoi TrNum) 4000)) "ACAD_ISO04W100")
	   ((and (> (atoi TrNum) 4000) (< (atoi TrNum) 4500)) "ACAD_ISO02W100")
	   (t "ByLayer")
	 )
  )
  (if (not pf)
    ;;; Нитка полностью уложилась в текущие сутки безе перехода через 1440 (24 часа)
    (LWPoly lst LineColor 0 50 LineType)
    (progn
      ;;; Поиск точки перехода через 1440, раззделение списка на два (в конце суток и в начале)
      (setq lst1 nil
	    lst2 nil
	    i	 0
      )
      (foreach a lst
	(if (<= i ind)
	  (setq lst1 (cons a lst1))
	  (setq lst2 (cons a lst2))
	)
	(setq i (1+ i))
      )
      (if (< (car (car lst1)) (car (last lst2)))
	(setq lst1 (cons pf lst1)
	      lst2 (append lst2 (list (polar pf 0.0 1440.0))))
	(setq lst1 (cons (polar pf 0.0 1440.0) lst1)
	      lst2 (append lst2 (list pf)))
      )	
      (LWPoly lst1 LineColor 0 50 LineType)
      (LWPoly lst2 LineColor 0 50 LineType)
    )
  )
  ;;; Вывод минут на график
  ;;; Определяем четный (нечетный) поезд
  (setq	i    0
	even (if (not pf)
	       (> (car (last lst)) (car (car lst)))
	       (< (car (last lst)) (car (car lst)))
	     )  
	odd  (not even)
  )
  ;;; Рассчитываем минуты от 0..9
  (foreach a lst
    (setq minute (rem (car a) 10)
	  pt	 a)
    ;;; Задаем выравнивание текста
    (cond
      ((and even (or (= i 0) (< (cadr pt) (cadr (nth (1+ i) lst)))))
       (setq tjh 2 tjv 1 pt (polar pt pi 1.0)))
      (even (setq tjh 0 tjv 3 pt (polar pt (* (/ 7.0 4.0) pi) 1.0)))
      ((and odd (or (= i 0) (equal (cadr pt) (cadr (nth (1- i) lst)) 0.01)))
       (setq tjh 0 tjv 1 pt (polar pt 0.0 1.0)))
      (t (setq tjh 2 tjv 3 pt (polar pt (* (/ 5.0 4.0) pi) 1.0)))
    )
    (entmake (list '(0 . "TEXT")
		   (cons 1 (itoa minute))
		   (cons 10 pt)
		   (cons 11 pt)
		   (cons 40 5.0)
		   (cons 41 0.75)
		   (cons 62 LineColor)
		   (cons 72 tjh)
		   (cons 73 tjv)))
    (setq i (1+ i))
  )
  ;;; Вывод номера поезда
  (if even
    (progn
      (if pf (setq lst (reverse lst1)))      
      (setq p1 (car lst) p2 (cadr lst)))
    (progn
      (if pf (setq lst (reverse lst2)))
      (setq p1 (car (reverse lst)) p2 (cadr (reverse lst)))))
  (setq pt (midpt p1 p2)
	ang (angle p1 p2))
  (entmake (list '(0 . "TEXT")
		 (cons 1 TrNum)
		 (cons 10 pt)
		 (cons 11 pt)
		 (cons 40 7.5)
		 (cons 41 0.75)
		 (cons 50 ang)
		 (cons 62 LineColor)
		 (cons 72 1)
		 (cons 73 1)))  
)

(defun c:gdp (/ data file stations dist i shedule)
  (if
    (and (setq file (getfiled "Select CSV File"
			      "" ;"F:\\Text\\acad_support\\stan\\module\\5.csv"
			      "csv" 8)) (setq data (LM:readcsv file)))
     (progn
       (princ "\n(")
       (foreach	line data
	 (princ "\n    ")
	 (prin1 line)
       )
       (princ "\n)")
     )
  )
  (setq	stations  nil
	dist	  nil
	TabHeader (car data)
	i	  1
  )
  (while (< i (length TabHeader))
    (setq stations (cons (nth i TabHeader) stations)
	  dist	   (cons (nth (1+ i) TabHeader) dist)
	  i	   (+ 2 i)
    )
  )
  (setq	stations (reverse stations)
	dist	 (reverse
		   (cons 0 (ProgressiveTotal (mapcar 'atoi (cdr dist))))
		 )
  )
  (DrawGrid stations dist)
  (setq shedule (cdr data))
  (foreach x shedule
    (DrawTrain x dist)
  )
  (print)
)
