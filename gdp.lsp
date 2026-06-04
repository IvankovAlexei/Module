(command "_style" "standard" "simplex.shx" "" 0.75 "" "" "" "")
(if (= (tblsearch "ltype" "ACAD_ISO02W100" t) nil)
  (command "_.linetype" "_Load" "ACAD_ISO02W100" "acadiso.lin" "")
)

;;; s = string d = delimiter p = position delimiter (thanx Lee Mac)
(defun SplitStr ( s d / p )
  (if (setq p (vl-string-search d s))
    (cons (substr s 1 p) (SplitStr (substr s (+ p 1 (strlen d))) d)) (list s)))

;; Read CSV  -  Lee Mac
;; Parses a CSV file into a matrix list of cell values.
;; csv - [str] filename of CSV file to read
 
(defun LM:readcsv ( csv / des lst sep str )
    (if (setq des (open csv "r"))
        (progn
            (setq sep (cond ((vl-registry-read "HKEY_CURRENT_USER\\Control Panel\\International" "sList")) (",")))
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
 
(defun LM:csv->lst ( str sep pos / s )
    (cond
        (   (not (setq pos (vl-string-search sep str pos)))
            (if (wcmatch str "\"*\"")
                (list (LM:csv-replacequotes (substr str 2 (- (strlen str) 2))))
                (list str)
            )
        )
        (   (or (wcmatch (setq s (substr str 1 pos)) "\"*[~\"]")
                (and (wcmatch s "~*[~\"]*") (= 1 (logand 1 pos)))
            )
            (LM:csv->lst str sep (+ pos 2))
        )
        (   (wcmatch s "\"*\"")
            (cons
                (LM:csv-replacequotes (substr str 2 (- pos 2)))
                (LM:csv->lst (substr str (+ pos 2)) sep 0)
            )
        )
        (   (cons s (LM:csv->lst (substr str (+ pos 2)) sep 0)))
    )
)
 
(defun LM:csv-replacequotes ( str / pos )
    (setq pos 0)
    (while (setq pos (vl-string-search  "\"\"" str pos))
        (setq str (vl-string-subst "\"" "\"\"" str pos)
              pos (1+ pos)
        )
    )
    str
)

;;; ***************************************************************
;;; *           Отрисовка полилинии по списку точек               *
;;; ***************************************************************

(defun LWPoly (lst color cls thikn / exv)
					; Draw lwpolyline in current UCS
					; version for pre-visual lisp.
					; lst - list of points ((X1 Y1)(X2 Y2) ... (Xn Yn)) in current UCS
					; cls - DXF group 70 flag  to indicate closure : 0 = open, 1 = closed 
  (setq exv (trans (list 0 0 1) 1 0 T))
  (entmakex
    (append (list (cons 0 "LWPOLYLINE")
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

(defun ProgressiveTotal (lst / a)
  (setq a 0)
  (mapcar '(lambda (x) (setq a (+ a x))) lst))

(defun DrawGrid (st dst / i p1 p2 lst kv)
  (setq i 0
	kv (/ 900.0 (car dst)))
  (while (<= i 1440)
    (setq p1 (list (float i) 0.0)
	  p2 (list (float i) 900.0)
	  lst (cond
		((= (rem i 60) 0) (list (cons 370 50)))
		((= (rem i 30) 0) (list (cons 6 "ACAD_ISO02W100") (cons 370 35)))
		(t nil)
	  )
    )
    (entmake (append (list '(0 . "LINE") (cons 8 "Сетка") (cons 10 p1) (cons 11 p2) (cons 62 106)) lst))
    (if (= (rem i 60) 0)
      (progn
	(entmake (list '(0 . "TEXT") (cons 1 (itoa (/ i 60))) (cons 8 "Сетка") (cons 10 p1) (cons 11 p1) (cons 40 7.0) (cons 41 0.75) (cons 72 1) (cons 73 3)))
	(entmake (list '(0 . "TEXT") (cons 1 (itoa (/ i 60))) (cons 8 "Сетка") (cons 10 p2) (cons 11 p2) (cons 40 7.0) (cons 41 0.75) (cons 72 1) (cons 73 1)))
    ))
    (setq i (+ 10 i))
  )
  (setq i 0)
  (foreach x dst
    (setq p1 (list 0.0 (* x kv))
	  p2 (list 1440.0 (* x kv)))
    (entmake (list '(0 . "LINE") (cons 8 "Сетка") (cons 10 p1) (cons 11 p2) (cons 62 106) (cons 370 50)))
    (setq p1 (polar p1 pi 5.0)
	  p2 (polar p2 0.0 5.0))
    (entmake (list '(0 . "TEXT") (cons 1 (nth i st)) (cons 8 "Сетка") (cons 10 p1) (cons 11 p1) (cons 40 7.0) (cons 41 0.75) (cons 72 2) (cons 73 2)))
    (entmake (list '(0 . "TEXT") (cons 1 (nth i st)) (cons 8 "Сетка") (cons 10 p2) (cons 11 p2) (cons 40 7.0) (cons 41 0.75) (cons 72 0) (cons 73 2)))
    (setq i (1+ i))
  )
)
(defun DrawTrain (train dist / lst TrNum i kv l minute pt tjh tjv a even odd)
  (setq kv (/ 900.0 (car dist))
	TrNum (car train)
	train (cdr train)
	lst nil
	i 0)
  (while (< i (length train))
    (setq l (mapcar 'atoi (SplitStr (nth i train) ":"))
	  lst (cons (list (+ (* (car l) 60) (cadr l)) (* (nth (/ i 2) dist) kv)) lst))
    (if (and (/= (nth (1+ i) train) "") (/= (nth (1+ i) train) nil))
      (setq l (mapcar 'atoi (SplitStr (nth (1+ i) train) ":"))
	    lst (cons (list (+ (* (car l) 60) (cadr l)) (* (nth (/ i 2) dist) kv)) lst))
    )
    (setq i (+ 2 i))
  )
  (LWPoly lst 0 0 50)
  (setq i 0
	even (> (car (last lst)) (car (car lst)))
	odd (not even)
  )
  (foreach a lst
    (setq minute (rem (car a) 10)
	  pt a
	  )
    (cond
      ((and even (or (= i 0) (< (cadr pt)(cadr (nth (1+ i) lst))))) (setq tjh 2 tjv 1))
      (even (setq tjh 0 tjv 3))
      ((and odd (or (= i 0) (equal (cadr pt)(cadr (nth (1- i) lst)) 0.01))) (setq tjh 0 tjv 1))
      (t (setq tjh 2 tjv 3))
    )
    (entmake (list '(0 . "TEXT") (cons 1 (itoa minute)) (cons 10 pt) (cons 11 pt) (cons 40 5.0) (cons 41 0.75) (cons 72 tjh) (cons 73 tjv)))
    (setq i (1+ i))
  )
)  

(defun main (/ data file stations dist i shedule)
  (if
    (and
      (setq file (getfiled "Select CSV File" "D:\\aivankov\\stan\\modules\\ГДП\\1.csv" "csv" 8))
      (setq data (LM:readcsv file))
    )
     (progn
       (princ "\n(")
       (foreach	line data
	 (princ "\n    ")
	 (prin1 line)
       )
       (princ "\n)")
     )
  )
  (setq	stations nil
	dist nil
	TabHeader (car data)
	i 1
  )
  (while (< i (length TabHeader))
    (setq stations (cons (nth i TabHeader) stations)
	  dist	   (cons (nth (1+ i) TabHeader) dist)
	  i	   (+ 2 i)
    )
  )
  (setq	stations	(reverse stations)
	dist		(reverse (cons 0 (ProgressiveTotal (mapcar 'atoi (cdr dist)))))
  )
  (DrawGrid stations dist)
  (setq shedule (cdr data))
  (foreach x shedule
    (DrawTrain x dist)
  )
  (print)
)
