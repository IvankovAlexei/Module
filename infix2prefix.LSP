;| Infix notation Converter and Calculator
	Supports: + - * / ^ ( )
_Examples_

(str2prefix "B*8-5/2^(2 PI)")  				-> 	(- (* B 8) (/ 5 (EXPT 2 (* 2 PI))))

(setq B 4.6)
(calculate "B*8-5/2^(2 PI)")   				->	36.7358


(eval (infix-prefix '(10 - 3 - 2 - 1))) 	->  4

(infix-prefix '(10 - 3 - 2 - 1)) 			->	(- (- (- 10 3) 2) 1)

It also handles unary - and +, and implicit multiplication:
(infix-prefix '(- 2 a + b))					->	(+ (* (- 2) A) B)

|;

;Original code from: http://www.lispology.com/show?JIH  by: johnsondavies


(defun str2prefix (str / i cache)
	(setq i 1 cache "(")
	(repeat (strlen str)
		(setq char (substr str i 1))
		(if (member char '("+" "-" "*" "/" "(" ")" "^" "%"))
			(setq cache (strcat cache " " char " "))
			(setq cache (strcat cache char))
		)
		(setq i (1+ i))
	)
	(setq cache (strcat cache ")"))
	(infix-prefix (read cache))
)

(defun calculate (str) (eval (str2prefix str)))


(setq *binary-operators* 	'((+ 1 +) (- 1 -) (* 2 *) (/ 2 /) (^ 3 expt)))   ;removed (x 2 *) to avoid accidents
(setq *unary-operators* 	'((+ 4 +) (- 4 -)))
(defun weight (c) 			(cadr (assoc c *binary-operators*)))
(defun binary-opcode (c) 	(caddr (assoc c *binary-operators*)))
(defun unary-opcode (c) 	(caddr (assoc c *unary-operators*)))

(defun infix-prefix (ae)
  	(cond 
	  	((atom ae) ae)
        (t (inf-aux ae nil nil))
	)
)

(defun inf-aux (ae operators operands)
  (cond
   ;; Unary operator
   ((and (atom (car ae)) (assoc (car ae) *unary-operators*))
    (inf-iter (cddr ae) operators (cons
                                   (list
                                    (unary-opcode (car ae))
                                    (infix-prefix (cadr ae))) operands)))
   (t (inf-iter (cdr ae) operators (cons (infix-prefix (car ae)) operands)))))

(defun inf-iter (ae operators operands)
  (cond 
   ((and (null ae) (null operators)) (car operands))
   ;; Implicit multiplication
   ((and ae
         (or (listp (car ae))
             (null (weight (car ae)))))
    (inf-iter (cons '* ae) operators operands))
   ((and ae
         (or (null operators)
             (> (weight (car ae)) (weight (car operators)))))
    (inf-aux (cdr ae) (cons (car ae) operators) operands))
   (t 
    (inf-iter ae (cdr operators) 
              (cons (list (binary-opcode (car operators))
                          (cadr operands) (car operands)) (cddr operands))))))