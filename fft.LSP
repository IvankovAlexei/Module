;;;=============================
;;; Быстрое преобразование Фурье
;;; Алгоритм Кули-Тьюки, 1965
;;; 2026/05/23
;;;=============================

;;; Вспомогательные функции
;;; Элементы массива с нечетными индексами
(defun evens (l)
  (if (null l)
    nil
    (cons (car l) (evens (cddr l)))))
;;; Элементы массива с четными индексами
(defun odds (l)
  (if (null l)
    nil
    (cons (cadr l) (odds (cddr l)))))
;;; Экспонента комплексного числа
(defun cexp (re im / er)
  (setq er (exp re))
  (list (* er (cos im)) (* er (sin im))))
;;; Произведение комплексных чисел
(defun cmult (a b)
  (list	(- (* (car a) (car b)) (* (cadr a) (cadr b)))
	(+ (* (car a) (cadr b)) (* (cadr a) (car b)))))
;;; Сумма комплексных чисел
(defun cadd (a b)
  (list (+ (car a) (car b)) (+ (cadr a) (cadr b))))
;;; Разность комплексных чисел
(defun csub (a b)
  (list (- (car a) (car b)) (- (cadr a) (cadr b))))
;;; Реккурентная функция FFT
(defun fft (x / n even odd l1 l2)
  (setq n (length x))
  (if (<= n 1)
    (setq res x)							; ничего не делаем, просто возвращаем x
    (progn
      (setq even (fft (evens x))
	    odd	 (fft (odds x))
	    k	 0)
      (while (< k (length even))
	(setq a	 (cmult (cexp 0.0 (/ (* -2.0 pi k) n)) (nth k odd))	; поворотный множитель
	      l1 (cons (cadd (nth k even) a) l1)			; первая половина списка
	      l2 (cons (csub (nth k even) a) l2)			; вторая половина списка
	      k	 (1+ k))
      ); while
      (setq res (reverse (append l2 l1)))				; склеиваем и разворачиваем
    ); progn
  ); if
  res
); defun fft
(defun main (/ x res dimz)
  (setq	x '((1.0 0.0) (1.0 0.0) (1.0 0.0) (1.0 0.0)
	    (0.0 0.0) (0.0 0.0) (0.0 0.0) (0.0 0.0))
	res  (fft x)
	dimz (getvar "dimzin"))
  (setvar "dimzin" 0)
;;; Вывод результатов
  (princ "X\t\tFFT\nRe\tIm\tRe\tIm\n")
  (mapcar '(lambda (a b)
	     (princ (strcat (rtos (car a) 2 4)	"\t"
			    (rtos (cadr a) 2 4)	"\t"
			    (rtos (car b) 2 4)	"\t"
			    (rtos (cadr b) 2 4) "\n")))
	  x res)
  (setvar "dimzin" dimz)
  (princ)
)