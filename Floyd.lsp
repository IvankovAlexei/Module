;;; Извлечь значение ii,jj
;;; (print (Mget '((1 2 3) (4 5 6) (7 8 9)) 2 3)) => 6
(defun Mget (Matrix ii jj)
  (nth (1- jj) (nth (1- ii) Matrix))
)

;;; Присвоить ii,jj значение
;;; (print (Mput '((1 2 3) (4 5 6) (7 8 9)) 1 3 25)) => ((1 2 25) (4 5 6) (7 8 9))
(defun Mput (Matrix ii jj elem / i j)
  (setq i 0)
  (mapcar '(lambda (r)
	     (setq j 0 i (1+ i))
	     (mapcar '(lambda (s)
			(setq j (1+ j))
			(if (and (= i ii) (= j jj))
			  elem
			  s))
		     r))
	  Matrix))

;;;{Реализация алгоритма Флойда-Уоршелла}
;;;program FloydWarshall;
;;;const
;;;  n = 5; {количество вершин графа}
;;;  INFINITY = MaxInt;
;;;type
;;;  TRow = array [0..n - 1] of integer;
;;;  TVertex = array [0..n - 1] of TRow;
;;;const
;;;  Vertex1: TVertex =
;;;    (
;;;    (0, 0, 5, 0, 0),
;;;    (7, 0, 0, 0, 10),
;;;    (0, 7, 0, 0, 0),
;;;    (0, 0, 6, 0, 4),
;;;    (0, 0, 1, 0, 0)
;;;    );
;;;  Vertex2: TVertex =
;;;    (
;;;    (0, 3, 2, 0, 0),
;;;    (0, 0, 0, 4, 0),
;;;    (0, 0, 0, 6, 0),
;;;    (0, 0, 0, 0, 2),
;;;    (0, 0, 0, 0, 0)
;;;    );
;;;  procedure FloydWarshall(v: TVertex; n: integer; var d, p: TVertex);
;;;  var
;;;    i, j, k: integer;
;;;  begin
;;;    {
;;;    матрицу веса дуги преобразуем в требуемый для алгоритма вид
;;;    - если i=j, то d[i, j]:=0
;;;    - если из i в j нет ребра, то d[i, j]:=INFINITY (бесконечности)
;;;    - иначе d[i, j] равно весу ребра из i в j
;;;    подготовим матрицу для восстановления пути p
;;;    }
;;;    d := v;
;;;    for i := 0 to pred(n) do
;;;      for j := 0 to pred(n) do
;;;      begin
;;;        if d[i, j] = 0 then
;;;          d[i, j] := INFINITY;
;;;        if i = j then
;;;          d[i, j] := 0;
;;;        p[i, j] := j;
;;;      end;
;;;    for k := 0 to pred(n) do
;;;    begin
;;;      for i := 0 to pred(n) do
;;;      begin
;;;        for j := 0 to pred(n) do
;;;        begin
;;;          if (d[i, k] <> INFINITY) and (d[k, j] <> INFINITY) then
;;;          begin
;;;            if (d[i, j] > d[i, k] + d[k, j]) then
;;;            begin
;;;              d[i, j] := d[i, k] + d[k, j];
;;;              p[i, j] := p[i, k];
;;;            end;
;;;          end;
;;;        end;
;;;      end;
;;;    end;
;;;  end;
;;;  procedure RestorePath(const D, P: TVertex; n: integer; A, B: integer);
;;;  var
;;;    k: integer;
;;;  begin
;;;    if A >= n then
;;;    begin
;;;      writeln('The vertex A is out of range.');
;;;      exit;
;;;    end;
;;;    if B >= n then
;;;    begin
;;;      writeln('The vertex B is out of range.');
;;;      exit;
;;;    end;
;;;    if D[A, B] = INFINITY then
;;;    begin
;;;      writeln('There is not a path from vertex ', A, ' to vertex ', B, '.');
;;;      exit;
;;;    end;
;;;    Write('The path from vertex ', A, ' to vertex ', B, ' is: <');
;;;    Write(A: 4);
;;;    k := A;
;;;    while k <> B do
;;;    begin
;;;      k := p[k, B];
;;;      Write(k: 4);
;;;    end;
;;;    writeln('>');
;;;  end;
;;;  procedure ShowMatrix(const M: TVertex; n: integer);
;;;  var
;;;    i, j: integer;
;;;  begin
;;;    for i := 0 to pred(n) do
;;;    begin
;;;      for j := 0 to pred(n) do
;;;      begin
;;;        if M[i, j] <> INFINITY then
;;;          Write(M[i, j]: 4)
;;;        else
;;;          Write('inf': 4);
;;;      end;
;;;      writeln;
;;;    end;
;;;  end;
;;;  procedure TestAlgoFW(const Vertex: TVertex; n: integer);
;;;  var
;;;    D, P: TVertex;
;;;  begin
;;;    FloydWarshall(Vertex, n, D, P);
;;;    writeln('Vertex matrix:');
;;;    ShowMatrix(Vertex, n);
;;;    writeln('Distance matrix:');
;;;    ShowMatrix(D, n);
;;;    writeln('Path matrix:');
;;;    ShowMatrix(P, n);
;;;    RestorePath(D, P, n, 3, 1);
;;;    RestorePath(D, P, n, 0, 3);
;;;    RestorePath(D, P, n, 3, 3);
;;;  end;
;;;begin
;;;  TestAlgoFW(Vertex1, n);
;;;  TestAlgoFW(Vertex2, n);
;;;end.

(setq INFINITY 1000000
      Vertex1  '((0 0 5 0 0)
		 (7 0 0 0 10)
		 (0 7 0 0 0)
		 (0 0 6 0 4)
		 (0 0 1 0 0)
		)
      Vertex2  '((0 3 2 0 0)
		 (0 0 0 4 0)
		 (0 0 0 6 0)
		 (0 0 0 0 2)
		 (0 0 0 0 0)
		)
)
(defun FloydWarshall (v n / d p i j k)
  (setq	d v
	p v
	i 1
  )
  (while (<= i n)
    (setq j 1)
    (while (<= j n)
      (if (and (/= i j) (= (Mget d i j) 0))
	(setq d (Mput d i j INFINITY))
      )
      (setq p (Mput p i j j))
      (setq j (1+ j))
    )
    (setq i (1+ i))
  )
  (setq k 1)
  (while (<= k n)
    (setq i 1)
    (while (<= i n)
      (setq j 1)
      (while (<= j n)
	(if (and (/= (Mget d i k) INFINITY) (/= (Mget d k j) INFINITY) (> (Mget d i j) (setq delem (+ (Mget d i k) (Mget d k j)))))
	  (setq d (Mput d i j delem)
		p (Mput p i j (Mget p i k)))
	)  
	(setq j (1+ j))
      )
      (setq i (1+ i))
    )  
    (setq k (1+ k))
  )
  (list d p)
)
(print (FloydWarshall Vertex1 5))


