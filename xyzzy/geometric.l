;;;; require - provide - defpackage - in-package - export

;;(require )

(provide :geometric)

(defpackage :geometric
  (:nicknames :geo)
  (:use :lisp :editor)
  )

(in-package :geometric)

(export '(create-size size-w size-h
		  size-p
		  copy-size*
		  enlarge-size* enlarge-size

		  size-rect max-size

		  create-rect rect-x rect-y rect-w rect-h
		  rect-l rect-t ;マクロ
		  rect-r rect-b
		  rect-p
		  copy-rect*
		  rect-size or-rect and-rect cut-rect
		  )
		) ;export


;;;; 
;;;; ■サイズ
;;;; 

;;; サイズを示す構造体（幅・高さ）
(defstruct (size
			(:constructor make-size (&key w h))
			(:constructor create-size (w h))
			)
  (w 0)
  (h 0)
  )

;;; サイズの複製
;;; nil を扱える copy-size
;;; @param siz 複製元のサイズ
;;; @return 複製したサイズ
(defun copy-size* (siz)
  (if (size-p siz) (copy-size siz) siz)
  )

;;; サイズを指定量大きくする
;;; ※破壊的
;;; @param siz 変更対象のサイズ（※スロット値が変更される）
;;; @param incw 幅に加算する値
;;; @param inch 高さに加算する値
;;; @return 引数 siz
(defun enlarge-size* (siz incw inch)
  (cond
   ((null siz) (setf siz (make-size :w incw :h inch)))
   ((size-p siz)
	(incf (size-w siz) incw)
	(incf (size-h siz) inch)
	)
   ) ;cond
	siz
  )

;;; 指定量大きくしたサイズを取得
;;; ※非破壊的
;;; @param siz 基準とするサイズ（※複製される）
;;; @param incw 幅に加算する値
;;; @param inch 高さに加算する値
;;; @return 複製したサイズ
(defun enlarge-size (siz incw inch)
  (enlarge-size* (copy-size* siz) incw inch)
  )

;;; サイズから矩形を作成
;;; @param siz 矩形の幅及び高さとするサイズ
;;; @param &optional x 矩形の X 座標
;;; @param &optional y 矩形の Y 座標
;;; @return 矩形
(defun size-rect (siz &optional (x 0) (y 0))
  (make-rect :x x :y y :w (+ x (size-w siz)) :h (+ y (size-h siz)))
  )

;;; 指定したサイズから、最も大きい幅と最も大きい高さを用いたサイズ
;;; @param siz サイズ
;;; @param &rest sizes サイズ（複数）
;;; @return 最も大きい幅と最も大きい高さを用いたサイズ
(defun max-size (siz &rest sizes)
  (cond
   ;; p->e :サイズ１つ
   ((null sizes) (copy-size* siz))
   ;; p->e :引数が複数
   (t (reduce #'(lambda (siz0 siz1)
				  (when siz1
					(setf (size-w siz0) (max (size-w siz0) (size-w siz1))
						  (size-h siz0) (max (size-h siz0) (size-h siz1))
						  ) ;setf
					) ;when
				  siz0
				  ) ;lambda
			  sizes
			  :initial-value (or siz (create-size 0 0))
			  ) ;reduce
	  )
   ) ;cond
  )

;;;; 
;;;; ■矩形
;;;; 

;;; 矩形を示す構造体（位置＋幅・高さ）
(defstruct (rect
			(:constructor make-rect (&key x y w h))
			(:constructor create-rect (x y w h))
			)
  (x 0)
  (y 0)
  (w 0)
  (h 0)
  )

;;; 矩形の左端（rect-x の別名）
;;; @param rct 矩形
;;; @return 左端
(defmacro rect-l (rct) `(rect-x ,rct))

;;; 矩形の上端（rect-y の別名）
;;; @param rct 矩形
;;; @return 上端
(defmacro rect-t (rct) `(rect-y ,rct))

;;; 矩形の右端
;;; @param rct 矩形
;;; @return 右端
(defun rect-r (rct)
  (and rct (+ (rect-x rct) (rect-w rct)))
  )

;;; 矩形の下端
;;; @param rct 矩形
;;; @return 下端
(defun rect-b (rct)
  (and rct (+ (rect-y rct) (rect-h rct)))
  )

;;; 矩形の幅・高さ
;;; @param rct 矩形
;;; @return 二次元サイズ（構造体 size）
(defun rect-size (rct)
  (if (rect-p rct) (make-size :w (rect-w rct) :h (rect-h rct)) rct)
  )

;;; 矩形の複製
;;; nil を扱える copy-rect
;;; @param rct 複製元の矩形
(defun copy-rect* (rct)
  (if (rect-p rct) (copy-rect rct) rct)
  )

;;; 指定した全ての矩形を含む矩形
;;; @param rct 矩形
;;; @param &rest rcts 矩形（複数）
;;; @return 指定した全ての矩形を含む大きさの矩形
(defun or-rect (rct &rest rcts)
  (cond
   ;; p->e :矩形１つ
   ((null rcts) (copy-rect* rct))
   ;; p->e :矩形が複数
   (t (reduce #'union-rect rcts :initial-value rct))
  ) ;cond
)

;;; ２矩形を含む矩形
;;; ※nil は大きさ 0 の矩形と見なす
;;; @param rct0 矩形
;;; @param rct1 矩形
;;; @return 引数の矩形を含む大きさの矩形
(defun union-rect (rct0 rct1)
  (cond
   ;; p->e
   ((null rct0) (copy-rect* rct1))
   ;; p->e
   ((null rct1) (copy-rect* rct0))
   ;; p->e
   (t (let* ((x (min (rect-x rct0) (rect-x rct1)))
			 (y (min (rect-y rct0) (rect-y rct1)))
			 (r (max (rect-r rct0) (rect-r rct1)))
			 (b (max (rect-b rct0) (rect-b rct1)))
			 (w (- r x))
			 (h (- b y))
			)
		(make-rect :x x :y y :w w :h h) ;make-rect
	  ) ;let*
   )
  ) ;cond
)

;;; 指定した全ての矩形が重なった矩形
;;; @param rct 矩形
;;; @param &rest rcts 矩形（複数）
;;; @return 指定した全ての矩形が重なった部分の矩形
(defun and-rect (rct &rest rcts)
  (cond
   ;; p->e :矩形１つ
   ((null rcts) (copy-rect* rct))
   ;; p->e :矩形が複数
   (t (reduce #'intersection-rect rcts :initial-value rct))
  ) ;cond
)

;;; ２矩形の重なった部分となる矩形
;;; ※nil は大きさ 0 の矩形と見なす
;;; @param rct0 矩形
;;; @param rct1 矩形
;;; @return 引数の矩形が重なった部分の矩形
(defun intersection-rect (rct0 rct1)
  (cond
   ;; p->e
   ((or (null rct0) (null rct1)) nil)
   ;; p->e
   (t (let* ((x (max (rect-x rct0) (rect-x rct1)))
			 (y (max (rect-y rct0) (rect-y rct1)))
			 (r (min (rect-r rct0) (rect-r rct1)))
			 (b (min (rect-b rct0) (rect-b rct1)))
			 (w (- r x))
			 (h (- b y))
			)
		(if (and (< 0 w) (< 0 h))
			(make-rect :x x :y y :w w :h h) ;make-rect
			;; else
			nil
		)
	  ) ;let*
   )
  ) ;cond
)

;;; 矩形の端を指定の幅で分割する
;;; 第１引数は、:left, :top, :right, :bottom のいずれか
;;; （例）
;;; (cut-rect :bottom 10 (make-rect :x 10 :y 20 :w 30 :h 40))
;;; => #S(rect x 10 y 20 w 30 h 30)
;;;    #S(rect x 10 y 50 w 30 h 10)
;;; @param edge :left / :top / :right / :bottom
;;; @param len 引数 edge で示した辺から分割する長さ
;;; @param rct 分割する矩形
;;; @return 多値
;;; 	[0] :<分断された矩形>
;;; 	[1] :<分断した矩形>
(defun cut-rect (edge len rct)
  (cond
   ;; p->e
   ((null rct) (list nil nil))
   ;; p->e
   (t
	(let* ((x (rect-x rct))
		   (y (rect-y rct))
		   (w (rect-w rct))
		   (h (rect-h rct))
		  )
	  (case edge
		;; left
		(:left (values
				(and (< len w)
					(make-rect :x (+ x len) :y y :w (- w len) :h h)
				) ;and
				(make-rect :x x :y y :w len :h h)
			   ) ;values
		)
		;; top
		(:top (values
			   (and (< len h)
				   (make-rect :x x :y (+ y len) :w w :h (- h len))
			   ) ;and
			   (make-rect :x x :y y :w w :h len)
			  ) ;values
		)
		;; right
		(:right (values
				 (and (< len w)
					  (make-rect :x x :y y :w (- w len) :h h)
				 ) ;and
				 (make-rect :x (- (+ x w) len) :y y :w len :h h)
				) ;values
		)
		;; bottom
		(:bottom (values
				  (and (< len h)
					   (make-rect :x x :y y :w w :h (- h len))
				  ) ;and
				  (make-rect :x x :y (- (+ y h) len) :w w :h len)
				 ) ;values
		)
		;; otherwise
		(otherwise (values
					nil
					rct
					) ;values
				   )
		) ;case
	  ) ;let*
	)
   ) ;cond
  )


