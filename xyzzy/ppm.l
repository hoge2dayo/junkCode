;;;; 「 Common Lisp 第２版」（共立出版） p245 より
;;;; 1. provide
;;;; (1.5. defpackage)
;;;; 2. in-package
;;;; 3. shadow
;;;; 4. export
;;;; 5. require
;;;; 6. use-package
;;;; 7. import

;;;; 1. provide
(provide :ppm)

;;;; (1.5. defpackage)
(defpackage :ppm (:use :lisp :editor))

;;;; 2. in-package
(in-package :ppm)

;;;; 4. export
(export '(8color
		  clear-img
		  draw-rect
		  fill-rect
		  draw-line
		  *tmp-filepath*
		  save-img
		  open-imgfile-by-viewer
		  )
		)

;;;; 5. require

;;;; 6. use-package

;;;; 7. import


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; ■ppm ファイル（画像）用　描画・ファイル出力機能
;;;; 

;;; デジタル８色を保持した配列
(defvar *8color* (make-array 8 :initial-contents '(#x000000 #x0000ff #xff0000 #xff00ff
												   #x00ff00 #x00ffff #xffff00 #xffffff
												   )
							 ) ;make-array
  )

;;; デジタル８色の内１色を取得
;;; @param idx 任意の整数（８で割った剰余が用いられる）
;;; @return デジタル８色の内の１色
(defun 8color (idx)
  (elt *8color* (mod idx 8))
  )

;;; 画像の最大幅および最大高
(defvar *img-len* 1024)

;;; 画像の画素値を保持した配列
(defvar *img-data* (make-array (list *img-len* *img-len*)
							   :element-type '(unsigned-byte 24)
							   :initial-element 0
							   ) ;make-array
  )

;;; 画像全面クリア
;;; ※動作が重たいので、コンパイルすること
;;; @param &optional pix クリアに用いる画素値
;;; @return 
(defun clear-img (&optional (pix 0))
  (dotimes (i (* *img-len* *img-len*))
	(setf (row-major-aref *img-data* i) pix)
	) ;dotimes
  )

;;; 始点と終点（始点＋長さ）が画像内に収まるように調整
;;; 値によっては調整不可能な場合があり、その場合は引数の値をそのまま返す
;;; @param start 開始位置（ X 座標あるいは Y 座標）
;;; @param len 開始位置からの長さ（ X 方向あるいは Y 方向）
;;; @return 多値
;;; 	[0]: 調整後の start
;;; 	[1]: 調整後の len
(defun adjust-start-len (start len)
  (when (and (< start *img-len*)
			 (<= 0 (+ start len))
			 ) ;and
	(when (< start 0)
	  (incf len start)
	  (setf start 0)
	  ) ;when

	(when (<= *img-len* (+ start len))
	  (incf len (- *img-len* (+ start len)))
	  ) ;when
	) ;when

  (values start len)
  )

;;; 水平線を描画（開始位置から右方向へ）
;;; @param x 開始位置の X 座標
;;; @param y 開始位置の Y 座標
;;; @param width 水平線の長さ（※ 0 以上）
;;; @param pix 画素値
;;; @return 
(defun draw-horizontal (x y width pix)
  (when (<= 0 y (1- *img-len*))
	(multiple-value-setq (x width) (adjust-start-len x width))

	(when (and (<= 0 x (1- *img-len*))
			   (<= 0 (+ x width) (1- *img-len*))
			   ) ;and
	  (dotimes (i width)
		(setf (aref *img-data* y (+ x i)) pix)
		) ;dotimes
	  ) ;when
	) ;when
  )

;;; 垂直線を描画（開始位置から下方向へ）
;;; @param x 開始位置の X 座標
;;; @param y 開始位置の Y 座標
;;; @param height 垂直線の長さ（※ 0 以上）
;;; @param pix 画素値
;;; @return 
(defun draw-vertical (x y height pix)
  (when (<= 0 x (1- *img-len*))
	(multiple-value-setq (y height) (adjust-start-len y height))

	(when (and (<= 0 y (1- *img-len*))
			   (<= 0 (+ y height) (1- *img-len*))
			   ) ;and
	  (dotimes (i height)
		(setf (aref *img-data* (+ y i) x) pix)
		) ;dotimes
	  ) ;when
	) ;when
  )

;;; 矩形を描画
;;; @param x 矩形の左上位置の X 座標
;;; @param y 矩形の左上位置の Y 座標
;;; @param w 矩形の幅（※ 0 以上）
;;; @param h 矩形の高さ（※ 0 以上）
;;; @param pix 画素値
;;; @return 
(defun draw-rect (x y w h pix)
  (when (and (< 0 w) (< 0 h))
	(draw-horizontal x y          w pix)
	(draw-horizontal x (+ y h -1) w pix)
	(draw-vertical x          (1+ y) (- h 2) pix)
	(draw-vertical (+ x w -1) (1+ y) (- h 2) pix)
	) ;when
  )

;;; 塗りつぶされた矩形を描画
;;; @param x 矩形の左上位置の X 座標
;;; @param y 矩形の左上位置の Y 座標
;;; @param w 矩形の幅（※ 0 以上）
;;; @param h 矩形の高さ（※ 0 以上）
;;; @param pix 画素値
;;; @return 
(defun fill-rect (x y w h pix)
  (dotimes (i h)
	(draw-horizontal x (+ y i) w pix)
	) ;dotimes
  )

;;; カウンタ（汎変数）を減算し、 0 以下になれば指定の値を加算して処理を行う
;;; @param place-counter 加減算対象の汎変数
;;; @param dec 汎変数を減算する量
;;; @param inc 汎変数が 0 以下になった時に加算する量
;;; @param &body body-inc 汎変数が 0 以下になった時の処理
;;; @return 
(defmacro dec-inc (place-counter dec inc &body body-inc)
  `(when (<= (decf ,place-counter ,dec) 0)
	 (incf ,place-counter ,inc)
	 (progn
	   ,@body-inc
	   ) ;progn
	 )
  )

;;; 直線を描画
;;; @param x0 始点の X 座標
;;; @param y0 始点の Y 座標
;;; @param x1 終点の X 座標
;;; @param y1 終点の Y 座標
;;; @param pix 描画色
;;; @return
(defun draw-line (x0 y0 x1 y1 pix)
  (let* ((dx (signum (- x1 x0)))
		 (dy (signum (- y1 y0)))
		 (w (abs (- x1 x0)))
		 (h (abs (- y1 y0)))
		 (inc (1+ (max w h)))
		 (dec (1+ (min w h)))
		 (count inc)
		 (x x0)
		 (y y0)
		 )
	(cond
	 ((<= h w)
	  (dotimes (i (1+ w))
		(when (and (<= 0 x (1- *img-len*))
				   (<= 0 y (1- *img-len*))
				   ) ;and
		  (setf (aref *img-data* y x) pix)
		  ) ;when
		(incf x dx)
		(dec-inc count dec inc
		  (incf y dy)
		  ) ;dec-inc
		) ;dotimes
	  )
	 (t
	  (dotimes (i (1+ h))
		(when (and (<= 0 x (1- *img-len*))
				   (<= 0 y (1- *img-len*))
				   ) ;and
		  (setf (aref *img-data* y x) pix)
		  ) ;when
		(incf y dy)
		(dec-inc count dec inc
		  (incf x dx)
		  ) ;dec-inc
		) ;dotimes
	  )
	 ) ;cond
	) ;let*
  )

;;; テンポラリファイルを配置するディレクトリ
(defparameter *tmp-dir* (si:getenv "TMP"))
;;; テンポラリファイルのファイル名
(defparameter *tmp-file* "tmp.ppm")
;;; テンポラリファイルのパス
(defparameter *tmp-filepath* (merge-pathnames *tmp-file* *tmp-dir*))

;;; 画像ファイル（ppm フォーマット）を保存
;;; 配列 *img-data* の座標( 0, 0 ) から指定の幅・高さまでを保存
;;; @param width 保存する画像ファイルの幅（最大 *img-len* ）
;;; @param height 保存する画像ファイルの高さ（最大 *img-len* ）
;;; @param &optional filepath 出力する ppm ファイルのパス
;;; @return 
(defun save-img (width height &optional (filepath *tmp-filepath*))
  (with-open-file (os filepath
					  :direction :output
					  :if-exists :supersede
					  :if-does-not-exist :create
					  :encoding :binary
					  )
	(format os "P6~%~d ~d 255~%" width height)
	(let* (pix
		   )
	  (dotimes (y height)
		(dotimes (x width)
		  (setf pix (aref *img-data* y x))
		  (princ (code-char (logand (ash pix -16) #xff)) os) ;R
		  (princ (code-char (logand (ash pix -8) #xff)) os)  ;G
		  (princ (code-char (logand pix #xff)) os)           ;B
		  ) ;dotimes
		) ;dotimes
	  ) ;let*
	t
	) ;with-open-file
  )

;;; ppm ファイルをアプリで開く
;;; ※ppm ファイルを開くデフォルトアプリが設定済みであること
;;; @param &optional filepath 開く ppm ファイルのパス
;;; @return 
(defun open-imgfile-by-viewer (&optional (filepath *tmp-filepath*))
  (shell-execute filepath nil nil :verb :open)
  )


