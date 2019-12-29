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
(provide :util-editor)

;;;; (1.5. defpackage)
(defpackage :util-editor (:use :lisp :editor)
  (:nicknames :uedit)
  )

;;;; 2. in-package
(in-package :util-editor)

;;;; 3. shadow

;;;; 4. export
(export '(define-key*            ;define-key の不定数登録版
;		  with-selected-buffer

		  goto-point-min         ;ファイル先頭へ移動し point-min を返す
		  goto-point-max         ;ファイル末尾へ移動し point-max を返す
		  goto-bol-point         ;行頭へ移動し point を返す
		  goto-eol-point         ;行末へ移動し point を返す
		  goto-line-number       ;指定行へ移動
		  line-number-point      ;指定行の行頭および行末の point を取得
		  selection-info         ;selection の情報
		  set-selection          ;selection を設定
		  restore-selection      ;処理後（解除された） selection を戻す
		  selection-or-region    ;selection または region 取得
		  find-word-near-point   ;カーソル近辺にある単語の位置を取得
		  buffer-line-string     ;バッファ１行分の文字列（末尾の改行は含まない）を取得
		  delete-until-eol       ;行末まで削除
		  do-buffer-line-string  ;バッファ各行でのループ
		  match-line-list        ;検索パターンが見つかる行のリストを返す
		  set-buffer*            ;バッファ名も指定可能な set-buffer
		  restore-buffer         ;指定処理終了後、選択バッファを元に戻す
		  create-working-buffer  ;作業用バッファを作成
		  with-working-buffer    ;一時的に作業用バッファを作成
		  )
		)

;;;; 5. require

;;;; 6. use-package

;;;; 7. import


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; マクロ
;;;; 

;;; define-key の不定数登録版
;;; @param keymap キーマップ。make-keymapなどで作成。
;;; @param &rest key-cmd-pairs キーとハンドラ関数を交互に指定
(defmacro define-key* (keymap &rest key-cmd-pairs)
  `(progn
	 ,@(do* ((remain key-cmd-pairs (cddr remain))
			 (result nil))
		   ((null (cdr remain)) (reverse result))
		 (push `(define-key ,keymap ,(car remain) ,(cadr remain)) result)
		 ) ;do*
	 ) ;progn
  )

#| output.l で使用されているが、output.l 自体廃止した。
   ちなみに with-set-buffer というマクロは xyzzy で使われている。
   想像する機能ではない。
;;; フォーム実行後、選択バッファを実行前のバッファに戻す
;;; @param &body body
(defmacro with-selected-buffer (&body body)
  (let* ((var-buf (gensym))
		 )
	`(let ((,var-buf (selected-buffer))
		   )
	   (unwind-protect
		   (progn
			 ,@body
			 )
		 ;; clean-up
		 (set-buffer ,var-buf)
		   ) ;unwind-protect
	   ) ;let
	) ;let*
  )
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; 
;;;; 

;;; ファイルの先頭（ point-min ）へ移動し、 point を返す
;;; @return point-min
(defun goto-point-min (&aux (pnt (point-min)))
  (goto-char pnt)
  pnt
  )

;;; ファイルの末尾（ point-max ）へ移動し、 point を返す
;;; @return point-max
(defun goto-point-max (&aux (pnt (point-max)))
  (goto-char pnt)
  pnt
  )

;;; 行頭へ移動し point を返す
;;; @return 行頭の point
(defun goto-bol-point ()
  (goto-bol)
  (point)
  )

;;; 行末へ移動し point を返す
;;; @return 行末の point
(defun goto-eol-point ()
  (goto-eol)
  (point)
  )

;;; 指定行へ移動
;;; @param line 移動先の行
;;; @return 指定行に移動できなかった場合は nil
(defun goto-line-number (line)
  (goto-line line)
  (eql line (current-line-number))
  )

;;; 指定行の行頭および行末の point を取得
;;; @param ln 行
;;; @return 行頭および行末の point のリスト
;;; 	指定行に移動できなかった場合は nil
(defun line-number-point (ln)
  (when (goto-line-number ln)
	(list (goto-bol-point) (goto-eol-point))
	) ;when
  )

;;; selection の情報
;;; @param &optional sort-pnt 真の場合、point をソートする。
;;; @return selection が存在しない場合 nil。
;;; 	selection が存在する場合、( <selection の種類> <point> <point> )
;;; 		<selection の種類> ：1=行選択 / 2=文字選択 / 矩形選択
(defun selection-info (&optional (sort-pnt nil))
  (let* ((type (get-selection-type))
		 (mrk (and type (selection-mark)))
		 (pnt (and type (selection-point)))
		 )
	(when type
	  (when (and sort-pnt (< pnt mrk)) (rotatef mrk pnt))
	  (list type mrk pnt)
	  ) ;when
	) ;let*
  )

;;; selection を設定する。
;;; @param type 1=行選択 / 2=文字選択 / 3=矩形選択
;;; 	※矩形選択は機能しない
;;; @param mrk
;;; @param pnt
;;; @return
(defun set-selection (type mrk pnt)
  (start-selection type t mrk)
  (goto-char pnt)
  )

;;; 処理後（解除された） selection を戻す。
;;; ※selection が矩形の場合にうまく動作しない
;;; @param &body body
(defmacro restore-selection (&body body)
  (let ((var-info (gensym))
		)
	`(let* ((,var-info (selection-info))
			)
	   (unwind-protect
		   (progn
			 ,@body
			 )
		 ;; クリーンアップ
		 (when ,var-info
		   (set-selection (car ,var-info) (cadr ,var-info) (caddr ,var-info))
		   ) ;when
		 ) ;unwind-protect
	   ) ;let*
	) ;let
  )

;;; selection または region を取得
;;; @param &optional sort-pnt 真の場合、point をソートする。
;;; @param &optional rect-selection selection の矩形選択を取得するか
;;; @return
;;; 	selection が存在する場合：(:selection <point> <point> <selection の種類>)
;;; 		<selection の種類> ：1=行選択 / 2=文字選択 / 矩形選択
;;; 	region が存在する場合：(:region <point> <point>)
;;; 	その他：nil
(defun selection-or-region (&optional (sort-pnt nil) (rect-selection nil))
  (or
   ;; selection
   (let* ((info (selection-info sort-pnt))
		  (type (car info))
		  )
	 (when (and type (find type (if rect-selection '(1 2 3) '(1 2))))
	   (list :selection (cadr info) (caddr info) (car info))
	   ) ;when
	 ) ;let*

   ;; region
   (let* ((mrk (mark))
		  (pnt (point))
		  )
	 (when mrk
	   (when (and sort-pnt (< pnt mrk)) (rotatef mrk pnt))
	   (list :region mrk pnt)
	   ) ;when
	 ) ;let*
   ) ;or
  )

;;; カーソル近辺の単語を取得
;;; @param &optional pnt 単語を探し始める位置。nil の場合、カーソル位置。
;;; @return 単語の開始位置と終了位置を要素とするリスト。
;;; 	単語が見つからなかった場合は nil
(defun find-word-near-point (&optional pnt)
  (let* (start
		 end
		 )
	(save-excursion
	  (when pnt
		(goto-char pnt)
		) ;when
	  (setf end (progn
				  (or (skip-syntax-spec-forward "w")
					  (skip-syntax-spec-backward "^w")
					  ) ;or
				  (point)
				  ) ;progn
			start (progn
					(skip-syntax-spec-backward "w")
					(point)
					) ;progn
			) ;setf
	  ) ;save-excursion
	(when (< start end)
	  (list start end)
	  ) ;when
	) ;let*
  )

;;; バッファ１行分の文字列（末尾の改行は含まない）を取得
;;; @param &optional line 移動先の行
;;; @return １行分の文字列（末尾の改行は含まない）
;;; 	移動先の行へ移動できなかった場合は nil
(defun buffer-line-string (&optional (line nil))
  (when
	;; line が nil の場合は何もしない（チェックなし）
	;; line が 非nil の場合、その行への移動を試みる。失敗すれば nil
   (or
	(null line)  ;nil の場合、行移動しない ⇒or を抜ける
	(goto-line-number line)
	) ;or

   (buffer-substring (goto-bol-point) (goto-eol-point))
   ) ;when
  )

;;; 行末まで削除
;;; @return 
(defun delete-until-eol ()
  (let* ((start (point))
		 (end (goto-eol-point))
		 )
	(delete-region start end)
	) ;let*
  )

;;; バッファ各行でのループ
;;; （例）(do-buffer-line-string (str-line 1 10) ...)
;;; @param var-str-line ループ変数。バッファの各行文字列に束縛される
;;; @param &optional ln-start 開始行。nilの場合現在行
;;; @param &optional ln-end 終了行。nilまたは省略の場合バッファ最終行
;;; @param &optional result-from ループ終了時のフォーム値
;;; @param &body body 各ループで実行するフォーム
(defmacro do-buffer-line-string ((var-str-line &optional ln-start ln-end result-form) &body body)
  (let ((var-ln (gensym))
		(var-ln-end (gensym))
		)
	`(do ((,var-ln (or ,ln-start (current-line-number)) (1+ ,var-ln))
		  (,var-ln-end (or ,ln-end (buffer-lines)))
		  (,var-str-line)
		  )
		 ((< ,var-ln-end ,var-ln) ,result-form)
	   (setf ,var-str-line (buffer-line-string ,var-ln))
	   ,@body
	   ) ;do
	) ;let
  )

;;; バッファから検索パターンが見つかった行のリストを取得
;;; @param ptn 検索パターン
;;; @param &key regexp 検索パターンが正規表現かどうか
;;; @return 検索パターンが見つかった行のリスト
(defun match-line-list (ptn &key (regexp nil))
  (goto-char (point-min))

  (do* ((hit (scan-buffer ptn :regexp regexp :tail t)
			 (scan-buffer ptn :regexp regexp :tail t)
			 )
		(lis-ln nil)
		)
	  ((null hit) (reverse lis-ln))
	(push (current-line-number) lis-ln)
	(goto-eol)
	(forward-char)
	) ;do*
  )

;;; バッファ名も指定可能な set-buffer
;;; @param buf-or-name バッファあるいはバッファ名
;;; @return 
(defun set-buffer* (buf-or-name)
  (if (bufferp buf-or-name)
	  (set-buffer buf-or-name)
	(set-buffer (find-buffer buf-or-name))
	) ;if
  )

;;; 指定処理終了後、選択バッファを元に戻す
;;; ※with-set-buffer というマクロを作りたかったが、
;;; 　同名のマクロ（動作は望みと異なる）が既にあるのでできない
;;; @param &body body
(defmacro restore-buffer (&body body)
  (let ((var-buf (gensym))
		)
	`(let* ((,var-buf (selected-buffer))
			)
	   (unwind-protect
		   (progn ,@body)
		 (set-buffer ,var-buf)
		 ) ;unwind-protect
	   ) ;let*
	) ;let
  )

;;; 作業用バッファを作成
;;; 既に指定したバッファ名と同名のバッファがあっても作成
;;; @param name バッファ名
;;; @param &optional path 作成したバッファに読み込むファイルのパス
;;; @return 作成したバッファ。ファイル読込失敗時は nil
(defun create-working-buffer (name &optional path)
  (let* ((buf (create-new-buffer name))
		 )
	(setup-temp-buffer buf)
	(set-buffer buf)

	(when path
	  ;; ファイル読込。読込失敗時はバッファ削除
	  (handler-case
		  (read-file path)
		(error (c)
		  (delete-buffer buf)
		  (error c)
		  )
		) ;handler-case
	  ) ;when

	buf
	) ;let*
  )

;;; 作業用バッファを作成し、処理後削除する
;;; @param var-buf 作成した作業用バッファに束縛される変数
;;; @param &rest args 作業用バッファ作成の為、関数 create-working-buffer に渡される引数
;;; @param &body body
(defmacro with-working-buffer ((var-buf &rest args) &body body)
  `(let* ((,var-buf (create-working-buffer ,@args))
		  )
	 (unwind-protect
		 (progn
		   ,@body
		   ) ;progn
	   (delete-buffer ,var-buf)
	   ) ;unwind-protect
	 ) ;let*
  )



#|
;;; カレントウィンドウの最上部に表示されてる行を指定行にする
;;; @param ln 
(defun set-window-start-line (ln)
  (when (<= 1 ln (buffer-lines))
	(scroll-window (- ln (get-window-start-line)))
	t
	) ;when
  )
|#

#|
;;; 指定ウィンドウの最終行が表示される様スクロールさせる
;;; （ログ出力などのウィンドウに利用）
;;; ※取得される ln-start と wnd-lines は画面表示と何故か異なる
;;; 　だが結果としてスクロール量は正しい。理由不明
;;; @param wnd
(defun scroll-window-to-final-line (wnd)
  (let* ((ln-start (get-window-start-line wnd))
		 (wnd-lines (window-lines wnd))
		 (buf-lines (buffer-lines (window-buffer wnd)))
		 ;; 何行スクロールさせれば最終行が下端に来るか
		 (line-offset (- buf-lines (+ ln-start wnd-lines -1)))
		 )
;(message "ln-start=~a, wnd-lines=~a, buf-lines=~a, line-offset=~a" ln-start wnd-lines buf-lines line-offset)
	;; バッファ最終行がウィンドウ下端より下ならスクロール
	(when (< 0 line-offset)
	  (with-selected-window
		(set-window wnd)
		(scroll-window line-offset)
		) ;with-selected-window
	  ) ;when
	) ;let*
  )
|#

#|
;;; 指定バッファを表示する全てのウィンドウに於いて、
;;; 最終行が表示される様スクロールさせる
;;; @param buf
(defun scroll-buffer-window-to-final-line (buf)
  (let* ((lis-wnd (get-buffer-windows buf))
		 )
	(dolist (wnd lis-wnd)
	  (scroll-window-to-final-line wnd)
	  ) ;dolist
	) ;let*
  )
|#

#|
;;; 全てのウィンドウを取得
;;; @param &optional wnd-start 最初に取得するウィンドウ
(defun get-all-windows (&optional wnd-start)
  (let* ((wnd (or wnd-start (selected-window)))
		 (lis-wnd nil)
		 )
	(dotimes (i (count-windows) lis-wnd)
	  (push wnd lis-wnd)
	  (setf wnd (next-window wnd))
	  ) ;dotimes
	) ;let*
  )
|#

#|
;;; 指定バッファのウィンドウを全て取得
;;; ※get-buffer-window で取得しようとしてもうまくいかない（バグ？）ので
;;; 　全てのウィンドウを取得してから選別
;;; @param buf
(defun get-buffer-windows (buf)
  (let* ((lis-wnd (get-all-windows)) ; 全てのウィンドウ
		 )
	;; 指定バッファと異なるウィンドウを削除
	(remove-if-not #'(lambda (w) (eql (window-buffer w) buf))
				   lis-wnd
				   ) ;remove-if-not
	) ;let*
  )
|#

#|
;;; ウィンドウを位置でソートする際に用いる関数
(defparameter *fnc-pos-priority*
  (list
   :left-top #'(lambda (c1 c2) (or (< (cadr c1) (cadr c2)) ;上辺
								   (and (eql (cadr c1) (cadr c2))
										(< (car c1) (car c2)) ;左辺
										) ;and
								   ) ;or
				 ) ;lambda
   :right-top #'(lambda (c1 c2) (or (< (cadr c1) (cadr c2)) ;上辺
									(and (eql (cadr c1) (cadr c2))
										 (< (caddr c2) (caddr c1)) ;右辺
										 ) ;and
									) ;or
				  ) ;lambda
   :left-bottom #'(lambda (c1 c2) (or (< (cadddr c2) (cadddr c1)) ;下辺
									  (and (eql (cadddr c1) (cadddr c2))
										   (< (car c1) (car c2)) ;左辺
										   ) ;and
									  ) ;or
					) ;lambda
   :right-bottom #'(lambda (c1 c2) (or (< (cadddr c2) (cadddr c1)) ;下辺
									   (and (eql (cadddr c1) (cadddr c2))
											(< (caddr c2) (caddr c1)) ;右辺
											) ;and
									   ) ;or
					 ) ;lambda
   ) ;list
  )
|#

#|
;;; ウィンドウを位置でソート
;;; @param lis-wnd ウィンドウのリスト
;;; @param direction :left-top/:left-bottom/:right-top/:right-bottom
(defun sort-window-by-position (lis-wnd direction)
  (let* ((fnc (getf *fnc-pos-priority* direction))
		 )
	(if fnc
		;; (ウィンドウ 座標） ⇒ ウィンドウ
		(mapcar #'car
				(sort
				 ;; ウィンドウ ⇒ (ウィンドウ 座標）
				 (mapcar #'(lambda (wnd) (list wnd (window-coordinate wnd)))
						 lis-wnd
						 ) ;mapcar
				 fnc
				 :key #'cadr
				 ) ;sort
				) ;mapcar
		;; else direction が不適切
		lis-wnd
	  ) ;if
	) ;let*
  )
|#




