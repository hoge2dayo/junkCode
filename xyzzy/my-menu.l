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
(provide :my-menu)

;;;; (1.5. defpackage)
(defpackage :my-menu (:use :lisp :editor))

;;;; 2. in-package
(in-package :my-menu)

;;;; 3. shadow

;;;; 4. export
(export '(
		  )
		) ;export

;;;; 5. require

;;;; 6. use-package

;;;; 7. import


;;;; 
;;;; ■メニューバーに［Lisp(L)］追加
;;;; 
;;;; ●項目と動作
;;;; 
;;;; ［バッファを評価(B)］
;;;; 　 バッファ全体を評価（eval-buffer と同じ）
;;;; 
;;;; ［最後のＳ式を評価(&L)］
;;;; 　 カーソル直前のＳ式を評価（eval-last-sexp と同じ）
;;;; 
;;;; ［リージョン又は選択範囲を評価(&R)］
;;;; 　 リージョンまたは SELECTION のＳ式を評価
;;;; 

;;; 現在のバッファに対して eval-buffer を実行
;;; @return 
(defun eval-selected-buffer ()
  (interactive)

  (eval-buffer (selected-buffer))
  )

;;; 選択（selection）を評価
;;; @return 評価した場合（selectionがあった場合）t
(defun eval-selection ()
  (interactive)

  (case (get-selection-type)
	((1 2)  ;行モード選択、文字モード選択
	 (setf mrk (selection-mark)
		   pnt (selection-point)
		   ) ;setf
	 (when (< pnt mrk) (rotatef mrk pnt))
	 (eval-region mrk pnt)
	 t
	 )
	(t nil)  ;選択なし、矩形選択
	) ;case
  )

;;; 選択（ selection ）またはリージョンを評価
;;; @return 
(defun eval-selection-or-region ()
  (interactive)
  (or (eval-selection)
	  (let ((mrk (mark))
			(pnt (point))
			)
		(when (< pnt mrk) (rotatef mrk pnt))
		(eval-region mrk pnt)
		) ;let
	  t
	  ) ;or
  )

;;; メニューのルート項目数を取得
;;; @param &optional menu
;;; @param &optional limit
;;; @return 
(defun get-menu-count (&optional (menu *app-menu*) (limit 20))
  (dotimes (i limit)
	(unless (get-menu menu i t)
	  (return i)
	  )
	) ;dotimes
  )


;;; 追加するメニュー
(defparameter *custom-menu-lisp*
  (define-popup-menu
	(:item nil "バッファを評価(&B)" 'eval-selected-buffer)
	:sep
	(:item nil "最後のＳ式を評価(&L)" 'eval-last-sexp)
	(:item nil "リージョン又は選択範囲を評価(&R)" 'eval-selection-or-region)
	)
  )

;;; ウィンドウメニューに追加
;;; 最後から２番目に追加（最後は［ヘルプ(?)］）
(add-hook '*init-app-menus-hook*
		  #'(lambda () (insert-popup-menu *app-menu* (1- (get-menu-count))
										  *custom-menu-lisp* "Lisp(&L)"
										  ) ;insert-popup-menu
			  ) ;lambda
		  )


