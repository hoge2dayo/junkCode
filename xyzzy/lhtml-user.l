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
(provide :lhtml-user)

;;;; (1.5. defpackage)
;;; ※ :lhtml は use しない
(defpackage :lhtml-user (:use :lisp :editor))

;;;; 2. in-package
(in-package :lhtml-user)

;;;; 3. shadow

;;;; 4. export
(export '(*register-info*
		  get-info
		  clear-info
		  *request*
		  print-parsed-lhtml
		  def-lhtml
		  )
		)

;;;; 5. require

;;;; 6. use-package

;;;; 7. import


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; ■html ファイル出力を行うパッケージ
;;;; 　lhtml ファイルに記載した処理は、このパッケージ内で実行される。
;;;; 

;;; def-lhtml で定義した関数の名前
;;; lobj ファイルのロード・評価時にセット
(defparameter *register-info* nil)

;;; *register-info* を取得
(defun get-info ()
  *register-info*
  )

;;; *register-info* をクリア
(defun clear-info ()
  (setf *register-info* nil)
  )

;;; 関数呼出時に値を渡す為の変数
(defparameter *request* nil)

;;; *request* に p-list がセットされている前提で値を取り出す
;;; @param p プロパティ
;;; @param &optional val-if-nil 値が nil の場合に用いる値
(defmacro req (p &optional (val-if-nil nil)) `(or (getf *request* ,p) ,val-if-nil))

;;; html ファイルへ出力
;;; （リダイレクトするので、標準出力への出力でよい）
;;; @param e
(defmacro out (e) `(princ ,e))

;;; 関数 parse-lhtml で得た結果の１セクションを Lisp コードとして出力
;;; @param section セクション（ p-list のプロパティ）
;;; @param parsed-lhtml 関数 parse-lhtml で得た結果
;;; @return 
(defun print-parsed-lhtml-section (section parsed-lhtml)
  (let* ((lis (getf parsed-lhtml section))
		 )
	(dolist (e lis)
	  (cond
	   ;; 文字列
	   ((stringp e) (format t "(~s ~s)~%" 'out e))
	   ;; 文字列以外
	   (t (case (car e)
			(:sexp (format t "(~s ~a)~%" 'out (cadr e)) ;(cadr e) はＳ式を示した文字列
			 )
			(:block (format t "~a~%" (cadr e))
			 )
			(:comment
			 )
			(:require (format t "~s~%" `(require ,(cadr e)))
			 )
			(:use-package (format t "~s~%" `(use-package ,(cadr e)))
			 )
			(t
			 )
			) ;case
		  )
	   ) ;cond
	  ) ;dolist
	) ;let*
  )

;;; 各行に分けて記述した文字列を１つにする
;;; @param &rest lis-str 各要素は文字列
(defmacro string-lines (&rest lis-str)
  (format nil "~{~a~%~}" lis-str)
  )

;;; use-package 前に出力する内容
(defparameter *main-header-fmt-1*
  (string-lines ""
				"(require :lhtml-user)"
				"(in-package :lhtml-user)"
				) ;string-lines
  )

;;; メイン処理直前に出力する内容
(defparameter *main-header-fmt-2*
  (string-lines ""
				"(def-lhtml ~a (request)"
				"  (let* ((*request* (copy-list request)) ;値を変更する可能性に備えて複製"
				"         )"
				";;;;----> メイン処理"
				) ;string-lines
  )

;;; メイン処理直後に出力する内容
(defparameter *main-footer-fmt*
  (string-lines ""
				";;;;<---- メイン処理"
				"   ) ;let*"
				" ) ;def-lhtml"
				) ;string-lines
  )

;;; 関数 parse-lhtml で得た結果を Lisp コードとして出力
;;; 標準出力へ出力するので、必要に応じて事前にリダイレクトしておくこと
;;; @param parsed-lhtml 関数 parse-lhtml で得た結果
;;; @param func-name メイン関数の名前
;;; @return 
(defun print-parsed-lhtml (parsed-lhtml func-name)
  ;; カレントパッケージを変更して出力
  (let* ((*package* (in-package :lhtml-user))
		 )
	;; ---- require
	(print-parsed-lhtml-section :require parsed-lhtml)
	;; ---- in-package
	(format t *main-header-fmt-1* func-name)
	;; ---- use-package
	(print-parsed-lhtml-section :use-package parsed-lhtml)

	;; ---- メイン処理
	;; ヘッダ
	(format t *main-header-fmt-2* func-name)
	;; 処理
	(print-parsed-lhtml-section :main parsed-lhtml)
	;; フッタ
	(format t *main-footer-fmt*)
	) ;let*
  )

;;; 関数を定義
;;; lobj の式は 関数 load でロード・評価される為、直接戻り値を返せない。
;;; グローバル変数を使って、戻り値を間接的に戻り値を返す。
;;; @param name 関数名（シンボル）
;;; @param lambda-list ラムダリスト
;;; @param &body body
(defmacro def-lhtml (name lambda-list &body body)
  `(progn
	 (defun ,name ,lambda-list
	   ,@body
	   ) ;defun

	 ;; 関数名をセット
	 (push ',name *register-info*)

	 ',name
	) ;progn
  )

