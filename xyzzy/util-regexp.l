;;;; require - provide - defpackage - in-package - export

;;; 
;(require )
;;; 
(provide :util-regexp)
;;; 
(defpackage :util-regexp (:use :lisp :editor)
  (:nicknames :uregexp)
  )
;;; 
(in-package :util-regexp)

;;; 
(export '(*html-tag*
		  combine-regexp          ;正規表現を要素に含むリストを１つの正規表現文字列にする
		  replace-match-string    ;文字列中の正規表現に合致する箇所を置き換える
		  remove-match-string     ;文字列中の正規表現に合致する箇所を削除
		  )
		)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; 
;;;; 

;;; 正規表現を要素に含むリストを、１つの正規表現文字列にする
;;; @param e
;;; 	or で始まるリストは、\(?:～\)\|\(?:～\) となる。
;;; @return 正規表現文字列
(defun combine-regexp (e)
  (cond
   ((null e) "")
   ((stringp e) e)
   ((listp e) (case (car e)
				(or (format nil "\\(?:~{\\(?:~a\\)~^\\|~}\\)"
							(mapcar #'combine-regexp (cdr e))
							) ;format
				 )
				(t (format nil "\\(?:~{~a~}\\)" (mapcar #'combine-regexp e)))
				) ;case
	)
   ((keywordp e) (symbol-name e))
   ((symbolp e) (case e
				  ((+ * ?) (symbol-name e))
				  (t (symbol-value e))
				  ) ;case
	)
   (t "")
   ) ;cond
  )

;;; 空白（１文字）
(defparameter *ws* "[ \t\n]")
;;; タグ名／属性名
(defparameter *name* "[^ \t\n<>=\"']+")
;;; 値（その１）
(defparameter *value-1* "[^ \t\n<>=\"']*")
;;; 値（その２）
(defparameter *value-2* "\"[^\"]*\"")
;;; 属性名と値
(defparameter *name-value*
  (combine-regexp '(*name* ("=" (or *value-1* *value-2*)) ?))
  )
;;; HTML タグ
(defparameter *html-tag*
  (combine-regexp '("</?" *name*
					(*ws* + *name-value*) *
					*ws* * "/?>")
				  )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; 
;;;; 
;;;; 

;;; 文字列中から正規表現を検索し、合致した箇所を置き換える。
;;; （元の文字列は変わらない）
;;; @param rpl 置き換え後の文字列。合致部分を示す \1 - \9 を含めることが可能
;;; @param regexp 正規表現
;;; @param s 検索先文字列
;;; @param &key case-fold 真の場合、大文字小文字を区別しない
;;; 	正規表現がコンパイル済みの場合、無意味
;;; @return 
(defun replace-match-string (rpl regexp s &key (case-fold nil))
  (do* ((len (length s))
		(fnc-match (if case-fold #'string-matchp #'string-match))
		(lis nil)
		(start 0 (match-end 0))
		)
	  ((<= len start) (format nil "~{~a~}" (reverse lis)))
	(cond
	 ((funcall fnc-match regexp s start)
	  (push (subseq s start (match-beginning 0)) lis)
	  (push (string-replace-match s rpl) lis)
	  )
	 (t (push (subseq s start) lis)
		(return (format nil "~{~a~}" (reverse lis)))
	  )
	 ) ;cond
	) ;do*
  )

#|
;;; 文字列中から正規表現を検索し、合致した箇所を削除
;;; （元の文字列は変わらない）
;;; @param regexp 正規表現
;;; @param s 検索先文字列
;;; @param &key case-fold 真の場合、大文字小文字を区別しない
;;; 	正規表現がコンパイル済みの場合、無意味
;;; @return 
(defun remove-match-string (regexp s &key (case-fold nil))
  (do* ((len (length s))
		(fnc-match (if case-fold #'string-matchp #'string-match))
		(lis nil)
		(start 0 (match-end 0))
		)
	  ((<= len start) (format nil "~{~a~}" (reverse lis)))
	(cond
	 ((funcall fnc-match regexp s start) (push (subseq s start (match-beginning 0)) lis)
	  )
	 (t (push (subseq s start) lis)
		(return (format nil "~{~a~}" (reverse lis)))
		)
	 ) ;cond
	) ;do*
  )
|#

;;; 文字列中から正規表現を検索し、合致した箇所を削除
;;; （元の文字列は変わらない）
;;; @param regexp 正規表現
;;; @param s 検索先文字列
;;; @param &key case-fold 真の場合、大文字小文字を区別しない
;;; 	正規表現がコンパイル済みの場合、無意味
;;; @return 
(defun remove-match-string (regexp s &key (case-fold nil))
  (replace-match-string "" regexp s :case-fold case-fold)
  )

