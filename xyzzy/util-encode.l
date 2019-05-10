;;;; require - provide - defpackage - in-package - export

;;; 
;(require )
;;; 
(provide :util-encode)
;;; 
(defpackage :util-encode (:use :lisp :editor)
  (:nicknames :uenc)
  )
;;; 
(in-package :util-encode)

;;; 
(export '(utf8-encode
		  url-encode
		  )
		)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; 
;;;; 

;;; 数値を UTF-8 エンコードしたときの各バイトを返す
;;; @param n 数値。0～0x1fffff
;;; @return バイト単位のリスト
(defun utf8-encode (n)
  (cond
   ;; 
   ((< n 0) nil)
   ;; 0xxxxxxx
   ((< n #x80) (list n))
   ;; 110yyyyx 10xxxxxx
   ((< n #x800) (list (logior #b11000000 (ash n -6))
					  (logior #b10000000 (logand n #x3f))
					  ) ;list
	)
   ;; 1110yyyy 10yxxxxx 10xxxxxx
   ((< n #x10000) (list (logior #b11100000 (ash n -12))
						(logior #b10000000 (logand (ash n -6) #x3f))
						(logior #b10000000 (logand n #x3f))
						) ;list
	)
   ;; 11110yyy 10yyxxxx 10xxxxxx 10xxxxxx
   ((< n #x200000) (list (logior #b11110000 (ash n -18))
						 (logior #b10000000 (logand (ash n -12) #x3f))
						 (logior #b10000000 (logand (ash n -6) #x3f))
						 (logior #b10000000 (logand n #x3f))
					) ;list
	)
   ;; 
   (t nil)
   ) ;cond
  )

;;; 文字コードが URL エンコードの対象かどうか調べる。
;;; @param c 文字コードあるい文字。
;;; @return 
(defun url-encode-target-p (c)
  (when (characterp c)
	(setf c (char-code c))
	) ;when

   (or (<= 0 c #x1f)
	   (< #x7e c)
	   (and (find c #(#x20 #x22 #x23 #x3c #x3e #x60 #x7b #x7d)) t)
	   ) ;or
  )

;;; URL エンコードを文字列に適用する
;;; @param s
;;; @return URLエンコード後の文字列
(defun url-encode (s)
  (with-output-to-string (os)
	(map nil #'(lambda (ch)
				 (mapcar #'(lambda (n)
							 (if (url-encode-target-p n)
								 (format os "%~:@(~2,'0x~)" n)
							   (princ (unicode-char n) os)
							   ) ;if
							 ) ;lambda
						 (utf8-encode (char-unicode ch))
						 ) ;mapcar
				 ) ;lambda
		 s
		 ) ;map
	) ;with-output-to-string
  )
