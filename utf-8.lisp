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
(provide :utf-8)

;;;; (1.5. defpackage)
(defpackage :utf-8 (:use :common-lisp)
  (:nicknames :utf8)
  )

;;;; 2. in-package
(in-package :utf-8)

;;;; 3. shadow

;;;; 4. export
(export '(read-utf8-char    ; stream から読み取った UTF-8 符号を復号して得た文字
          read-utf8-line    ; stream から読み取った UTF-8 符号を復号して得た文字列一行
          
          write-utf8-char   ; 文字を UTF-8 符号（バイト列）で stream へ出力
          write-utf8-string ; 文字列を UTF-8 符号（バイト列）で stream へ出力
          write-utf8-line   ; 文字列と改行を UTF-8 符号（バイト列）で stream へ出力
          )
        )

;;;; 5. require

;;;; 6. use-package

;;;; 7. import


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; 
;;;; ■ UTF-8
;;;; 
;;;; UTF-8 符号での文字／文字列の入出力。
;;;; ライブラリ usocket では external format に未対応なので、
;;;; 直接日本語の文字を送受信できない。
;;;; そのためバイナリ（ (unsigned-byte 8) ）のストリームで
;;;; UTF-8 の文字列を送受信する関数を作成。
;;;; 

;;;; 
;;;; □ 実装差の吸収部分
;;;; 

#+xyzzy
;;; xyzzy 用 read-byte
;;; @param stream 
;;; @return 数値（バイト）
(defun read-byte (stream &optional (eof-error-p t) eof-value)
  (handler-case (let ((ch (read-char stream t))
                      )
                  (char-code ch)
                  ) ;let
	(error (e) (if eof-error-p (error e) eof-value))
	) ;handler-case
  )

#+xyzzy
;;; xyzzy 用 write-byte
;;; @param byte 数値（バイト）
;;; @param stream 
;;; @return 
(defun write-byte (byte stream)
  (write-char (code-char byte) stream)
  byte
  )

;;; UNICODE に対応する文字
;;; ※Lisp 実装差を吸収する為の変換
;;; @param UNICODE
;;; @return 文字
(defun unicode-to-char (code)
  #+xyzzy(unicode-char code)
  #-xyzzy(code-char code) ;LispWorks
  )

;;; 文字に対応する UNICODE
;;; ※Lisp 実装差を吸収する為の変換
;;; @param ch 文字
;;; @return UNICODE
(defun char-to-unicode (ch)
  #+xyzzy(char-unicode ch)
  #-xyzzy(char-code ch) ;LispWorks
  )


;;;; 
;;;; □ read （入力）
;;;; 

;;; UTF-8 として不適切なコードを取得した場合に使用する文字
(defparameter *alternative-char* #\?)

;;; stream から読み取った UTF-8 符号を復号して得た文字
;;; @param input-stream 
;;; @param &optional eof-error-p 
;;; @param &optional eof-value 
;;; @return UNICODE。
;;; 	読取時に EOF に達した場合： eof-value
(defun read-utf8-char (input-stream &optional (eof-error-p t) eof-value)
  (flet ((read-trail (lead bits-lead cnt-trail)
           (let* ((code (ldb (byte bits-lead 0) lead))
                  trail
                  )
             (dotimes (i cnt-trail)
               (setf trail (read-byte input-stream t))
               ;; trail バイトとして不適切
               (unless (= (ldb (byte 2 6) trail) 2) (return *alternative-char*))
               
               (setf code (logior (ash code 6) (ldb (byte 6 0) trail)))
               ) ;dotimes
             
             (unicode-to-char code)
             ) ;let*
           )
         )
    
    (handler-case (let* ((lead (read-byte input-stream t))
                         )
                    (cond
                     ;; 0xxxxxxx
                     ((<= lead #x7f) (unicode-to-char lead))
                     ;; 10xxxxxx --- trail バイトであり復号不能
                     ((<= lead #xbf) *alternative-char*)
                     ;; 110yyyyx 10xxxxxx
                     ((<= lead #xdf) (read-trail lead 5 1))
                     ;; 1110yyyy 10yxxxxx 10xxxxxx
                     ((<= lead #xef) (read-trail lead 4 2))
                     ;; 11110yyy 10yyxxxx 10xxxxxx 10xxxxxx
                     ((<= lead #xf7) (read-trail lead 3 3))
                     ;; 復号不能
                     (t *alternative-char*)
                     ) ;cond
                    ) ;let*
      (error (e) (if eof-error-p (error e) eof-value))
      ) ;handler-case
    ) ;flet
  )

;;; stream から読み取った UTF-8 符号を復号して得た文字列一行
;;; @param input-stream 
;;; @param &optional eof-error-p 
;;; @param &optional eof-value 
;;; @return 
(defun read-utf8-line (input-stream &optional (eof-error-p t) eof-value)
  (let* ((missing-newline-p nil)
         ch1
         )
    (values (block nil
              (with-output-to-string (os)
                ;; 最初の一文字目。 EOF に達したときの処理がこの時だけ異なる。
                (handler-case (setf ch1 (read-utf8-char input-stream t))
                  (error (e) (setf missing-newline-p t)
                    (if eof-error-p (error e) (return eof-value))
                    )
                  ) ;handler-case
                
                ;; 二文字目以降。 EOF に達してもエラーにならない。
                (handler-case (do ((ch2 ch1 (read-utf8-char input-stream t))
                                   )
                                  ((eql ch2 #\Newline))
                                (princ ch2 os)
                                ) ;do
                  (error (e) (setf missing-newline-p t))
                  ) ;handler-case
                ) ;with-output-to-string
              ) ;block
            missing-newline-p    ;（ values の引数）
            ) ;values
    ) ;let*
  )


;;;; 
;;;; □ write （出力）
;;;; 

;;; 文字を UTF-8 符号（バイト列）で stream へ出力
;;; @param ch 文字
;;; @param stream 出力先。（※数値を出力するので、 &optional 省略での標準出力は無意味）
;;; @return 出力したバイト数。
(defun write-utf8-char (ch stream)
  (let* ((code (char-to-unicode ch))
         )
    (cond
     ;; 0xxxxxxx
     ((<= code #x7f) (write-byte code stream)
      1)
     ;; 110yyyyx 10xxxxxx
     ((<= code #x7ff)
      (write-byte (logior #b11000000 (ldb (byte 5 6) code)) stream)
      (write-byte (logior #b10000000 (ldb (byte 6 0) code)) stream)
      2)
     ;; 1110yyyy 10yxxxxx 10xxxxxx
     ((<= code #xffff)
      (write-byte (logior #b11100000 (ldb (byte 4 12) code)) stream)
      (write-byte (logior #b10000000 (ldb (byte 6  6) code)) stream)
      (write-byte (logior #b10000000 (ldb (byte 6  0) code)) stream)
      3)
     ;; 11110yyy 10yyxxxx 10xxxxxx 10xxxxxx
     ((<= code #x1fffff)
      (write-byte (logior #b11110000 (ldb (byte 3 18) code)) stream)
      (write-byte (logior #b10000000 (ldb (byte 6 12) code)) stream)
      (write-byte (logior #b10000000 (ldb (byte 6  6) code)) stream)
      (write-byte (logior #b10000000 (ldb (byte 6  0) code)) stream)
      4)
     ;;
     (t 0)
     ) ;cond
    ) ;let*
  )

;;; 文字列を UTF-8 符号（バイト列）で stream へ出力
;;; @param output-stream 出力先。（※数値を出力するので、 &optional 省略での標準出力は無意味）
;;; @param &key start 
;;; @param &key end 
;;; @return 
(defun write-utf8-string (string output-stream &key (start 0) (end nil))
  (do* ((end (or end (length string)))
        (idx start (1+ idx))
        )
       ((<= end idx) string)
    (write-utf8-char (char string idx) output-stream)
    ) ;do*
  )

;;; 文字列と改行を UTF-8 符号（バイト列）で stream へ出力
;;; @param output-stream 出力先。（※数値を出力するので、 &optional 省略での標準出力は無意味）
;;; @param &key start 
;;; @param &key end 
;;; @return 
(defun write-utf8-line (string output-stream &key (start 0) (end nil))
  (write-utf8-string string output-stream :start start :end end)
  (write-utf8-char #\Newline output-stream)
  string
  )


