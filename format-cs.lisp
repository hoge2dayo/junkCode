;;;; require - provide - defpackage - in-package - export

;;(require )

(provide :format-cs)

(defpackage :format-cs (:use :common-lisp))

(in-package :format-cs)

(export '(format-cs
          )
        ) ;export

;;;; 
;;;; ■format-cs マクロ
;;;; 
;;;; Ｃ＃の string.Format() 同様の書式指定で文字列を出力する。
;;;; 
;;;; （例）
;;;; (format-cs t "名前：{0} 年齢：{1}" "山田太郎" 20)
;;;; 


;;; フォームの戻り値（多値）[0]が真の場合に、値をセットする
;;; 条件付き multiple-value-setq
;;; 戻り値はフォームの戻り値（多値）[0]
;;; @param var-lis 多値[1]以降をセットする先の変数。変数にnil指定可
;;; @param &body forms 多値を返すフォーム
(defmacro multiple-value-set-if (var-lis &body forms)
  (let ((var-r (gensym))
        )
    `(let ((,var-r (multiple-value-list (progn ,@forms)))
           )
       (when (car ,var-r)
         (setf ,@(do ((rem-var var-lis (cdr rem-var))
                      (i 1 (1+ i))
                      (rpairs nil (if (car rem-var)
                                      (list* `(nth ,i ,var-r) (car rem-var) rpairs)
                                    rpairs
                                    ) ;if
                              )
                      )
                     ((null rem-var) (reverse rpairs))
                   ) ;do
               ) ;setf
         ) ;when
       
       (car ,var-r)
       ) ;let
    ) ;let
  )

;;; 文字列を１文字ずつ読み取るクロージャを作成
;;; @param str 読み取る文字列
;;; @param &optional index 最初の読取位置
;;; @return クロージャ
;;; 	クロージャの引数：
;;; 		なし   : １文字読み取り次の位置へ進める
;;; 		:peek  : １文字読み取るが位置は進めない
;;; 		:index : 読取位置を取得
;;; 		整数   : 読取位置のセット
(defun reader-string (str &optional (index 0))
  (let ((len (length str))
        (idx index)
        )
    #'(lambda (&optional mode-or-idx)
        (case mode-or-idx
          ((nil) (and (<= 0 idx)
                      (< idx len)
                      (prog1 (char str idx) (incf idx))
                      ) ;and
           )
          (:peek (and (<= 0 idx)
                      (< idx len)
                      (char str idx)
                      ) ;and
           )
          (:index idx
           )
          ;; 整数であること
          (t (when (numberp mode-or-idx)
               (setf idx mode-or-idx)
               ) ;when
             )
          ) ;case
        ) ;lambda
    ) ;let
  )

;;; 指定文字あるいは文字列と一致するか
;;; @param str
;;; @param val
;;; @param start
;;; @return
;;; 	[0]:一致／不一致
;;; 	[1]:一致直後の位置
;;; 	[2]:引数strで一致した部分
(defun read-const (str val start)
  (cond
   ((characterp val) (values (char= (char str start) val)
                             (1+ start)
                             val
                             ) ;values
    )
   ((stringp val) (values (string-equal str val :start1 start :end1 (+ start (length val)))
                          (+ start (length val))
                          (subseq str start (+ start (length val)))
                          ) ;values
    )
   ) ;cond
  )

;;; 文字列から整数を読み取る
;;; @param str
;;; @param start
;;; @return
;;; 	[0]:整数を読み取れたかどうか
;;; 	[1]:整数の読取終了位置+1
;;; 	[2]:整数
(defun read-int (str start)
  (let* ((len (length str))
         (start-n start) ;数字の開始位置
         end-n           ;数字の終了位置+1
         )
    ;; 符号
    (when (and (< start len) (find (char str start) "+-"))
      (incf start-n)
      ) ;when

    ;; 数字の終焉を検索
    (setf end-n (or (position-if-not #'(lambda (ch) (char<= #\0 ch #\9))
                                     str :start start-n
                                     ) ;position-if-not
                    len
                    ) ;or
          ) ;setf
    
    (when (< start-n end-n)
      (values t
              end-n
              (parse-integer str :start start :end end-n)
              )
      ) ;when
    ) ;let*
  )

;;; 中括弧または末尾まで文字列を読み込む。
;;; "{{"=>"{", "}}"=>"}"の変換を行う。
;;; @param str
;;; @param start
;;; @return
;;; 	[0]:t
;;; 	[1]:文字列を読み込んだ直後の位置
;;; 	[2]:読み込んだ文字列
;;; 	[3]:文字列読込終了の原因。括弧またはnil（末尾）
(defun read-cs-string-until-brace (str start)
  (let ((fnc-read (reader-string str start))
        end
        ch
        read-str
        )
    (setf read-str
          (with-output-to-string (os)
            (multiple-value-setq (end ch)
                (do ((last-ch nil ch)
                     (ch (funcall fnc-read) (funcall fnc-read))
                     )
                    ((null ch) (values (funcall fnc-read :index)
                                       (find last-ch "{}")
                                       ) ;values
                     )
                  (case ch
                    ((#\{ #\}) (when (eql last-ch ch)
                                 (princ ch os)
                                 (setf ch nil) ;last-ch クリアの為
                                 ) ;when
                     )
                    (t (when (find last-ch "{}")
                         (return (values (1- (funcall fnc-read :index)) ;文字の読込時に+1されているので-1
                                         last-ch
                                         ) ;values
                                 ) ;return
                         ) ;when
                       (princ ch os)
                       )
                    ) ;case
                  ) ;do
              ) ;multiple-value-setq
            ) ;with-output-to-string
          ) ;setf
    
    ;; index は括弧読み込み前の位置に修正
    (when ch (decf end))
    
    ;; 必ず真である点に注意
    (values t
            end
            read-str
            ch
            ) ;values
    ) ;let
  )

;;; 文字列の指定位置からＣ＃書式指定項目を読み取る
;;; @param str
;;; @param start
;;; @return
;;; 	[0]:指定位置にＣ＃書式指定項目があったかどうか
;;; 	[1]:Ｃ＃書式指定項目直後の位置
;;; 	[2]:Ｃ＃書式指定項目のindex
;;; 	[3]:Ｃ＃書式指定項目のalignment
;;; 	[4]:Ｃ＃書式指定項目のコロン以降の文字列（書式指定文字列）
(defun read-cs-item-format (str start)
  (let (idx
        (align nil)
        (fmtstr nil)
        ch
        )
    (and
     (multiple-value-set-if (start) (read-const str "{" start))
     
     ;; index
     (multiple-value-set-if (start idx) (read-int str start))
     
     ;; alignment
     (if (multiple-value-set-if (start) (read-const str "," start))
         (multiple-value-set-if (start align) (read-int str start))
       t
       ) ;if
     
     ;; format string
     (if (multiple-value-set-if (start) (read-const str ":" start))
         (and
          (multiple-value-set-if (start fmtstr ch) (read-cs-string-until-brace str start))
          (eql ch #\})
          (incf start)
          ) ;and
       (multiple-value-set-if (start) (read-const str "}" start))
       ) ;if
     
     (values t
             start
             idx
             align
             fmtstr
             ) ;values
     ) ;and
    )
  )

;;; Ｃ＃のstring.format()などで使用される複合書式指定文字列を読み込む
;;; @param str
;;; @return リスト。パラメータが埋め込まれる部分はリストが要素となる。
;;; 	要素となるリスト:(<index> <align> <書式指定文字列>)
(defun parse-cs-format-string (str)
  (let ((start 0)
        index
        align
        read-str
        ch
        (result nil)
        )
    (do ((len (length str))
         )
        ((<= len start) (reverse result))
      
      (multiple-value-set-if (start read-str ch) (read-cs-string-until-brace str start))
      (when (eql ch #\})
        (error "複合書式指定文字列が異常です。")
        ) ;when
      
      (when (< 0 (length read-str))
        (push read-str result)
        ) ;when
      
      ;; 書式指定項目
      (when (eql ch #\{)
        (unless (multiple-value-set-if (start index align read-str) (read-cs-item-format str start))
          (error "書式指定項目が異常です。")
          ) ;unless
        
        (push (list index align read-str) result)
        ) ;when
      
      ) ;do
    ) ;let
  )

;;; 書式に従って文字列を出力する。（マクロ）
;;; 複合書式指定文字列の変換のみ行い、残りの処理は format-csl で行う。
;;; @param stream ストリーム。t は標準出力。nil は文字列として出力。
;;; @param fmt-string Ｃ＃のstring.Format()などで使用される複合書式指定文字列
;;; @param &rest args 書式指定項目の部分に埋め込まれる文字列表現の基になるオブジェクト
(defmacro format-cs (stream fmt-string &rest args)
  (cond
   ;; fmt-string が文字列定数
   ((stringp fmt-string)
    `(format-csl ,stream ',(parse-cs-format-string fmt-string) ,@args)
    )
   (t
    `(format-csl ,stream (parse-cs-format-string ,fmt-string) ,@args)
    )
   ) ;cond
  )

;;; 書式に従って文字列を出力する。
;;; @param stream ストリーム。t は標準出力。nil は文字列として出力。
;;; @param fmt-list 複合書式指定文字列から関数 parse-cs-format-string で作成したリスト
;;; @param &rest args 書式指定項目の部分に埋め込まれる文字列表現の基になるオブジェクト
(defun format-csl (stream fmt-list &rest args)
  (let ((lis-str nil)
        )
    (setf lis-str
          (mapcar #'(lambda (e) (cond
                                 ((stringp e) e)
                                 ;; (<index> <align> <書式指定文字列>)
                                 ((consp e) (apply-to-string e args))
                                 ;;
                                 (t nil)
                                 ) ;cond
                      ) ;lambda
                  fmt-list
                  ) ;mapcar
          ) ;setf
    
    (format stream "~{~a~}" lis-str)
    ) ;let
  )


;;; 書式指定文字列に対応する（埋め込む）文字列を作成
;;; @param e 書式指定項目を示すリスト。(<index> <align> <書式指定文字列>)
;;; @param format-args
;;; @return 文字列
(defun apply-to-string (e format-args)
  (let* ((index (car e))
         (align (cadr e))
         (fmt (caddr e))
         (val (nth index format-args))
         (str (if val (to-string val fmt) ""))
         )
    (when (and align (< (length str) (abs align)))
      ;; align が設定されていて、文字列の長さが align の絶対値より短い場合
      ;; align が正の場合右揃え。負の場合左揃え。
      (setf str
            (replace (fill #+xyzzy(make-vector (abs align) :element-type 'character)
                           #-xyzzy(make-string (abs align))
                           #\Space
                           ) ;fill
                     str
                     :start1 (if (< 0 align)
                                 (- align (length str))
                               0
                               ) ;if
                     ) ;replace
            ) ;setf
      ) ;when
    
    str
    ) ;let*
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; 総称関数およびメソッド to-string

;;; オブジェクトの文字列表現を取得
;;; @param obj 文字列表現にするオブジェクト
;;; @param &optional fmt 書式指定文字列
;;; @return 文字列
(defgeneric to-string (obj &optional fmt)
  (:documentation "オブジェクトの文字列表現を取得。Ｃ＃のToString()に相当。")
  )

;;; オブジェクトの文字列表現を取得
;;; @param obj 文字列表現にするオブジェクト（ t ）
;;; @param &optional fmt 書式指定文字列
;;; @return 文字列
(defmethod to-string ((obj t) &optional (fmt nil))
  (format nil "~a" obj)
  )

;;; オブジェクトの文字列表現を取得
;;; @param obj 文字列表現にするオブジェクト（ integer ）
;;; @param &optional fmt 書式指定文字列
;;; @return 文字列
(defmethod to-string ((obj integer) &optional (fmt nil))
  (let ((ch (and (< 0 (length fmt)) (char fmt 0)))
        )
    (case ch
     ((#\C #\c) (format nil "\\~:d" obj))
     (t (format nil "~a" obj))
     ) ;case
    ) ;let
  )


