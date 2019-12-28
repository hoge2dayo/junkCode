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
(provide :format-cs)

;;;; (1.5. defpackage)
(defpackage :format-cs (:use :common-lisp))

;;;; 2. in-package
(in-package :format-cs)

;;;; 3. shadow

;;;; 4. export
(export '(format-cs
          )
        ) ;export

;;;; 5. require

;;;; 6. use-package

;;;; 7. import


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; 
;;;; ■format-cs マクロ
;;;; 
;;;; Ｃ＃の string.Format() 同様の書式指定で文字列を出力する。
;;;; 
;;;; （例）
;;;; (format-cs t "名前：{0} 年齢：{1}" "山田太郎" 20)
;;;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; 複合書式指定文字列（例．"名前：{0} 年齢：{1}" "山田太郎"）解析
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

;;; 指定文字あるいは文字列と一致するか
;;; @param str
;;; @param val
;;; @param start
;;; @return
;;; 	[0]:一致／不一致
;;; 	[1]:一致直後の位置
;;; 	[2]:引数strで一致した部分
(defun read-const (str val start)
  (let* ((end (+ start (length val)))
         )
    (when (<= 0 start end (length str))
      (values (string-equal str val :start1 start :end1 end)
              end
              (subseq str start end)
              ) ;values
      ) ;when
    ) ;let*
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
    (setf end-n (or (position-if-not #'digit-char-p str :start start-n)
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

;;; string-output-stream にアクセスするクロージャを作成
;;; @return クロージャ
;;; 	クロージャの引数：
;;; 		あり   : 内部の stream へ出力
;;; 		なし   : stream に蓄積した内容を文字列で返す
(defun writer-string ()
  (let ((stream (make-string-output-stream))
        )
    #'(lambda (&optional (val nil val-p))
        (if val-p
            (princ val stream)
          (get-output-stream-string stream)
          ) ;if
        ) ;lambda
    )
  )

;;;
;;; @param chs 探す文字が含まれる文字列
;;; @param str 検索範囲
;;; @param start 検索開始位置
;;; @return 
(defun position-some-char (chs str start)
  (when (<= 0 start (1- (length str)))
    (position-if #'(lambda (ch) (find ch chs)) str :start start)
    ) ;when
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
(defun read-string-until-brace (str start)
  (let ((fnc-write (writer-string))
        end
        read-str
        ch
        )
    (do* ((start-s start (+ end-s 2)) ;ループ継続＝同じ文字が２連続だった。なので+2
          (end-s)
          )
         (nil)
      ;; { または } までを出力
      (setf end-s (position-some-char "{}" str start-s))
      (funcall fnc-write (subseq str start-s end-s))
      
      (when (or (null end-s)                                    ;文字列末尾に到達
                (<= (1- (length str)) end-s)                    ;最後の文字（２連続はあり得ない）
                (char/= (char str end-s) (char str (1+ end-s))) ;２連続でない
                ) ;or
        (return (values t
                        (or end-s (length str))
                        (funcall fnc-write)
                        (and end-s (char str end-s))
                        ) ;values
                ) ;return
        ) ;when
      
      ;; { または } の２連続だったので１文字出力
      (funcall fnc-write (char str end-s))
      ) ;do*
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
          (multiple-value-set-if (start fmtstr ch) (read-string-until-brace str start))
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

;;; 複合書式指定文字列を解析した結果
(defstruct parsed-cs-format
  (lis nil)
  )

;;; Ｃ＃のstring.format()などで使用される複合書式指定文字列を読み込む
;;; @param str
;;; @return 構造体 parsed-cs-format。メンバに解析結果のリストがセットされている。
;;; 	解析結果のリスト：
;;; 		パラメータが埋め込まれる部分はリストが要素となる。
;;; 		要素となるリスト:(<index> <align> <書式指定文字列>)
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
        ((<= len start))
      
      (multiple-value-set-if (start read-str ch) (read-string-until-brace str start))
      (when (eql ch #\})
        (error "複合書式指定文字列が異常です。~%~a~%~@,,va~%" str  start #\^)
        ) ;when
      
      (when (< 0 (length read-str))
        (push read-str result)
        ) ;when
      
      ;; 書式指定項目
      (when (eql ch #\{)
        (unless (multiple-value-set-if (start index align read-str) (read-cs-item-format str start))
          (error "書式指定項目が異常です。~%~a~%~@,,va~%" str start #\^)
          ) ;unless
        
        (push (list index align read-str) result)
        ) ;when
      ) ;do

    ;; 構造体に結果をセット
    (make-parsed-cs-format :lis (reverse result))
    ) ;let
  )

;;; 書式に従って文字列を出力する。（マクロ）
;;; 複合書式指定文字列の変換のみ行い、残りの処理は format-csl で行う。
;;; @param stream ストリーム。t は標準出力。nil は文字列として出力。
;;; @param fmt Ｃ＃のstring.Format()などで使用される複合書式指定文字列
;;; 	あるいは関数 parse-cs-format-string で取得した値
;;; @param &rest args 書式指定項目の部分に埋め込まれる文字列表現の基になるオブジェクト
(defmacro format-cs (stream fmt &rest args)
  (cond
   ;; fmt が文字列定数
   ((stringp fmt)
    `(format-csl ,stream ',(parse-cs-format-string fmt) ,@args)
    )
   (t
    `(format-csl ,stream ,fmt ,@args)
    )
   ) ;cond
  )

;;; 書式に従って文字列を出力する。
;;; @param stream ストリーム。t は標準出力。nil は文字列として出力。
;;; @param fmt 複合書式指定文字列、
;;; 	あるいは関数 parse-cs-format-string で取得した値
;;; @param &rest args 書式指定項目の部分に埋め込まれる文字列表現の基になるオブジェクト
(defun format-csl (stream fmt &rest args)
  (let (fmt-lis
        (lis-str nil)
        )
    (cond
     ((stringp fmt) (setf fmt (parse-cs-format-string fmt))
      )
     ((parsed-cs-format-p fmt)
      )
     (t (error "書式として解釈できません。")
        )
     ) ;cond
    
    (unless (setf fmt-lis (parsed-cs-format-lis fmt))
      (error "書式データが異常です。")
      ) ;unless
    
    (setf lis-str
          (mapcar #'(lambda (e) (cond
                                 ((stringp e) e)
                                 ;; (<index> <align> <書式指定文字列>)
                                 ((consp e) (apply-to-string e args))
                                 ;;
                                 (t nil)
                                 ) ;cond
                      ) ;lambda
                  fmt-lis
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
            (replace (fill (make-string (abs align)) #\Space) str
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; 数値出力用関数
;;;; 

;;; 書式指定文字列の１文字目と続く数値（ある場合）を取得（多値）
;;; @param fmt
;;; @return
;;; 	[0]:１文字目
;;; 	[1]:数値
(defun fmt-ch-scale (fmt)
  (let ((ch nil)
        (scale nil)
        )
    (setf ch (and (< 0 (length fmt)) (char fmt 0)))
    (when (and (<= 2 (length fmt)) (char<= #\0 (char fmt 1) #\9))
      (setf scale (parse-integer fmt :start 1 :junk-allowed t))
      ) ;when
    
    (values ch scale)
    ) ;let
  )

;;; 指定桁の下の桁を四捨五入し、指定桁未満を切り捨てる。
;;; @param n 数。有理数推奨。浮動小数点数は非推奨。
;;; 	有理数を使用すると、例えば (truncate 1.3 1/10) => 12 0.09999993 となり、
;;; 	望む結果とならない
;;; @param col-dec 小数点第何位までを残すか。四捨五入はこの下の桁。
;;; @return 
(defun round-off (n col-dec)
  (let* ((unit (expt 1/10 col-dec))
         )
    (* unit (truncate (+ n (* 1/2 unit)) unit))
    ) ;let*
  )

;;; 固定小数点数文字列を作成
;;; @param n 数
;;; @param comma 整数部をカンマ区切りにするかどうか
;;; @param cols-dec 小数点以下の桁数。非 nil の場合、その下の桁で四捨五入する
;;; @param trunc 小数点以下が0になったら以降の桁を省略するか
;;; @return 
(defun float-string-cs (n comma cols-dec trunc)
  (let* ((r (abs (rationalize n))) ;有理数に変換
         int ;整数部
         dec ;小数部
         (cols-dec-max 20) ;小数部の最大桁数（循環小数時の対策）
         )
    ;; 小数部を四捨五入
    (when cols-dec
      (setf r (round-off r cols-dec))
      ) ;when
    
    ;; 整数部と小数部を取得
    (multiple-value-setq (int dec) (truncate r))
    
    ;; 出力
    (with-output-to-string (os)
      ;; 符号 ※四捨五入の結果で負が 0 になってる可能性も確認
      (when (and (minusp n) (not (eql r 0)))
        (princ #\- os)
        ) ;when
      ;; 整数部
      (format os (if comma "~:d" "~d") int)
      ;; 小数点
      (unless (or (eql cols-dec 0)
                  (and trunc (eql dec 0))
                  ) ;or
        (princ #\. os)
        ) ;unless
      ;; 小数部
      (dotimes (i (or cols-dec cols-dec-max))
        (when (and trunc (zerop dec))
          (return)
          ) ;when
        
        (setf dec (* 10 dec))
        (multiple-value-setq (int dec) (truncate dec))
        (princ int os)
        ) ;dotimes
      ) ;with-output-to-string
    ) ;let*
  )

;;; 指数形式文字列を作成
;;; @param cols-dec 小数点以下の桁数。非 nil の場合、その下の桁で四捨五入する
;;; @param trunc 小数点以下が0になったら以降の桁を省略するか
;;; @param ch-exp 指数を表す文字。一般に 'E' または 'e'
;;; @param cols-exp 指数の桁数
;;; @return 
(defun exponent-string-cs (n cols-dec trunc ch-exp cols-exp)
  (let* ((r (abs (rationalize n))) ;有理数に変換
         exp
         int
         dec
         )
    ;; 指数を求める
    (setf exp (if (not (eql r 0))
                  (floor (log (abs r) 10))
                0
                ) ;if
          r (* r (expt 1/10 exp))
          ) ;setf
    
    ;; 小数部を四捨五入
    (when cols-dec
      (setf r (round-off r cols-dec))
      
      ;; 四捨五入で整数部が２桁になった場合の調整
      (when (<= 10 r)
        (setf r (* r 1/10))
        (incf exp)
        ) ;when
      ) ;when
    
    ;; 出力
    (with-output-to-string (os)
      ;; 符号
      (when (and (minusp n) (not (eql r 0)))
        (princ #\- os)
        ) ;when
      ;; 実数部
      (princ (float-string-cs r nil cols-dec trunc) os)
      ;; 実数部
      (princ ch-exp os)
      ;; 指数
      (format os "~a~v,'0d" (if (<= 0 exp) "+" "-") cols-exp (abs exp))
      ) ;with-output-to-string
    ) ;let*
  )

;;; 整数を２の補数で表現したbitパターンをマスクする最小の値
;;; @param n
;;; @param &optional bits-min 値のbit幅が小さくても最小限確保するbit数
;;; @return 引数の値をマスクする最小の値（8bit単位）。
;;; 	#b1...1 LSB
(defun bit-mask-min (n &optional (bits-min 32))
  (let ((bits (max bits-min (1+ (integer-length n))))
        )
    ;; 8bit単位で切り上げ
    (setf bits (* 8 (ceiling bits 8)))
    (1- (expt 2 bits))
    ) ;let
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; 総称関数およびメソッド to-string
;;;; 

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
;;; @param obj 文字列表現にするオブジェクト（ real ）
;;; @param &optional fmt 書式指定文字列
;;; @return 文字列
(defmethod to-string ((n real) &optional (fmt nil))
  (let (ch sc)
    (multiple-value-setq (ch sc) (fmt-ch-scale fmt))

    (case ch
      ((#\C #\c)
       ;; 通貨	（例）-\12,345.68  ※符号が通貨記号より前なので注意
       ;; 整数部はカンマ区切り
       ;; 精度指定子：小数点以下の桁数。デフォルトは0
       (format nil "~:[~;-~]\\~a"
               (< n 0)
               (float-string-cs (abs n) t (or sc 0) nil)
               ) ;format
       )
      ((#\E #\e)
       ;; 指数	（例）1.234568E+004
       ;; 指数部は３桁
       ;; 精度指定子：小数点以下の桁数。デフォルト6。0の時、小数点は出力しない
       (exponent-string-cs n (or sc 6) nil ch 3)
       )
      ((#\F #\f)
       ;; 固定小数点表記
       ;; 精度指定子：小数点以下の桁数。デフォルトは2。0の時、小数点は出力しない
       (float-string-cs n nil (or sc 2) nil)
       )
      ((#\G #\g)
       ;; 精度指定なしあるいは 0 の場合の精度を決定
       (when (zerop (or sc 0))
         (setf sc (cond
                   ((or (typep n 'long-float) (typep n 'double-float)) 15)
                   (t 7) ;short-float または single-float
                   ) ;cond
               ) ;setf
         ) ;when
       
       (cond
        ;; 整数の場合はＣ＃の decimal と見なして固定小数点表記とする
        ((typep n 'integer)
         (format nil "~d" n)
         )
        ;; 指数が-5以下の場合は指数表記。この場合四捨五入はしない
        ((< (abs n) (expt 10 -4))
         (exponent-string-cs n nil t (if (upper-case-p ch) #\E #\e) 2)
         )
        ;; 指数が指定精度以上の場合は指数表記
        ((<= (expt 10 sc) (abs n))
         ;; 指定された精度は整数部も含む桁数なので、-1 する
         (exponent-string-cs n (1- sc) t (if (upper-case-p ch) #\E #\e) 2)
         )
        ;; 指数が-5より大きく指定精度未満
        (t
         ;; G/g では精度指定子は整数部と小数部の両方桁数合計
         (let ((isc (1+ (floor (log (abs n) 10)))) ;整数部の桁数
               )
           ;; 固定小数点表記
           (float-string-cs n nil (- sc isc) t)
           ) ;let
         )
        ) ;cond
       )
      ((#\N #\n)
       (float-string-cs n t (or sc 2) nil)
       )
      ((#\P #\p)
       (format nil "~a%" (float-string-cs (* n 100) t (or sc 2) nil))
       )
      (t
       (error "形式指定子が無効です。")
       )
      ) ;case
    ) ;let
  )

;;; オブジェクトの文字列表現を取得
;;; @param i 文字列表現にするオブジェクト（ integer ）
;;; @param &optional fmt 書式指定文字列
;;; @return 文字列
(defmethod to-string ((i integer) &optional (fmt nil))
  (let (ch sc)
    (multiple-value-setq (ch sc) (fmt-ch-scale fmt))

    (case ch
      ((#\D #\d)
       ;; 整数	（例）00012345
       ;; 精度指定子：数字部分の桁数。桁数が指定以上になるよう0を出力。桁数に符号は含まない。
       (format nil "~a~v,'0d" (if (< i 0) "-" "") sc (abs i))
       )
      ((#\X #\x)
       (when (< i 0)
         ;; 負の場合、２の補数表現をした正の値にする
         (setf i (logand (bit-mask-min i) i))
         )
       ;; 大文字小文字を書式指定文字列と合わせる為のフォーマット切替
       (format nil (if (upper-case-p ch) "~:@(~v,'0x~)" "~(~v,'0x~)")
               sc i
               ) ;format
       )
      ((nil)
       (call-next-method i "G")
       )
      (t
       (call-next-method)
       )
      ) ;case
    ) ;let
  )

;;; オブジェクトの文字列表現を取得
;;; @param f 文字列表現にするオブジェクト（ float ）
;;; @param &optional fmt 書式指定文字列
;;; @return 文字列
(defmethod to-string ((f float) &optional (fmt nil))
  (let (ch sc)
    (multiple-value-setq (ch sc) (fmt-ch-scale fmt))

    (case ch
      ((#\R #\r)
       ;; Lisp での print-read 同一性
       (format nil "~s" f)
       )
      ((#\C #\c #\E #\e #\F #\f #\G #\g #\N #\n #\P #\p)
       (call-next-method)
       )
      ((nil)
       (call-next-method f "G")
       )
      (t
       (call-next-method)
       )
      ) ;case
    ) ;let
  )


