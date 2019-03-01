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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;; 浮動小数点数文字列から、整数部分の位置を取得
;;; @param fstr "nnn.ddd" 形式の浮動小数点数文字列
;;; @return 整数部分の index 範囲を示すリスト
(defun extract-int-in-float (fstr)
  (let ((pos-period (or (position #\. fstr) (length fstr)))
        (pos-num (position-if #'digit-char-p fstr))
        )
    (and pos-num (< pos-num pos-period) (list pos-num pos-period))
    ) ;let
  )


;;; 文字列の右端から３桁ごとにカンマを挿入した文字列を作成
;;; @param str-src
;;; @return 
(defun separate-with-comma (str-src)
  (do* ((len-src (length str-src))
        (len-dst (+ len-src (truncate (1- len-src) 3)))
        (str-dst (make-string len-dst))
        (idx-src len-src)
        (idx-dst len-dst)
        (cnt 0)
        )
       ((<= idx-dst 0) str-dst)
    (setf (char str-dst (decf idx-dst))
          (if (< 0 (rem (incf cnt) 4)) (char str-src (decf idx-src)) #\,)
          ) ;setf
    ) ;do*
  )

;;; 固定小数点数文字列を作成
;;; @param f 浮動小数点数
;;; @param sc 小数点以下の桁数
;;; @param &optional separate 整数部をカンマ区切りにするかどうか
;;; @param &optional shift 指定した場合 10^shift 倍の値を出力する
;;; @return
(defun float-string-cs (f sc &optional (separate nil) (shift nil))
  (let* ((fstr (format nil "~,v,vf" sc shift f))
         range-int
         )
    ;; 小数点以下の桁数が 0 の時、小数点を削除
    (when (eql sc 0) ;nilの場合があるので zerop は使わない
      (setf fstr (subseq fstr 0 (position #\. fstr :from-end t)))
      )
    
    (when (and separate (setf range-int (extract-int-in-float fstr)))
      ;; 整数部をカンマ区切りにする
      (setf fstr (format nil "~a~a~a"
                         (subseq fstr 0 (car range-int))
                         (separate-with-comma (subseq fstr (car range-int) (cadr range-int)))
                         (subseq fstr (cadr range-int))
                         ) ;format
            ) ;setf
      ) ;when
    fstr
    ) ;let*
  )

;;; 浮動小数点数文字列から、小数部分の位置を取得
;;; @param fstr "i.ddde+nnn" 形式の浮動小数点数文字列
;;; @return ピリオドを含む小数部分の index 範囲を示すリスト
(defun extract-dec-in-float (fstr)
  (let (pos-period
        pos-dec-end
        )
    (and
     (setf pos-period (position #\. fstr))
     (setf pos-dec-end (or (position-if-not #'digit-char-p fstr :start (1+ pos-period))
                           (length fstr)
                           ) ;or
           ) ;setf
     
     (list pos-period pos-dec-end)
     ) ;and
    ) ;let
  )

;;; 指数形式の浮動小数点数文字列を作成
;;; 小数部末尾の0は省略される
;;; @param f
;;; @param sc 整数部をも含む桁数。1以上
;;; @param echr 指数表記前の文字
;;; @return 
(defun exp-float-string-cs-for-g (f sc &optional (echr #\E))
  (let* ((fstr (format nil "~,v,2,,,,ve" (1- sc) echr f))
         range-dec
         str-dec
         )
    (and
     (setf range-dec (extract-dec-in-float fstr))
     (setf str-dec (string-right-trim "0" (subseq fstr (car range-dec) (cadr range-dec))))
     (format nil "~a~a~a"
             (subseq fstr 0 (car range-dec))
             (if (<= (length str-dec) 1) "" str-dec) ;ピリオドのみの場合ピリオドもなくす
             (subseq fstr (cadr range-dec))
             ) ;format
     ) ;and
    ) ;let*
  )

;;; 整数から浮動小数点数文字列を作成
;;; @param i 整数
;;; @param sc 精度。0の場合19が使用される
;;; @param &optional echr 指数表記前の文字
;;; @return 
(defun exp-float-string-cs-from-int (i sc &optional (echr #\E))
  (let* ((a (abs i))
         (n (1+ (truncate (log a 10)))) ;桁数
         )
    (when (zerop (or sc 0))
      (setf sc 19) ;Int64のデフォルト精度
      ) ;when
    
    (cond
     ((< sc  n)
      ;; 四捨五入の為の加算
      (incf a (* 5 (expt 10 (- n sc 1))))
      
      (setf s (subseq (format nil "~a" a) 0 sc))
      (setf s (string-right-trim "0" s))
      
      (format nil "~a~[~;~a~:;~a.~]~a~a+~2,'0d"
              (if (< i 0) "-" "")
              (length s) (char s 0) (subseq s 1)
              echr (1- n)
              ) ;format
      )
     (t (format nil "~a" i))
     ) ;cond
    
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
;;; @param i 文字列表現にするオブジェクト（ integer ）
;;; @param &optional fmt 書式指定文字列
;;; @return 文字列
(defmethod to-string ((i integer) &optional (fmt nil))
  (let (ch sc s)
    (multiple-value-setq (ch sc) (fmt-ch-scale fmt))

    (case ch
      ((#\C #\c)
       ;; 精度指定子がある場合に小数点以下を出力
       (format nil "\\~:d~[~:;.~:*~v,,,'0a~]" i (or sc 0) "")
       )
      ((#\D #\d)
       (format nil "~a~v,'0d" (if (< i 0) "-" "") sc (abs i))
       )
      ((#\G #\g)
       (exp-float-string-cs-from-int i sc (if (upper-case-p ch) #\E #\e))
       )
      ((#\N #\n)
       (setf sc (or sc 2))
       ;; 精度指定子がある場合に小数点以下を出力
       (format nil "~:d~[~:;.~:*~v,,,'0a~]" i (or sc 0) "")
       )
      ((#\P #\p)
       (setf sc (or sc 2))
       ;; 精度指定子がある場合に小数点以下を出力
       (format nil "~:d~[~:;.~:*~v,,,'0a~]%" (* 100 i) (or sc 0) "")
       )
      ((#\R #\r)
       ;; Lisp での print-read 同一性
       (format nil "~s" i)
       )
      ((#\X #\x)
       ;; 大文字小文字を書式指定文字列と合わせる為のフォーマット切替
       (setf fmt2 (if (upper-case-p ch) "~:@(~v,'0x~)" "~(~v,'0x~)"))
       
       (when (< i 0)
         ;; 負の場合、２の補数表現をした正の値にする
         (setf i (logand (bit-mask-min i) i))
         )
       (format nil fmt2 sc i)
       )
      (t (format nil "~a" i)
         )
      ) ;case
    ) ;let
  )

;;; オブジェクトの文字列表現を取得
;;; @param f 文字列表現にするオブジェクト（ float ）
;;; @param &optional fmt 書式指定文字列
;;; @return 文字列
(defmethod to-string ((f float) &optional (fmt nil))
  (let (ch sc fmt2 s1 n)
    (multiple-value-setq (ch sc) (fmt-ch-scale fmt))

    (case ch
      ((#\C #\c)
       (setf sc (or sc 0))
       (format nil "\\~a" (float-string-cs f sc t))
       )
      ((#\E #\e)
       (setf sc (or sc 6)
             ;; 大文字小文字を書式指定文字列と合わせる為のフォーマット切替
             fmt2 (if (upper-case-p ch) "~,v,3,,,,'Ee" "~,v,3,,,,'ee")
             ) ;setf
       (format nil fmt2 sc f)
       )
      ((#\F #\f) (float-string-cs f sc)
       )
      ((#\G #\g)
       (when (zerop (or sc 0))
         (setf sc (cond
                   ((or (typep f 'long-float) (typep f 'double-float)) 15)
                   (t 7) ;short-float または single-float
                   ) ;cond
               ) ;setf
         ) ;when
       
       (cond
        ;; 指数が-5以下あるいは精度指定子以上の場合は指数表記
        ((or (< (abs f) (expt 10 -4)) (<= (expt 10 sc) (abs f)))
         ;; 大文字小文字を書式指定文字列と合わせる
         (exp-float-string-cs-for-g f sc (if (upper-case-p ch) #\E #\e))
         )
        (t
         ;; G/g では精度指定子は整数部と小数部の両方桁数合計
         (setf n (1+ (truncate (log (abs f) 10)))) ;整数部の桁数
         ;; 固定小数点表記
         (float-string-cs f (- sc n))
         )
        ) ;cond
       )
      ((#\N #\n)
       (setf sc (or sc 2))
       (float-string-cs f sc t)
       )
      ((#\P #\p)
       (setf sc (or sc 2))
       (format nil "~a%" (float-string-cs f sc t 2))
       )
      ((#\R #\r)
       ;; Lisp での print-read 同一性
       (format nil "~s" f)
       )
      (t (format nil "~a" f)
         )
      ) ;case
    ) ;let
  )


