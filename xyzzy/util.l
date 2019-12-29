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
(provide :util)

;;;; (1.5. defpackage)
(defpackage :util (:use :lisp :editor))

;;;; 2. in-package
(in-package :util)

;;;; 3. shadow

;;;; 4. export
(export '(error-when            ;真偽値が真の場合にエラーを発する。（マクロ）
		  error-unless          ;真偽値が偽の場合にエラーを発する。（マクロ）
		  defalias              ;関数あるいはマクロの別名を与える
		  cons*                 ;list* の別名
		  require*              ;複数の require 呼出
		  cats                  ;複数の文字列を結合した文字列（マクロ）
		  cat-string            ;各文字列を結合した文字列
		  list-string           ;リストの各要素を結合したような文字列を作成
		  catfun                ;関数呼出を連鎖させる関数（マクロ）
		  eql-oneof             ;どれかに一致するか
		  cdrf                  ;汎変数の値をその cdr に更新
		  orf                   ;汎変数の値を引数の値 or をとった値に更新
		  andf                  ;汎変数の値を引数の値 and をとった値に更新
		  with-gensyms          ;指定のシンボルを(gensym)で束縛したletフォーム
		  loop-while            ;指定条件が成立する間のループ

		  not-eql               ;(not (eql x y)) と同義
		  and-eql               ;(and (eql ～) (eql ～) ...) と同義
		  nth*                  ;位置を複数指定できる nth
		  nth-place*            ;nth* で取得できる値を持つセル
		  first-char            ;文字列の最初の文字を取得
		  last-char             ;文字列の最後の文字を取得
		  char*                 ;文字列中の文字を取得
		  last1                 ;リストの最後の要素
		  length-0              ;リストあるいは配列の長さが０か
		  length-0<             ;リストあるいは配列の長さが０超過か
		  length-n<             ;リストあるいは配列の長さが指定超過か
		  char-number           ;数字一文字を数値に変換
		  char-number*          ;数字一文字を数値に変換。全角文字にも対応。
		  char-shift            ;指定した文字の文字コードに数値を加算した文字を取得
		  symbol-name-equal     ;シンボルの綴りが一致するか（大文字小文字の区別なし）
		  symbol-name=          ;シンボルの綴りが一致するか（大文字小文字の区別あり）
		  parse-integer*        ;全角文字を扱える parse-integer
		  string-columns        ;文字列の表示幅
		  into-range            ;ある値が上限を上回る場合は上限を、下限を下回る場合は下限を返す。
		  maxf                  ; place および 指定した数値の最大値で place を更新（マクロ）
		  minf                  ; place および 指定した数値の最小値で place を更新（マクロ）
		  max*                  ;nil を指定可能な max
		  min*                  ;nil を指定可能な min
		  sortf                 ;汎変数の値をソートし、汎変数へ反映（マクロ）
		  sort-list-onetime     ;ソート基準の値を作成して、リストソート。（非破壊関数）
		  substitute-list       ;リストの各要素の置き換えを行う。（非破壊関数）
		  substitute-tree       ;木の各要素の置き換えを行う。（非破壊関数）
		  stable-pairlis        ;引数に指定されたキーと値の順番を保つ pairlis
		  ignore-error          ;エラーを無視する

		  ;; --------

		  cond/                 ;
		  if/                   ;

		  ;; --------

		  @@                    ;式中で使用されているシンボル $1 ～ $9 をラムダ式のパラメータにする（マクロ）

		  ;; --------

		  collector-push        ;構造体 collector に要素を追加（蓄積）
		  collector-append      ;構造体 collector に、リストの要素を追加（蓄積）
		  get-collector-list    ;構造体 collector に蓄積した結果（リスト）を取得
		  with-collector        ;（マクロ）構造体 controller を作成し、フォーム終了時に蓄積結果（リスト）を返す

		  collect-while         ;指定関数が真を返す要素を要素とするリスト

		  pme-1                 ;macroexpand-1 の結果を印字

		  ;; --------

		  date-info             ;実行した日付における年月日などの情報をかえす
		  )
		)

;;;; 5. require

;;;; 6. use-package

;;;; 7. import


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; 
;;;; 

;;; 真偽値が真の場合にエラーを発する。
;;; @param condition 真偽値
;;; @param fmt フォーマット文字列。
;;; @param &rest args フォーマット文字列の引数。
;;; @return 真偽値が偽の場合 nil 。
(defmacro error-when (condition fmt &rest args)
  `(when ,condition (error ,fmt ,@args))
  )

;;; 真偽値が偽の場合にエラーを発する。
;;; @param condition 真偽値
;;; @param fmt フォーマット文字列。
;;; @param &rest args フォーマット文字列の引数。
;;; @return 真偽値が真の場合 nil 。
(defmacro error-unless (condition fmt &rest args)
  `(unless ,condition (error ,fmt ,@args))
  )

;;; 関数あるいはマクロの別名を与える
;;; （用途）
;;; ・長い関数名を省略して書きたい
;;; ・別パッケージの一関数のみ使いたいが、その為だけに use-package したくない
;;; @param alias 別名（シンボル）
;;; @param name 実際に呼び出される関数あるいはマクロ（シンボル）
(defmacro defalias (alias name)
  `(defmacro ,alias (&rest args)
	 `(,',name ,@args)
	 )
  )
;;;; 上記マクロ定義を次のように書いてもうまくいかない。
;;;; `(',,name ,@args)
;;;;
;;;; 次のように展開されるためと推測される。（変数 name の値は xxx とする）
;;;; （その１）`(',,name ,@args) => `((quote ,,name) ,@args) => (cons (list 'quote xxx) args)
;;;; （その２）`(,',name ,@args) => `(,(quote ,name) ,@args) => (cons (quote xxx) args)

;;; list* の別名
(defalias cons* list*)

;;; 引数 operator を第１要素とし、引数 lis-args の要素を第２引数以降にする
;;; リストのリストを作成。
;;; （つまり同じ関数の呼出の連続となるようなリストを作成。）
;;; （例）
;;; (expand-args-list 'require '((:web-dictionary "～") :html-edit))
;;; => ((require :web-dictionary "～") (require :html-edit))
;;; @param operator
;;; @param lis-args ※要素が nil の場合、対応のリストは (operator) になる。
;;; 	(operator nil) にしたい場合は、(nil) と指定する。
;;; @return 
(defun expand-args-list (operator lis-args)
  (mapcar #'(lambda (arg)
			  (cond
			   ((listp arg) (cons operator arg))
			   (t (list operator arg))
			   ) ;cond
			  ) ;lambda
		  lis-args
		  ) ;mapcar
  )

;;; 複数の require 呼出
;;; （例）
;;; (require* :web-dictionary :html-edit)
;;; ≒ (list (require :web-dictionary) (require :html-edit))
;;; @param lis-args require に渡す引数を各要素とするリスト
(defmacro require* (&rest lis-args)
  `(list ,@(expand-args-list 'require lis-args)) ;progn
  )

;;; 複数の文字列を結合した文字列
;;; 但し各文字列の末尾には改行を付加
;;; （例）
;;; (cats "aaa" "bbb")
;;; => "aaa
;;;    bbb
;;;    "
;;; @param &rest lis-str 文字列定数のリスト
;;; @return
(defmacro cats (&rest lis-str)
  (format nil "~{~a~%~}" lis-str)
  )

;;; 文字列を結合した文字列
;;; @param opt 
;;; 	:no-newline ： 各文字列の末尾に改行を付加しない。
;;; 	:no-last-newline ： 最後の文字列にのみ改行を付加しない。
;;; 	その他 ： 各文字列の末尾に改行を付加。
;;; @param &rest lis-string 
;;; @return 
(defun cat-string (opt &rest lis-string)
  (let* (fmt
		 )
	(case opt
	  (:no-newline (setf fmt "~{~a~}")
	   ) ;:no-newline
	  (:no-last-newline (setf fmt "~{~a~^~%~}")
	   ) ;:no-last-newline
	  (otherwise (setf fmt "~{~a~%~}")
	   ) ;otherwise
	  ) ;case

	(format nil fmt lis-string)
	) ;let*
  )

;;; リストの要素を結合したような文字列を作成
;;; (format nil "~{~a~}" lis) とほぼ同じ。
;;; @param lis
;;; @param &key recursive 要素がリストの場合に、再帰的に処理するかどうか。
;;; 	デフォルトは nil 。
;;; @return 
(defun list-string (lis &key (recursive nil))
  (with-output-to-string (os)
	(labels ((sub (os lis recur)
			   (dolist (e lis)
				 (cond
				  ((not (consp e)) (princ e os))
				  ((and (consp e) recur) (sub os e recur))
				  ) ;cond
				 ) ;dolist
			   )
			 )
	  (sub os lis recursive)
	  ) ;labels
	) ;with-output-to-string
  )

;;; 関数呼出を連鎖させる関数（マクロ）
;;; fnc1 および fnc-n の最後を除く要素は、一引数呼出となる。
;;; （例）
;;; (catfun hoge #'(lambda (x) (piyo x)) #'fuga))
;;; => #'(lambda (&rest args) (funcall hoge (funcall #'(lambda (x) (piyo x)) (apply #'fuga args))))
;;; @param fnc1 最後に適用する関数
;;; @param &rest fnc-n 
(defmacro catfun (fnc1 &rest fnc-n)
  (let ((lis-fnc (reverse (cons fnc1 fnc-n)))
		)
	(cond (fnc-n (do ((lis-fnc (cdr lis-fnc) (cdr lis-fnc))
					  (result `(apply ,(car lis-fnc) args) `(funcall ,(car lis-fnc) ,result))
					  )
					 ((null lis-fnc) `#'(lambda (&rest args) ,result))
				   ) ;do
				 )
		  (t fnc1)
		  ) ;cond
	) ;let
  )

;;; どれかに一致するか
;;; （例）(eql-oneof x y z) => (or (eql x y) (eql x z))
;;; 検索対象の値が必要時に評価される点が find や member と異なる
;;; @param val
;;; @param &rest args
(defmacro eql-oneof (val &rest args)
  ;; 第１引数が関数呼出なら、評価値を変数に代入するように展開
  (cond ((consp val) (let ((var (gensym)))
					   `(let ((,var ,val))
						  (or ,@(mapcar #'(lambda (e) `(eql ,var ,e)) args))
						  ) ;let
					   ) ;let
		 )
		;; 第１引数が関数呼出でないなら、評価値を変数に代入しない
		(t `(or ,@(mapcar #'(lambda (e) `(eql ,val ,e)) args))
		   )
		) ;cond
  )

;;; 汎変数の値をその cdr に更新
;;; @param place 汎変数
;;; @param &optional N
(defmacro cdrf (place &optional (N 1))
  `(setf ,place (nthcdr ,N ,place))
  )

;;; 汎変数の値を引数の値 or をとった値に更新
;;; @param place 汎変数
;;; @param &rest args
(defmacro orf (place &rest args)
  `(setf ,place (or ,place ,@args))
  )

;;; 汎変数の値を引数の値 and をとった値に更新
;;; @param place 汎変数
;;; @param &rest args
(defmacro andf (place &rest args)
  `(setf ,place (and ,place ,@args))
  )

;;; 指定のシンボルを(gensym)で束縛したletフォーム
;;; （例）
;;; (with-gensyms (a b) ...)
;;; => (let ((a (gensym))
;;;          (b (gensym))
;;;         )
;;;     ...
;;;    )
(defmacro with-gensyms (lis-var &body body)
  `(let ,(mapcar #'(lambda (var) `(,var (gensym))) lis-var)
	   ,@body
	   ) ;let
  )

;;; 指定条件が成立する間のループ
(defmacro loop-while (condition &body body)
  `(loop
	 (unless ,condition (return))
	 ,@body
	 )
  )

;;; (not (eql x y)) と同義
;;; @param x
;;; @param y
;;; @return 
(defun not-eql (x y) (not (eql x y)))

;;; (and (eql ～) (eql ～) ...) と同義
;;; @param &rest args 要素数は偶数個であること。
(defmacro and-eql (&rest args)
  `(and ,@(do ((args args (cddr args))
			   (pairs nil (cons (list 'eql (car args) (cadr args)) pairs))
			   )
			  ((not (consp (cdr args))) (reverse pairs))
			) ;do
	) ;and
  )

;;; 位置を複数指定できる nth
;;; @param a1 （引数の数を制限する為のもの）
;;; @param a2 （引数の数を制限する為のもの）
;;; @param &rest args
(defun nth* (a1 a2 &rest args)
  (let* ((all (reverse (list* a1 a2 args)))
		 (lis (car all))
		 (lis-n (cdr all))
		 )
	(dolist (n lis-n lis)
	  (when (null lis) (return))
	  (setf lis (nth n lis))
	  ) ;dolist
	) ;let*
  )

;;; nth* で取得できる値を持つセル（セルの car が nth* で取得される値）
;;; （技術的な註）
;;; (nth n lis) で取得される値を（ car に）持つセルは、(nthcdr n lis) で取得できる。
;;; @param a1 （引数の数を制限する為のもの）
;;; @param a2 （引数の数を制限する為のもの）
;;; @param &rest args
(defun nth-place* (a1 a2 &rest args)
  (cond
   ((null args)
	(nthcdr a1 a2)
	)
   (t
	(let* ((lis (apply #'nth* a2 args))
		   )
	  (cond
	   ((null lis) lis)
	   (t (nthcdr a1 lis))
	   ) ;cond
	  ) ;let*
	)
   ) ;cond
  )

;;; 文字列の最初の文字を取得
;;; @param str 文字列
;;; @return 最初の文字あるいは nil
(defun first-char (str)
  (when (< 0 (length str))
	(char str 0)
	) ;when
  )

;;; 文字列の最後の文字を取得
;;; @param str 文字列
;;; @return 最後の文字あるいは nil
(defun last-char (str)
  (when (< 0 (length str))
	(char str (1- (length str)))
	) ;when
  )

;;; 文字列中の文字を取得。index が範囲外の場合 nil
;;; @param str 文字列
;;; @return 文字あるいは nil
(defun char* (str idx)
  (when (<= 0 idx (1- (length str)))
	(char str idx)
	) ;when
  )

;;; リストの最後の要素
;;; （Paul Graham 作成のものと同じ）
;;; @param lis
;;; @return
(defun last1 (lis)
  (car (last lis))
  )

;;; リストあるいは配列の長さが０か
;;; @param o リストあるいは配列
;;; @return 
(defun length-0 (o)
  (cond
   ((null o) t)
   ((consp o) nil)
   (t (zerop (length o)))
   ) ;cond
  )

;;; リストあるいは配列の長さが０超過か
;;; @param o リストあるいは配列
;;; @return 
(defun length-0< (o)
  (not (length-0 o))
  )

;;; リストあるいは配列の長さが指定超過か
;;; @param n 長さ
;;; @param o リストあるいは配列
;;; @return 長さが n 超過かどうか
(defun length-n< (n o)
  (cond
   ((minusp n) t)
   ((consp o) (and (nthcdr n o) t))
   (t (< n (length o)))
   ) ;cond
  )

;;; 
(defvar *ch-code-0* (char-code #\0))
;;; 
(defvar *ch-code-upper-a* (char-code #\A))
;;; 
(defvar *ch-code-lower-a* (char-code #\a))

;;; 数字一文字を数値に変換
;;; @param ch 文字
;;; @param &optional radix 基数。省略時 10 。
;;; @return 
(defun char-number (ch &optional (radix 10))
  (let* ((code (char-code ch))
		 (num (cond
			   ((char<= #\0 ch #\9) (- code *code-0*))
			   ((char<= #\A ch #\Z) (+ 10 (- code *code-upper-a*)))
			   ((char<= #\a ch #\z) (+ 10 (- code *code-lower-a*)))
			   ) ;cond
			  )
		 )
	(error-unless (<= 2 radix 36) "範囲外の基数です。 : ~a" radix)

	(and num (< num radix) num)
	) ;let*
  )

;;; 
(defvar *ch-code-0f* (char-code #\０))
;;; 
(defvar *ch-code-upper-af* (char-code #\Ａ))
;;; 
(defvar *ch-code-lower-af* (char-code #\ａ))

;;; 数字一文字を数値に変換。全角文字にも対応。
;;; @param ch 文字
;;; @param &optional radix 基数。省略時 10 。
;;; @return 
(defun char-number* (ch &optional (radix 10))
  (let* ((code (char-code ch))
		 (num (cond
			   ((char<= #\0 ch #\9) (- code *ch-code-0*))
			   ((char<= #\A ch #\Z) (+ 10 (- code *ch-code-upper-a*)))
			   ((char<= #\a ch #\z) (+ 10 (- code *ch-code-lower-a*)))
			   ((char<= #\０ ch #\９) (- code *ch-code-0f*))
			   ((char<= #\Ａ ch #\Ｚ) (+ 10 (- code *ch-code-upper-af*)))
			   ((char<= #\ａ ch #\ｚ) (+ 10 (- code *ch-code-lower-af*)))
			   ) ;cond
			  )
		 )
	(error-unless (<= 2 radix 36) "範囲外の基数です。 : ~a" radix)

	(and num (< num radix) num)
	) ;let*
  )

;;; 指定した文字の文字コードに数値を加算した文字を取得
;;; （例）(char-shift #\0 9) =>#\9
;;; @param ch-base 文字
;;; @param offset 加算する値（整数）
(defun char-shift (ch-base offset)
  (code-char (+ (char-code ch-base) offset))
  )

;;; シンボルの綴りが一致するか（大文字小文字の区別なし）
;;; @param s1 シンボル
;;; @param s2 シンボル
;;; @return 
(defun symbol-name-equal (s1 s2)
  (string-equal (symbol-name s1) (symbol-name s2))
  )

;;; シンボルの綴りが一致するか（大文字小文字の区別あり）
;;; @param s1 シンボル
;;; @param s2 シンボル
;;; @return 
(defun symbol-name= (s1 s2)
  (string= (symbol-name s1) (symbol-name s2))
  )

;;; 全角文字を扱える parse-integer
;;; ※junk-allowed t で数字がない場合の動作は xyzzy 準拠。エラーにならず nil を返す。
;;; @param string 
;;; @param &key start
;;; @param &key end
;;; @param &key junk-allowed （※動作は xyzzy 準拠）
;;; @return 
(defun parse-integer* (string &key (start 0) (end nil) (radix 10) (junk-allowed nil))
  (let ((neg-flag nil)
		idx
		(result 0)
		)
	(flet ((space-p (ch) (find ch " 　"))
		   (return-value () (values (if neg-flag (- result) result) idx))
		   )
	  ;; end が nil の場合、数値に変換
	  (unless end
		(setf end (length string))
		) ;unless

	  ;; 文字列先頭のスペースを飛ばす
	  (when junk-allowed
		(setf start (or (position-if-not #'space-p string :start start :end end) end))
		) ;when

	  ;; xyzzy 互換の為、ここで idx を初期化
	  (setf idx start)

	  (when (< idx end)
		(cond
		 ;; 先頭がプラスか？
		 ((find (char string idx) "+＋") (incf idx))
		 ;; 先頭がマイナスか？
		 ((find (char string idx) "-－") (setf neg-flag t) (incf idx))
		 ) ;cond
		) ;when

	  (cond
	   ((<= end idx) (cond (junk-allowed (if (< start end) (values result idx) (values nil idx)))
						   (t (error "不正な数値の形式です: ~s" string))
						   ) ;cond
		)
	   (t (do* ((ch)
				(n)
			   )
			  ((<= end idx) (return-value))
			(setf ch  (char string idx)
				  idx (1+ idx)
				  n (char-number* ch radix)
				  ) ;setf
			(unless n
			  (if junk-allowed
				  (return (return-value))
				(error "不正な数値の形式です: ~s" string)
				) ;if
			  ) ;unless
			(setf result (+ (* radix result) n))
			) ;do*
		)
	   ) ;cond
	  ) ;flet
	) ;let
  )

;;; 文字列の表示幅
;;; @param s 文字列
;;; @param &key start 開始 index
;;; @param &key end 終了 index + 1
;;; @return 半角を 1 とした幅。
(defun string-columns (s &key (start 0) (end nil))
  (reduce #'(lambda (w ch) (+ w (char-columns ch)))
		  s
		  :initial-value 0
		  :start start :end end
		  ) ;reduce
  )

;;; ある値が上限を上回る場合は上限を、下限を下回る場合は下限を返す。
;;; @param n
;;; @param low 下限
;;; @param up 上限
(defun into-range (n low up)
  (cond
   ((< n low) low)
   ((< up n) up)
   (t n)
   ) ;cond
  )

;;; place および 指定した数値の最大値で place を更新
;;; @param place 汎変数
;;; @param number 数値
;;; @param &rest more-numbers 数値（0 個以上）
;;; @return 
(defmacro maxf (place number &rest more-numbers)
  `(setf ,place (max ,place ,number ,@more-numbers))
  )

;;; place および 指定した数値の最小値で place を更新
;;; @param place 汎変数
;;; @param number 数値
;;; @param &rest more-numbers 数値（0 個以上）
;;; @return 
(defmacro minf (place number &rest more-numbers)
  `(setf ,place (min ,place ,number ,@more-numbers))
  )

;;; nil を指定可能な max
;;; @param number
;;; @param &rest more-numbers
;;; @return 引数全てがnilの場合はnil
(defun max* (number &rest more-numbers)
  (let* ((lis-val (remove nil (cons number more-numbers)))
		 )
	(and lis-val (apply #'max lis-val))
	) ;let*
  )

;;; nil を指定可能な min
;;; @param number
;;; @param &rest more-numbers
;;; @return 引数全てがnilの場合はnil
(defun min* (number &rest more-numbers)
  (let* ((lis-val (remove nil (cons number more-numbers)))
		 )
	(and lis-val (apply #'min lis-val))
	) ;let*
  )

;;; 汎変数の値をソートし、汎変数へ反映
;;; @param predicate ソートするのに用いる関数
;;; @param &rest lis-place 汎変数（複数）
(defmacro sortf (predicate &rest lis-place)
  (let ((gvect (gensym))
		)
	`(let ((,gvect (stable-sort (vector ,@lis-place) ,predicate))
		   )
	   (setf ,@(do ((len (length lis-place))
					(lis nil (nconc lis (list (elt lis-place i) `(elt ,gvect ,i))))
					(i 0 (1+ i))
					)
				   ((<= len i) lis)
				) ;do
			 ) ;setf
	   ) ;let
	) ;let
  )

;;; ソート基準の値を作成して、リストソート。（非破壊関数）
;;; ソートの基準とする値を作成、ソート、作成した値を削除。
;;; （例）
;;; (sort-list-onetime #'(lambda (n) (rem n 2)) #'< '(1 2 3 4 5 6))
;;; => (2 4 6 1 3 5)
;;; @param fnc-keyvalue ソートの基準とする値を求める関数。
;;; 	第１引数：リストの各要素
;;; @param fnc-compare ソートするための比較関数
;;; @param lis ソートするリスト
;;; @return 
(defun sort-list-onetime (fnc-keyvalue fnc-compare lis)
  (map 'list #'cdr
	   (stable-sort (map 'vector #'(lambda (e) (cons (funcall fnc-keyvalue e) e))
						 lis
						 ) ;map
					fnc-compare
					:key #'car
					) ;sort
	   ) ;map
  )

;;; リストの各要素の置き換えを行う。（非破壊関数）
;;; 置き換えを行うかの判断と、置き換え後の値を関数で求める。
;;; （例）
;;; (substitute-list #'1+ #'numberp '(a 1 2 (12 13)))
;;; => (a 2 3 (12 13))
;;; @param fnc-new-item 関数 fnc-test が真を返した場合に、
;;; 	置き換え後の値を求める関数
;;; @param fnc-test 要素を置き換えるか判断する関数。
;;; @param lis リスト
;;; @return 
(defun substitute-list (fnc-new-item fnc-test lis)
  (mapcar #'(lambda (e) (if (funcall fnc-test e)
							(funcall fnc-new-item e)
						  e
						  ) ;if
			  ) ;lambda
		  lis
		  ) ;mapcar
  )

;;; 木の各要素の置き換えを行う。（非破壊関数）
;;; 置き換えを行うかの判断と、置き換え後の値を関数で求める。
;;; 要素がリストで、関数 fnc-test が偽を返した場合は、
;;; そのリストの各要素についても再帰的に処理を行う。
;;; （例）
;;; (substitute-tree #'1+ #'numberp '(a 1 2 (12 13)))
;;; => (a 2 3 (13 14))
;;; @param fnc-new-item 関数 fnc-test が真を返した場合に、
;;; 	置き換え後の値を求める関数
;;; @param fnc-test 要素を置き換えるか判断する関数。
;;; @param tree 木
;;; @return 
(defun substitute-tree (fnc-new-item fnc-test tree)
  (mapcar #'(lambda (e) (cond
						 ((funcall fnc-test e) (funcall fnc-new-item e))
						 ((consp e) (substitute-tree fnc-new-item fnc-test e))
						 (t e)
						 ) ;cond
			  ) ;lambda
		  tree
		  ) ;mapcar
  )

;;; 引数に指定されたキーと値の順番を保つ pairlis
;;; @param keys
;;; @param data
;;; @param &optional a-list
;;; @return a-list
(defun stable-pairlis (keys data &optional (a-list nil))
  (nconc (mapcar #'cons keys data) a-list)
  )

;;; エラーを無視する
;;; @param &body body
(defmacro ignore-error (&body body)
  `(handler-case
	   (progn ,@body)
	 (error (c) )
	 ) ;handler-case
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; cond/ / if/
;;;;

;;; リストを特定の要素で分割する。（非破壊）
;;; @param lis 分割対象のリスト。
;;; @param &key fnc 区分の印を判別する関数。
;;; @param &key initial 一番最初の区分の印。
;;; @return 引数 fnc で判別された要素で始まるリストのリスト
(defun split-list-by (lis &key (fnc #'keywordp) (initial nil))
  (util:with-collector (d)
	(util:with-collector (c)
	  (util:collector-push initial c)
	  (do* ((e)
			)
		  ((null lis))
		(when (funcall fnc (setf e (pop lis)))
		  (util:collector-push (util:get-collector-list c) d)
		  ) ;when

		(util:collector-push e c)
		) ;do*

	  (util:collector-push (util:get-collector-list c) d)
	  ) ;with-collector
	) ;with-collector
  )

;;; リストを指定のシンボルで分割
;;; @param lis-symbol 区分の印とするシンボルのリスト
;;; @param lis 分割対象のリスト
;;; @param &key initial 一番最初の区分の印。
;;; @return 
(defun split-list-by-symbol (lis-symbol lis &key (initial nil))
  (split-list-by lis :fnc #'(lambda (e) (find e lis-symbol))
						 :initial initial
						 ) ;split-list-into-chunk
  )

;;; cond の (p e1 e2 ... en) を展開するように記述する cond
;;; (p e1 e2 ... en) ⇒ :case p e1 e2 ... en
;;; @param &rest lis 
;;; @return 
(defmacro cond/ (&rest lis)
  (let* ((lis-chunk (split-list-by-symbol '(:case) lis))
		 )
	;; 最初に :case を忘れていたらエラー（親切）
	(when (cdar lis-chunk)
	  (error ":case が指定されていません。")
	  ) ;when
	(pop lis-chunk) ;最初の要素を破棄

	`(cond
	  ,@(mapcar #'(lambda (chunk)
					(unless (cdr chunk)
					  (error ":case 節に predicate がありません。")
					  ) ;unless
					`(,(cadr chunk) ,@(cddr chunk))
					) ;lambda
				lis-chunk
				) ;mapcar
	  ) ;cond
	) ;let*
  )

;;; if の then 節と else 節を :else で区切る if 。
;;; @param test 
;;; @param &rest lis 
;;; @return 
(defmacro if/ (test &rest lis)
  (let* ((pos-else (position :else lis))
		 (then (subseq lis 0 pos-else))
		 (else (if pos-else (nthcdr (1+ pos-else) lis)))
		 )
	(flet ((sexp-1 (lis) (if (cdr lis) `(progn ,@lis) (car lis)))
		   )
	  `(if ,test ,(sexp-1 then) ,@(if else `(,(sexp-1 else))))
	  ) ;flet
	) ;let*
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; @@
;;;; 

;;; パラメータに使えるシンボルか？
;;; @param sym 
;;; @return 
(defun parameter-symbol-p (sym &aux (name (symbol-name sym)))
  (and (eql (length name) 2)
	   (eql (char name 0) #\$)
	   (char<= #\1 (char name 1) #\9)
	   ) ;and
  )

;;; リストに含まれるパラメータに使えるシンボルを収集。
;;; 最上位要素のみでなく、内部リスト内も収集対象。
;;; @param lis 
;;; @return 
(defun collect-parameter-symbol (lis)
  (let* ((result nil)
		 )
	(labels ((collect (lis)
			   (dolist (e lis)
				 (cond ((and e (symbolp e) (parameter-symbol-p e)) (pushnew e result))
					   ((consp e) (collect e))
					   ) ;cond
				 ) ;dolist
			   )
			 )
	  (collect lis)
	  result
	  ) ;labels
	) ;let*
  )

;;; シンボルのパラメータ番号
;;; @param sym $1 ～ $9
;;; @return 1 ～ 9
(defun parameter-number (param)
  (parse-integer (symbol-name param) :start 1)
  )

;;; パラメータを使用したラムダリスト。
;;; 存在しない番号に該当する仮引数は gensym で生成したシンボル。
;;; @param lis-param パラメータ（ $1 ～ $9 ）を要素とするリスト
;;; @return 
(defun lambda-list-for-@@ (lis-param)
  (let* ((ary (make-array 10))
		 num
		 (num-max 0)
		 )
	;; パラメータ（ $1 ～ $9 ）を番号に対応する箇所にセット
	(dolist (param lis-param)
	  (setf num (parameter-number param)
			num-max (max num-max num)
			(aref ary num) param
			) ;setf
	  ) ;dolist

	;; 空き番になっているパラメータ番号の箇所に gensym をセット
	(do* ((idx 1 (1+ idx))
		  )
		((< num-max idx))
	  (unless (aref ary idx)
		(setf (aref ary idx) (gensym))
		) ;unless
	  ) ;do*

	(coerce (subseq ary 1 (1+ num-max)) 'list)
	) ;let*
  )

;;; 式中で使用されているシンボル $1 ～ $9 をラムダ式のパラメータにする
;;; （例）
;;; (@@ (hoge $1 a))  => #'(lambda ($1) (hoge $1 a))
;;; （※当初 $$ という名前だったが、 LispWorks に同名のシンボルがあるので変更）
;;; @param &rest lis-sexp
(defmacro @@ (&rest lis-sexp)
  (let* ((lis-param (collect-parameter-symbol lis-sexp)
					)
		 )
	;; ※現状、抽出したパラメータの厳密なチェックはしていない。
	;; 　（追加すべきチェックの例）
	;; 　・同名かつ別パッケージのシンボルが存在するかどうか
	;; 　・キーワードでないかどうか
	`#'(lambda ,(lambda-list-for-@@ lis-param) ,@lis-sexp)
	) ;let*
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; collector
;;;; 

;;;; ********************************************************************************
;;;; ※開発の註 (collector)
;;;; 外部から内部変数を保護する為、当初クロージャを使う実装をしていたが、
;;;; パッケージを分けていることから、構造体にしても問題はないと気づいて、
;;;; そのような実装に変更した。
;;;; （無理矢理内部シンボルにアクセスしない限り問題ない）
;;;; ********************************************************************************

;;; リスト末尾に要素追加する為の構造体
;;; ※スロットの宣言は、(head (list nil)) (tail head)と書きたいが、
;;; 　initformで別スロットを参照できるのか不明なので、ラムダリストの&auxで指定
(defstruct (collector
			(:constructor make-collector (&aux (head (list nil)) (tail head)))
			)
  head
  tail
  )

;;; 構造体 collector に要素を追加（蓄積）
;;; @param e 追加する要素
;;; @param c 構造体collector
;;; @return 引数e
(defun collector-push (e c)
  (let* ((new-node (list e))
		 )
	(setf (cdr (collector-tail c)) new-node
		  (collector-tail c) new-node
		  ) ;setf
	e
	) ;let*
  )

;;; 構造体 collector に、リストの要素を追加（蓄積）
;;; @param lis 追加する要素を含むリスト
;;; @param c 構造体collector
;;; @return 引数lis
(defun collector-append (lis c)
  (dolist (e lis)
	(collector-push e c)
	) ;dolist
  )

;;; 構造体 collector に蓄積した結果（リスト）を取得
;;; 蓄積は消去される
;;; @param c 構造体 collector
;;; @return 蓄積結果
(defun get-collector-list (c)
  (prog1
	  (cdr (collector-head c))

	;; 蓄積を消去
	(setf (cdr (collector-head c)) nil
		  (collector-tail c) (collector-head c)
		  ) ;setf
	) ;prog1
  )

;;; 構造体 controller を作成し、フォーム終了時に蓄積結果（リスト）を返す
;;; （例）
;;; (with-collector (c) (collector-push 'a c) (collector-push 'b c))
;;; 	=> (a b)
;;; ※フォーム内で蓄積結果を得たい場合は、get-collector-list を使えばよい
;;; @param var collectorをセットする変数（シンボル）
;;; @param &body body 実行フォーム
;;; @return controller に蓄積された要素（リスト）
(defmacro with-collector ((var) &body body)
  `(let* ((,var (make-collector))
		  )
	 ,@body
	 (get-collector-list ,var)
	 ) ;let*
  )

;;; 指定関数が真を返す要素を要素とするリスト
;;; mapcar と異なり、指定関数が偽を返すと収集を終了する。
;;; @param predicate 要素を結果に追加するかどうかを判定する関数。
;;; @param lis 
;;; @param &key key predicate へ渡す前に、リストの要素へ適用する関数
;;; @param &key fnc-value 追加する値へ適用する関数。
;;; 	第１引数：リストの要素
;;; 	第２引数： predicate が返した値
;;; @return 
(defun collect-while (predicate lis &key (key #'identity) (fnc-value #'(lambda (e &optional p) e)))
  (let* ((fnc-p (catfun predicate key))
		 p
		 )
	(with-collector (c)
	  (dolist (e lis)
		(unless (setf p (funcall fnc-p e)) (return))
		(collector-push (funcall fnc-value e p) c)
		) ;dolist
	  ) ;with-collector
	) ;let*
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; 
;;;; 
;;;; 

;;; macroexpand-1 の結果を印字
;;; macroexpand-1 を直接呼び出すと、戻り値に含まれる eq なオブジェクトが
;;; 煩わしい余計な表示がされる。それを防ぐ為に用いる。
;;; @param expr 展開する式
(defmacro pme-1 (expr)
  `(progn
	 (print (macroexpand-1 ',expr))
	 nil
	 )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; 
;;;; 
;;;; 


;;; １分の秒数
(defconstant +sec-of-min+ 60)
;;; １時間の秒数
(defconstant +sec-of-hour+ (* 60 +sec-of-min+))
;;; １日の秒数
(defconstant +sec-of-day+ (* 24 +sec-of-hour+))


;;; 実行した日付における年月日などの情報をかえす
;;; キー：:year, :month, :day, :day-of-week, :hour, :min, :sec
;;; (年（西暦） 月 日 曜日（0=月曜） 時 分 秒)
;;; @param &optional add-days 日数のオフセット
;;; @return plist
(defun date-info (&optional (add-days 0))
  (multiple-value-bind (sec min hour day mon year day-of-week daylight tz)
	  (decode-universal-time (+ (get-universal-time) (* +sec-of-day+ add-days)))
	(list :year year :month mon :day day :day-of-week day-of-week
		  :hour hour :min min :sec sec
		  ) ;list
	) ;multiple-value-bind
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; let** 
;;;; xyzzy の不具合により、let* でグローバル変数の再束縛を指定した場合に、
;;;; 変数の参照がうまくいかない。それを解消する為の let*
;;;; 

;;; 連続した要素をグループ値でグルーピング
;;; （例）
;;; (split-by-group #'evenp '(1 3 4 8 10 5))
;;; => ((nil 1 3) (t 4 8 10) (nil 5))
;;; @param fnc グループ値（ハッシュ値のようなもの）を求める関数
;;; @param lis
;;; @param &key test グループ値の一致を判定する関数
;;; @return リスト。各要素は (<グループ値> <要素> ... <要素>)
(defun split-by-group (fnc lis &key (test #'eql))
  (when lis
	(do* ((lis lis (cdr lis))
		  (lis-cd (mapcar fnc lis) (cdr lis-cd))
		  (cd-last nil cd)
		  (cd (car lis-cd) (car lis-cd))
		  (rtest t (funcall test cd-last cd))
		  (grp nil)
		  (lis-grp nil)
		  )
		((null lis)
		 (push (cons cd-last (reverse grp)) lis-grp)
		 (reverse lis-grp)
		 )
	  (unless rtest
		(push (cons cd-last (reverse grp)) lis-grp)
		(setf grp nil)
		) ;unless

	  (push (car lis) grp)
	  ) ;do*
	) ;when
  )

;;; グローバル変数を複数の let* に分けて束縛する let*
;;; （ xyzzy の不具合対策）
;;; @param lis-var 
;;; @param &body body
(defmacro let** (lis-var &body body)
  (labels ((make-form (llis-var body)
			 (cond
			  ((null llis-var) body)
			  ((null (cdr llis-var)) `(let* ,(car llis-var)
										,@(make-form (cdr llis-var) body)
										) ;let*
			   )
			  (t `(let* ,(car llis-var)
					,(make-form (cdr llis-var) body)
					) ;let*
				 )
			  ) ;cond
			 )
		   )
	(make-form
	 ;; mapcan でグローバル変数の束縛は個別に分解
	 (mapcan #'(lambda (grp)
				 (if (car grp) (mapcar #'list (cdr grp)) (list (cdr grp)))
				 ) ;lambda
			 ;; グローバル変数とローカル変数でグルーピング
			 (split-by-group #'(lambda (e) (boundp (if (symbolp e) e (car e)))) lis-var)
			 ) ;mapcan
	 body
	 ) ;make-form
	) ;labels
  )


