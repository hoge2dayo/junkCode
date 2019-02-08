;;;; require - provide - defpackage - in-package - export

;;; （関数 compile-lhtml で 関数 lhtml:print-parsed-lhtml を呼び出すのに必要）
(require :lhtml-user)
;;; 
(provide :lhtml)
;;; 
(defpackage :lhtml (:use :lisp :editor))
;;; 
(in-package :lhtml)

;;; 
(export '(make-html
		  )
		)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; ■HTML と Lisp コードが混在した lhtml ファイルをコンパイル・実行し、
;;;; 　html ファイルを生成する
;;;; 
;;;; ● lhtml ファイル
;;;; HTML ファイルの一部に Razor 構文を取り入れたもの。
;;;; 
;;;; ○ @(～)
;;;; 括弧でくくられた全体をＳ式フォームと見なし、評価結果をその位置に出力。
;;;; 
;;;; ○ @{ ～ }
;;;; 実行する Lisp コードを既述。評価値は直接的には出力されない。
;;;; Ｓ式は @{ ～ } 内で完結している必要はない。
;;;; 
;;;; ○ @@
;;;; 文字 `@` をその位置に出力
;;;; 
;;;; ○ @require <パッケージ名>
;;;; lhtml ファイル内で使用するパッケージを指定。
;;;; 指定できるパッケージは一つで、その後の既述は行末まで無視される。
;;;; 複数のパッケージを求める場合は複数既述。
;;;; 
;;;; ○ @using <パッケージ名>
;;;; 指定したパッケージの外部シンボルを、パッケージ修飾なしで記述可能にする。
;;;; 指定できるパッケージは一つで、その後の既述は行末まで無視される。
;;;; 複数のパッケージを求める場合は複数既述。
;;;; 
;;;; 
;;;; ● lhtml の例
;;;;   <html>
;;;;   <head></head>
;;;;   <body>
;;;;   @{
;;;;   (dotimes (i 5)
;;;;   }
;;;;   <img src="./img/@(1+ i).jpg" /><br/>
;;;;   @{
;;;;    ) ;dotimes の終端
;;;;   }
;;;;   </body>
;;;;   </html>
;;;; 
;;;; 上の既述により出力・生成される html ファイルは次のようになる。
;;;; 
;;;;   <html>
;;;;   <head></head>
;;;;   <body>
;;;;   
;;;;   <img src="./img/1.jpg" /><br/>
;;;;   <img src="./img/2.jpg" /><br/>
;;;;   <img src="./img/3.jpg" /><br/>
;;;;   <img src="./img/4.jpg" /><br/>
;;;;   <img src="./img/5.jpg" /><br/>
;;;;   
;;;;   </body>
;;;;   </html>
;;;; 
;;;; 

;;; バッファよりＳ式を一つ読み取る
;;; @param &optional pnt 読取開始位置
;;; @param &optional buf バッファ
;;; @return 多値
;;; 	[0]: 読取終了位置+1  [1]: 読み取ったＳ式
(defun read-sexp-from-buffer (&optional (pnt (point)) (buf (selected-buffer)))
  (let* ((is (make-buffer-stream buf pnt))
		 (sexp (read is))
		 )
	(values (buffer-stream-point is) sexp)
	) ;let*
  )

;;; バッファの現在位置が開始文字列に一致するなら、終了文字列を検索してその位置を返す
;;; @param str-start ブロックの開始文字列
;;; @param str-end ブロックの終了文字列
;;; @return 終了文字列の位置
;;;		現在位置の文字列が開始文字列に合致しない場合 => nil
;;;		終了文字列が存在しない場合 => エラー
(defun read-block-end-pos (str-start str-end)
  (let* (pnt-end
		 )
	(when (looking-for str-start)
	  (setf pnt-end (progn (scan-buffer str-end) (point)))
	  (unless pnt-end
		(error "終端がありません。：~a" str-end)
		) ;unless

	  pnt-end
	  ) ;when
	) ;let*
  )

;;; カーソル位置の文字列が指定文字列と一致するなら、
;;; その文字列後のＳ式を読取り、次の行頭位置をＳ式を返す
;;; @param token バッファ上で続くと思われる文字列
;;; @return リスト。第１要素：次の行頭位置。第２要素：読み取ったＳ式
;;; 	呼出時のカーソル位置が引数 token と不一致な場合 nil
(defun read-token-sexp-line (token)
  (when (looking-at (format nil "~a[ \\t]+" token))
	;; Ｓ式読取
	(multiple-value-bind (pnt-end sexp)
	  (read-sexp-from-buffer (match-end 0))

	  ;; Ｓ式の終端として改行を読み飛ばした場合は、改行直前へ位置を移動
	  (when (eql (preceding-char) #\NewLine)
		(goto-char (1- pnt-end))
		) ;when

	  ;; 改行直後（次の行頭）の位置とＳ式を返す
	  (list (if (scan-buffer "\n" :tail t) (point) (point-max))
			sexp
			) ;list
	  ) ;multiple-value-setq
	) ;when
  )


;;; lhtml （ Razor 構文を含む html ）のバッファを読み取り、字句解析結果を返す
;;; @return 字句解析結果をセクション別にした p-list
;;; 	(:require <リスト> :use-package <リスト> :main <リスト>)
(defun parse-lhtml ()
  (goto-char (point-min))

  (do* ((lis-main nil)
		(lis-req nil) ;(:require ～) のリスト
		(lis-use nil) ;(:use-package ～) のリスト
		(pnt-start (point))
		(pnt-end (and (scan-buffer "@") (point))
				 (and (scan-buffer "@") (point))
				 )
		(sexp)
		(pnt-sexp)
		)
	  ((null pnt-end) (progn
						(push (buffer-substring pnt-start (point-max)) lis-main)
						(list :require (reverse lis-req)
							  :use-package (reverse lis-use)
							  :main  (reverse lis-main)
							  ) ;list
						) ;progn
	   )
	;; （空文字列は追加しない）
	(when (< pnt-start pnt-end)
	  (push (buffer-substring pnt-start pnt-end) lis-main)
	  ) ;when

	;; @ の次の位置
	(setf pnt-start (progn (forward-char) (point)))

	(cond
	 ;; @( ～ )
	 ((looking-for "(")
	  (setf pnt-end (read-sexp-from-buffer pnt-start)) ;開き括弧もＳ式の一部
	  (push `(:sexp ,(buffer-substring pnt-start pnt-end)) lis-main)
	  )
	 ;; @{ ～ }
	 ((setf pnt-end (read-block-end-pos "{" "}"))
	  (push `(:block ,(buffer-substring (1+ pnt-start) pnt-end)) lis-main)
	  (incf pnt-end)
	  )
	 ;; @* ～ *@
	 ((setf pnt-end (read-block-end-pos "*" "*@"))
	  (push `(:comment ,(buffer-substring (1+ pnt-start) pnt-end)) lis-main)
	  (incf pnt-end 2)
	  )
	 ;; @@
	 ((looking-for "@")
	  (push "@" lis-main)
	  (setf pnt-end (1+ pnt-start))
	  )
	 ;; @require
	 ((setf pnt-sexp (read-token-sexp-line "require"))
	  (push `(:require ,(cadr pnt-sexp)) lis-req) ;lis-req に push

	  (setf pnt-end (car pnt-sexp))
	  )
	 ;; @using
	 ((setf pnt-sexp (read-token-sexp-line "using"))
	  (push `(:use-package ,(cadr pnt-sexp)) lis-use) ;lis-use に push

	  (setf pnt-end (car pnt-sexp))
	  )
	 ;; その他
	 (t
	  (error "不正なトークンです。")
	  )
	 ) ;cond

	(setf pnt-start pnt-end)
	(goto-char pnt-start)
	) ;do
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; lhtml コンパイル（=> lobj ）
;;;; lobj ロード
;;;; html ファイル作成（ lobj => html ）
;;;; 

;;; バッファ名をセット
;;; @param name バッファ名
;;; @param &optional buf バッファ
;;; @return バッファ名（※引数とは異なる可能性あり）
(defun set-buffer-name (name &optional (buf (selected-buffer)))
  (rename-buffer name buf)
  (buffer-name buf)
  )

;;; エラー発生時に非局所脱出せずに指定した値を評価
;;; @param value-if-error エラー発生時の値
;;; @param &body body
(defmacro ignore-error (value-if-error &body body)
  `(handler-case
	   (progn ,@body) ;progn
	 (error (c) ,value-if-error)
	 ) ;handler-case
  )

;;; バッファを作成し、ファイルをロードする。
;;; @param path 
;;; @param &key encoding バッファの文字コード
;;; @param &key eol バッファの改行コード
;;; @return ファイルが存在しなかった場合 nil
;;; 	ファイルが存在した場合、読込先のバッファ
(defun create-and-read-file (path &key encoding eol)
  (let* ((buf-org (selected-buffer))
		 (name (file-namestring path)) ;バッファ名
		 (buf (create-new-buffer name))
		 )
	(block nil
	  (unwind-protect
		  (progn
			(set-buffer buf)

			(handler-case
				(read-file path)
			  ;; ファイル読込失敗
			  (error (c)
				(delete-buffer buf) ;バッファ削除
				(return nil)
				)
			  ) ;handler-case

			;; 文字コードおよび改行コードのセット
			(ignore-error nil
			  (when encoding (set-buffer-fileio-encoding encoding))
			  (when eol (set-buffer-eol-code eol))
			  ) ;ignore-error

			buf
			) ;progn

		;; 以下、クリーンアップ
		(set-buffer buf-org)
		) ;unwind-protect
	  ) ;block
	) ;let*
  )

;;; 拡張子のみ異なるファイルパスを作成
;;; @param type 拡張子（※ピリオドは含まない）
;;; @param pathname-base 基礎とするファイルパス
;;; @return ファイル拡張子のみ第２引数と異なるファイルパス
(defun merge-filetype (type pathname-base)
  (let* ((name (pathname-name pathname-base))
		 )
	(merge-pathnames (format nil "~a.~a" name type)
					 (directory-namestring pathname-base)
					 ) ;merge-pathnames
	) ;let*
  )

;;; lhtml-user のメイン関数名
;;; @param title 
;;; @return メイン関数名（文字列）
(defun main-func-name (title)
  (format nil "~a.main" title)
  )

;;; 指定した名称のシンボルを取得
;;; 外部シンボル／内部シンボルが一致しなければ、シンボルを取得できないので注意。
;;; @param sym-name パッケージ名を含むシンボル名。（例）lisp:car
;;; @return シンボル（シンボルがなかった場合 nil）
(defun find-symbol* (sym-name)
  (when (string-match "^\\([^:]*\\)\\(::?\\)\\([^:]+\\)$" sym-name)
	(let* ((pre-pack (match-string 1))
		   (pack (if (< 0 (length pre-pack)) pre-pack "keyword"))
		   (name (match-string 3))
		   (access (nth (length (match-string 2)) '(nil :external :internal)))
		   (lis (ignore-error nil (multiple-value-list (find-symbol name pack))))
		   )
	  ;; アクセス修飾子（:external / :internal）が一致した場合のみ、シンボルを返す
	  (and lis (eql access (cadr lis)) (car lis))
	  ) ;let*
	) ;when
  )

;;; 指定した名称のシンボルに関連付けられた関数を取得
;;; @param sym-name パッケージ名を含むシンボル名。（例）lisp:car
;;; @return 関数
(defun find-symbol-function (sym-name)
  (let* ((sym (find-symbol* sym-name))
		 (func (and sym (ignore-error nil (symbol-function sym))))
		 )
	(cond
	 ((null sym) (error "シンボルがありません。：~a" sym-name)
	  )
	 ((null func) (error "シンボルに関連付けられた関数がありません。：~a" sym-name)
	  )
	 (t func
		)
	 ) ;cond
	) ;let*
  )


;;; lhtml ファイルをコンパイル（ lhtml => lobj ）
;;; @param path-lhtml lhtml ファイルのパス
;;; @return 
(defun compile-lhtml (path-lhtml)
  (let* ((path-lobj (merge-filetype "lobj" path-lhtml))
		 (name-lobj (file-namestring path-lobj))
		 (title-lobj (pathname-name path-lobj))
		 (buf-lhtml (create-and-read-file path-lhtml)) ;lhtml ファイル読込
		 buf-lobj
		 parsed-lhtml
		 (func-print (find-symbol-function "lhtml-user:print-parsed-lhtml"))
		 )
	(unless buf-lhtml
	  (error "lhtml ファイル読込に失敗しました。")
	  ) ;unless

	;; lhtml 解析、バッファ削除
	(set-buffer buf-lhtml)
	(setf parsed-lhtml (parse-lhtml))
	(delete-buffer buf-lhtml)

	;; lobj 用バッファ
	(setf buf-lobj (create-new-buffer name-lobj))
	(set-buffer buf-lobj)
	;; ファイルパスセット
	(set-buffer-file-name path-lobj)
	;; バックアップとらないようにする
	(make-local-variable 'make-backup-files)
	(setq make-backup-files nil)

	(with-output-to-buffer (buf-lobj)
	  (funcall func-print parsed-lhtml (main-func-name title-lobj))
	  ) ;with-output-to-buffer

	;; lobj セーブ
	(save-buffer)
	;; lobj 用バッファ削除
	(delete-buffer buf-lobj)

	t
	) ;let*
  )

;;; パッケージ lhtml-user を削除して再ロード
;;; 過去の実行された lobj の影響をなくす目的
(defun reload-lhtml-user ()
  (delete-package :lhtml-user)
  (load-library "lhtml-user")
  )

;;; lobj ファイルのロード・評価
;;; @param path-lobj lobj ファイルのパス
;;; @return 
(defun load-lobj (path-lobj)
  (let* ((title-lobj (pathname-name path-lobj))
		 )
	(when (equal title-lobj "")
	  (error "拡張子のみのファイル名は許されません。：~a" title-lobj)
	  ) ;when

	;; パッケージ lhtml-user を削除して再ロード
	(reload-lhtml-user)

	;; 過去のロード後の情報をクリア
	(funcall (find-symbol-function "lhtml-user:clear-info"))
	;; ロード
	(load path-lobj)
	;; ロード後の情報（登録された関数名）
	(funcall (find-symbol-function "lhtml-user:get-info"))
	) ;let*
  )

;;; html ファイル作成（ lobj => html ）
;;; lobj ファイルで登録されてる関数を実行して、html ファイルを作成
;;; lobj ファイルはロード・評価済みであること
;;; @param path-lobj lobj ファイルのパス
;;; @param &key request リクエスト（ p-list ）
;;; @return 
(defun exec-lobj (path-lobj &key request)
  (let* ((title-lobj (pathname-name path-lobj))
		 buf-html
		 (path-html (merge-filetype "html" path-lobj))
		 (sym-name (format nil "lhtml-user::~a" (main-func-name title-lobj)))
		 (func (find-symbol-function sym-name))
		 )
	(setf buf-html (create-new-buffer (format nil "~a.html" title-lobj)))
	(set-buffer buf-html)
	;; 文字コードセット
	(set-buffer-fileio-encoding *encoding-utf8*)
	;; 改行コードセット
	(set-buffer-eol-code *eol-lf*)
	;; ファイルパスセット
	(set-buffer-file-name path-html)
	;; バックアップとらないようにする
	(make-local-variable 'make-backup-files)
	(setq make-backup-files nil)

	(unwind-protect
		(progn
		  (with-output-to-buffer (buf-html)
			;; lobj のメイン関数呼出
			(funcall func request)
			) ;with-output-to-buffer

		  ;; html セーブ
		  (save-buffer)
		  ) ;progn

	  ;; クリーンアップ
	  ;; html 用バッファ削除
	  (delete-buffer buf-html)
	  ) ;unwind-protect

	t
	) ;let*
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; lhtml ファイルから html ファイルを作成
;;;; lhtml ファイル、 lobj ファイル、 html ファイルの存在及びファイル時刻より、
;;;; makefile の様に最小限の処理で html ファイルを作成
;;;; 

;;; ファイルの更新日時を取得
;;; @param path ファイルパス
;;; @return 多値
;;; 	[0]: ユニバーサル時刻形式の値
;;; 	[1]: 年 [2]: 月 [3]: 日 [4]: 時  [5]: 分 [6]: 秒
(defun file-datetime (path)
  (let* ((ut #+xyzzy(file-write-time path)
			 #-xyzzy(file-write-date path)
			 ) ;universal time
		 )
	(when ut
	  (multiple-value-bind (se mi ho da mo ye)
		  (decode-universal-time ut)
		(values ut ye mo da ho mi se)
		) ;multiple-value-bind
	  ) ;when
	) ;let*
  )

;;; 依存ファイルより新しい生成ファイルが存在するか
;;; @param path-target 生成ファイルのパス
;;; @param path-source 依存ファイルのパス
;;; @return 真の時、新しい生成ファイルが既にある
(defun newer-target-exist-p (path-target path-source)
  (let* ((ut-target (file-datetime path-target))
		 (ut-source (file-datetime path-source))
		 )
	(and ut-source ut-target (<= ut-source ut-target))
	) ;let*
  )

;;; 指定モジュール名の html ファイルを作成
;;; 必要に応じて、lhtml のコンパイルおよび lobj ファイルのロード・評価を行う。
;;; @param module-name モジュール名（lhtml ファイルの拡張子を除いた部分）
;;; 	lhtml ファイルは require で読み込めるフォルダにある必要あり。
;;; 	lobj および html ファイルも同じフォルダに作成される。
;;; @param &key request lobj 実行時の引数。p-list 推奨。
;;; @return 
(defun make-html (module-name &key request)
  (let* ((lhtml-name (format nil "~a.lhtml" module-name))
		 (path-lhtml (find-load-path lhtml-name)) ;ファイルを *load-path* から検索したフルパス
		 path-lobj
		 path-html
		 )
	(unless path-lhtml
	  (error "lhtml ファイルが見つかりません。")
	  ) ;unless

	;; lhtml => lobj
	(setf path-lobj (merge-filetype "lobj" path-lhtml))
	(unless (newer-target-exist-p path-lobj path-lhtml)
	  (compile-lhtml path-lhtml)
	  ) ;unless

	;; lobj => html
	;; request の値により、出力が変わる可能性が常にあるので、
	;; 最新ファイルかどうかに拘わらず実行
	(setf path-html (merge-filetype "html" path-lhtml))
	(load-lobj path-lobj)                  ; lobj ロード
	(exec-lobj path-lobj :request request) ; lobj 実行

	(msgbox "html ファイルを作成しました。~%~a" path-html)

	t
	) ;let*
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; ■Razor 構文メモ
;;;; 
;;;; ●インライン式：単一のプロパティ／メソッド／フィールド変数の値の出力
;;;; 
;;;; <p>プロパティ：@DateTime.Now</p>
;;;; 
;;;; <p>メソッド：  @DateTime.Now.ToLongDateString() ←スペースで終了</p>
;;;; 
;;;; <p>フィールド：@DateTime.MaxValue：←全角コロンでコード終了</p>
;;;; 
;;;; <p>@("現在の時刻：" + DateTime.Now)</p>
;;;; 
;;;; ●コード・ブロック：複数の文からなるブロックの記述
;;;; 
;;;; @{
;;;;   int year = DateTime.Now.Year;
;;;;   string msg = (DateTime.IsLeapYear(year)) ? "うるう年" : "平年";
;;;; }
;;;; 
;;;; ●コード・ブロック内での変数や文字列の出力
;;;; 
;;;; @{
;;;;   string msg = DateTime.Now.ToLongDateString();
;;;;   <p>コード・ブロック内の変数を、HTMLタグ付きで出力：@msg</p>
;;;; }
;;;; 
;;;; @{
;;;;   string msg = DateTime.Now.ToLongDateString();
;;;;   <text>コード・ブロック内の変数を、textタグで出力：@msg</text>
;;;; }
;;;; 
;;;; @{
;;;;   string msg = DateTime.Now.ToLongDateString();
;;;;   @:この1行が、そのまま出力される。<br>変数も使える：@msg
;;;; }
;;;; 
;;;; ●コメント
;;;; 
;;;; @* この部分はコメントになる *@
;;;; @*
;;;;   当然、複数行で記述することもできる
;;;; *@
;;;; 
;;;; @{
;;;;   @* この部分はコメントになる *@
;;;;   // 当然、C#のコメントも使える
;;;;   /* C#における複数行のコメント */
;;;; }
;;;; 
;;;; ●「@」のエスケープ処理
;;;; 
;;;; <p>@@isshiki宛にツイートしてください。</p>
;;;; 
;;;; ●名前空間のインポート
;;;; 
;;;; @using System.Text.RegularExpressions;
;;;; @{
;;;;   string str= "こんにちは";
;;;;   if (Regex.IsMatch(str, @"^\p{IsHiragana}*$"))
;;;;   {
;;;;     @:全部、ひらがなです。
;;;;   }
;;;;   else
;;;;   {
;;;;     @:ひらがなではない文字が含まれています。
;;;;   }
;;;; }
;;;; 
;;;; ●条件分岐
;;;; 
;;;; @* ifコード・ブロック *@
;;;; @{int num = 1;}
;;;; @if (num > 0) {
;;;;   <p>正の数</p>
;;;; } else if (num == 0) {
;;;;   <p>0</p>
;;;; } else {
;;;;   <p>負の数</p>
;;;; }
;;;; 
;;;; @* switchコード・ブロック *@
;;;; @{int i = 1;}
;;;; @switch (i) {
;;;;   case 1:
;;;;     <p>「1」です。</p>
;;;;     break;
;;;;   default:
;;;;     <p>「1」以外です。</p>
;;;;     break;
;;;; }
;;;; 
;;;; ●繰り返し処理
;;;; 
;;;; @* forコード・ブロック *@
;;;; @for (int i = 0; i < 3; i++) {
;;;;   <p>for文のループの「@i」回目です。</p>
;;;; }
;;;; 
;;;; @* foreachコード・ブロック *@
;;;; @{int[] items = {1,2,3};}
;;;; @foreach (var item in items) {
;;;;   <p>foreach文のループで処理される個別項目の値は「@item」です。</p>
;;;; }
;;;; 
;;;; @* whileコード・ブロック *@
;;;; @{var j = 0;}
;;;; @while (j < 3) {
;;;;   <p>for文のループの「@j」回目です。</p>
;;;;   j++;
;;;; }
;;;; 
;;;; ●関数
;;;; 
;;;; @functions {
;;;; 
;;;;   // メソッドの定義
;;;;   int Sum(int a, int b)
;;;;   {
;;;;     return a + b;
;;;;   }
;;;; 
;;;;   // プロパティの定義
;;;;   int _num = 0;
;;;;   int Num
;;;;   {
;;;;     get
;;;;     {
;;;;       return _num;
;;;;     }
;;;;     set
;;;;     {
;;;;       _num = value;
;;;;     }
;;;;   }
;;;; }
;;;; 
;;;; <p>１＋２＝ @Sum(1, 2)</p>
;;;; <p>プロパティの値： @Num</p>
;;;; 


