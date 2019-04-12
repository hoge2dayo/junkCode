;;;; require - provide - defpackage - in-package - export

(require :geometric)

(provide :dlg)

(defpackage :dlg (:use :lisp :editor))

(in-package :dlg)

(export '(create-label
		  create-button
		  create-text
		  create-edit
		  create-check
		  create-check3
		  create-combobox
		  create-listbox
		  create-column-layout
		  create-row-layout
		  create-edge-layout
		  display-dialog
		  sample-dialog
		  )
		)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; ■ウィンドウスタイル用定数
;;;; 

;;;; ●ウィンドウ

(defvar +WS-POPUPWINDOW+       #x80880000) ; +WS-POPUP+、+WS-BORDER+、+WS-SYSMENU+ の併用。コントロールメニューを可視にするには、+WS-CAPTION+ と +WS-POPUPWINDOW+ が必要
(defvar +WS-POPUP+             #x80000000) ; ポップアップウィンドウ。+WS-CHILD+ とは併用不可
(defvar +WS-CHILD+             #x40000000) ; 子ウィンドウ。+WS-POPUP+ とは併用不可
(defvar +WS-MINIMIZE+          #x20000000) ; 初期状態でアイコン化されたウィンドウを作成。+WS-OVERLAPPED+ 必須
(defvar +WS-VISIBLE+           #x10000000) ; 初期状態で可視のウィンドウ
(defvar +WS-DISABLED+          #x08000000) ; 初期状態で使用禁止のウィンドウ
(defvar +WS-MAXIMIZE+          #x01000000) ; 最大表示されたウィンドウ
(defvar +WS-OVERLAPPEDWINDOW+  #x00CF0000) ; +WS-CAPTION+、+WS-SYSMENU+、+WS-THICKFRAME+、+WS-MINIMIZEBOX+、+WS-MAXIMIZEBOX+、+WS-OVERLAPPED+ の併用
(defvar +WS-CAPTION+           #x00C00000) ; タイトル バーを持つウィンドウ（暗黙の +WS-BORDER+ ）。+WS-DLGFRAME+ とは併用不可
(defvar +WS-BORDER+            #x00800000) ; 境界を持つウィンドウ
(defvar +WS-DLGFRAME+          #x00400000) ; 二重境界を持ち、タイトルを持たないウィンドウ
(defvar +WS-VSCROLL+           #x00200000) ; 垂直スクロールバーを持つウィンドウ
(defvar +WS-HSCROLL+           #x00100000) ; 水平スクロールバーを持つウィンドウ
(defvar +WS-SYSMENU+           #x00080000) ; タイトルバーにコントロールメニューボックスを持つウィンドウ。タイトルバーを持つウィンドウにのみ使用
(defvar +WS-THICKFRAME+        #x00040000) ; ウィンドウのサイズ変更に使用可能な太い枠を持つウィンドウ
(defvar +WS-GROUP+             #x00020000) ; グループの最初のコントロールの指定。最初のコントロールの後で +WS-GROUP+ スタイルでないコントロールは同じグループに属する
(defvar +WS-MINIMIZEBOX+       #x00020000) ; アイコン化ボタンを持つウィンドウ
(defvar +WS-MAXIMIZEBOX+       #x00010000) ; 最大表示ボタンを持つウィンドウ
(defvar +WS-SIZEBOX+           #x0000F2C0) ; +WS-THICKFRAME+ と同じ
(defvar +WS-OVERLAPPED+        #x00000000) ; オーバラップ ウィンドウを作成

;;;; ●ドロップダウンリスト、コンボボックス

(defvar +CBS-LOWERCASE+        #x00004000) ; コンボボックスに入力された大文字を小文字に変換
(defvar +CBS-UPPERCASE+        #x00002000) ; コンボボックスに入力された小文字を大文字に変換
(defvar +CBS-DISABLENOSCROLL+  #x00000800) ; 項目が少ないときに垂直スクロールバーを不活性で表示
(defvar +CBS-NOINTEGRALHEIGHT+ #x00000400) ; コンボボックスのサイズがアプリケーション指定のサイズとなる。通常は Windows がサイズを変更
(defvar +CBS-SORT+             #x00000100) ; リストボックス内の文字列を自動的にソート
(defvar +CBS-OEMCONVERT+       #x00000080) ; 入力されたテキストを、ANSI 文字セット⇒OEM 文字セット⇒ANSI 文字セットに変換（戻す）。ファイル名を保持するコンボボックスで有用。+CBS-SIMPLE+ または +CBS-DROPDOWN+ のコンボボックスでのみ指定可能
(defvar +CBS-DROPDOWN+         #x00000002)
(defvar +CBS-SIMPLE+           #x00000001) ; リストボックスを常に表示。リストボックスの現在の選択項目がエディットコントロールに表示される

;;;; ●リストボックス

(defvar +LBS-NOSEL+            #x00004000) ; 読取り専用（選択不可）
(defvar +LBS-DISABLENOSCROLL+  #x00001000) ; 項目が少ないときに垂直スクロールバーを不活性で表示
(defvar +LBS-EXTENDEDSEL+      #x00000800) ; 特殊なキーの組み合わせで、ユーザーが複数項目選択可能
(defvar +LBS-NOINTEGRALHEIGHT+ #x00000100) ; リストボックスのサイズがアプリケーション指定のサイズとなる。通常は Windows がサイズを変更
(defvar +LBS-USETABSTOPS+      #x00000080) ; 文字列中のタブ文字を認識し展開する。デフォルトのタブ位置は 32 ダイアログ単位
(defvar +LBS-MULTIPLESEL+      #x00000008) ; クリックまたはダブルクリックにより、文字列の選択が切り替わる
(defvar +LBS-SORT+             #x00000002) ; リストボックス内の文字列をアルファベット順にソート

;;;; ●エディットボックス

(defvar +ES-NUMBER+            #x00002000) ; 数字の入力のみ受け付ける
(defvar +ES-WANTRETURN+        #x00001000) ; 複数行のコントロールでの入力時 Enter キーで改行
(defvar +ES-READONLY+          #x00000800) ; テキストの入力や編集ができないようにする
(defvar +ES-OEMCONVERT+        #x00000400) ; 入力されたテキストを、ANSI 文字セット⇒OEM 文字セット⇒ANSI 文字セットに変換（戻す）。ファイル名を保持するエディットボックスで有用。
(defvar +ES-NOHIDESEL+         #x00000100) ; 入力フォーカスを受け取ったときの強調表示をしない
(defvar +ES-AUTOHSCROLL+       #x00000080) ; 行末での入力でテキストを自動的に 10 文字分右へスクロール
(defvar +ES-AUTOVSCROLL+       #x00000040) ; 最終行での Enter キーで自動的に 1 ページ分上にスクロール
(defvar +ES-PASSWORD+          #x00000020) ; 入力されたすべての文字を アスタリスク (*) で表示
(defvar +ES-LOWERCASE+         #x00000010) ; 入力された文字をすべて小文字に変換
(defvar +ES-UPPERCASE+         #x00000008) ; 入力された文字をすべて大文字に変換
(defvar +ES-MULTILINE+         #x00000004) ; 複数行
(defvar +ES-RIGHT+             #x00000002) ; テキストを右揃えで表示
(defvar +ES-CENTER+            #x00000001) ; テキストを中央揃えで表示
(defvar +ES-LEFT+              #x00000000)

;;;; ●ボタン、チェックボックス、ラジオボタン、グループボックス

(defvar +BS-BUTTON+            #x00010000)
(defvar +BS-FLAT+              #x00008000) ; 平坦なボタン
(defvar +BS-NOTIFY+            #x00004000) ; BN-DBLCLK,BN-KILLFOCUS,BN-SETFOCUS 通知メッセージを親に送る
(defvar +BS-MULTILINE+         #x00002000) ; 複数行テキストを表示
(defvar +BS-PUSHLIKE+          #x00001000) ; プッシュボタンのようなチェックボックスまたはラジオボタン
(defvar +BS-VCENTER+           #x00000C00) ; テキストを垂直方向で中央寄せ
(defvar +BS-BOTTOM+            #x00000800) ; テキストを下寄せ
(defvar +BS-TOP+               #x00000400) ; テキストを上寄せ
(defvar +BS-CENTER+            #x00000300) ; テキストを水平方向で中央寄せ
(defvar +BS-RIGHT+             #x00000200) ; テキストを右寄せ
(defvar +BS-LEFT+              #x00000100) ; テキストを左寄せ
(defvar +BS-BITMAP+            #x00000080) ; ビットマップを表示
(defvar +BS-ICON+              #x00000040) ; アイコンを表示
(defvar +BS-LEFTTEXT+          #x00000020) ; チェックボックスとらラジオボタンのチェック欄を右に配置
(defvar +BS-OWNERDRAW+         #x00000011) ; オーナー描画ボタン
(defvar +BS-AUTORADIOBUTTON+   #x00000009) ; グループ内で自動的に一つだけチェック状態にするラジオボタン
(defvar +BS-USERBUTTON+        #x00000008) ; Win32 では使わない
(defvar +BS-GROUPBOX+          #x00000007) ; グループボックス
(defvar +BS-AUTO3STATE+        #x00000006) ; 三つの状態が自動的に巡回するチェックボックス
(defvar +BS-3STATE+            #x00000005) ; チェック・アンチェックのほか不定を表示するチェックボックス
(defvar +BS-RADIOBUTTON+       #x00000004) ; ラジオボタン
(defvar +BS-AUTOCHECKBOX+      #x00000003) ; チェックマークが自動的に変更されるチェックボックス
(defvar +BS-CHECKBOX+          #x00000002) ; チェックボックス
(defvar +BS-DEFPUSHBUTTON+     #x00000001) ; 太枠のデフォルトボタン。ダイアログボックスでの Enter キー押下で選択可能
(defvar +BS-TEXT+              #x00000000) ; ボタンはテキストを表示（デフォルト）

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; ■
;;;; 

;;; プロパティリストのマージ
;;; ２引数に共通するプロパティは plis-add の値が優先される
;;; @param plis-add 優先される p-list
;;; @param plis-base デフォルトの plist
;;; @return プロパティリスト
(defun merge-plist (plis-add plis-base)
  (do ((rem plis-add (cddr rem))
	   (plis (copy-list plis-base)) ;定数指定時を考えて複製
	   )
	  ((null rem) plis)
	 (setf (getf plis (car rem)) (cadr rem))
	) ;do
  )

;;; place および 指定した数値の最大値で place を更新
;;; @param place 汎変数
;;; @param number 数値
;;; @param &rest more-numbers 数値（0 個以上）
;;; @return 
(defmacro maxf (place number &rest more-numbers)
  `(setf ,place (max ,place ,number ,@more-numbers))
  )

;;; place を place および 指定した数値の最小値で更新
;;; @param place 汎変数
;;; @param number 数値
;;; @param &rest more-numbers 数値（0 個以上）
;;; @return 
(defmacro minf (place number &rest more-numbers)
  `(setf ,place (min ,place ,number ,@more-numbers))
  )


;;; 半角文字幅を１、全角文字幅を２とした文字列の幅
;;; @param s 文字列
;;; @return 文字列の幅（半角文字を１、全角文字を２とする）
(defun string-columns (s)
  (reduce #'(lambda (w ch) (+ w (char-columns ch))) s
		  :initial-value 0
		  ) ;reduce
  )

;;; 文字列の行数と幅
;;; 幅は半角文字を１、全角文字を２とする
;;; @param s 文字列
;;; @return リスト。第１要素：行数、第２要素：最大幅
(defun caption-rows-columns (s)
  (let* ((rows (split-string s #\Linefeed t))
		 )
	(list (length rows)
		  (reduce #'max
				  (mapcar #'string-columns rows)
				  :initial-value 0
				  ) ;reduce
		  ) ;list
	) ;let*
  )

;;; 文字の高さあるいは全角文字の幅
(defparameter *font-len* 9)

;;; 文字列が占めるサイズ
;;; @param s 文字列
;;; @return サイズ（*font-len* 単位）
(defun string-size (s)
  (let* ((row-col (caption-rows-columns s))
		 (col (cadr row-col))
		 (row (max (car row-col) 1)) ;空文字列でも１行とする
		 )
	(geo:create-size (round (* *font-len* 1/2 col)) (* *font-len* row))
	) ;let*
  )

;;; オプション指定により、サイズを調整
;;; ※破壊的
;;; @param siz サイズ（※スロット値が変更される）
;;; @param option オプション指定のプロパティリスト
;;; @return 調整後のサイズ（引数 siz ）
(defun adjust-size-by-option (siz option)
  (let* ((width (getf option :width))
		 (height (getf option :height))
		 (min-width (getf option :min-width))
		 (max-width (getf option :max-width))
		 (min-height (getf option :min-height))
		 (max-height (getf option :max-height))
		 )
	;; width
	(when width (setf (geo:size-w siz) width)) ;when
	;; height
	(when height (setf (geo:size-h siz) height)) ;when
	;; min-width
	(when min-width (maxf (geo:size-w siz) min-width)) ;when
	;; max-width
	(when max-width (minf (geo:size-w siz) max-width)) ;when
	;; min-height
	(when min-height (maxf (geo:size-h siz) min-height)) ;when
	;; max-height
	(when max-height (minf (geo:size-h siz) max-height)) ;when

	siz
	) ;let*
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; ■コントロール、レイアウト
;;;; 

;;; コントロール（ ctrl ）およびレイアウト（ layout ）の親クラス（継承用）
(defstruct element
  (rect (geo:create-rect 0 0 0 0)) ;この要素の為に確保された矩形領域
  (option nil)                     ;オプション（プロパティリスト）
  )

;;; マージン（コントロール用）
(defparameter *ctrl-margin* 4)
;;; パディング（コントロール用）
(defparameter *ctrl-padding* 0)

;;; コントロール（継承用）
(defstruct (ctrl (:include element))
  (description-name nil)   ;関数 dialog-box の :control 節で使用する型名。継承先で値を設定すること
  (margin *ctrl-margin*)   ;マージン
  (padding *ctrl-padding*) ;パディング
  (name nil)               ;各コントロール個別の名称
  (caption "")             ;キャプション
  (style nil)              ;ウィンドウスタイル用定数の組み合わせ
  )

;;; コントロールの矩形サイズ（マージンとパディングを含む）
;;; @param obj 構造体 ctrl
;;; @return コントロールのサイズ
(defun size-ctrl (obj)
  (let* ((margin (ctrl-margin obj))
		 (padding (ctrl-padding obj))
		 (siz (string-size (ctrl-caption obj)))
		 )
	;; padding を含めた後にオプションによるサイズ調整
	(geo:enlarge-size* siz (* 2 padding) (* 2 padding))
	(adjust-size-by-option siz (element-option obj))

	(geo:enlarge-size* siz (* 2 margin) (* 2 margin))
	) ;let*
  )

;;; 関数 dialog-box の :control 節へ渡す式
;;; マージンは含めないコントロールのみの矩形を返す
;;; @param obj コントロール（構造体 ctrl）
;;; @return 関数 dialog-box の :control 節へ渡す式
(defun description-ctrl (obj)
  (let* ((rct (element-rect obj))
		 (margin (ctrl-margin obj))
		 )
	(list (ctrl-description-name obj) (ctrl-name obj) (ctrl-caption obj) (ctrl-style obj)
		  (+ (geo:rect-x rct) margin)
		  (+ (geo:rect-y rct) margin)
		  (max (- (geo:rect-w rct) (* 2 margin)) 0)
		  (max (- (geo:rect-h rct) (* 2 margin)) 0)
		  ) ;list
	) ;let*
  )

;;; レイアウト（継承用）
(defstruct (layout (:include element))
  (lis-elm nil)  ;内部に保持・配置する要素
  )

;;; 矩形を位置でソートするための比較関数
;;; Y 座標が異なる場合、Y 座標が小さい方が小とする
;;; Y 座標が同じ場合は、X 座標が小さい方が小とする
;;; （コントロールをフォーカス移動順に並べるのに使用）
;;; @param rct0 矩形
;;; @param rct1 矩形
;;; @return rct0 < rct1 の場合に真
(defun rect-pos< (rct0 rct1)
  (let* ((y0 (geo:rect-y rct0))
		 (y1 (geo:rect-y rct1))
		 )
	(cond
	 ((< y0 y1) t)
	 ((eql y0 y1) (< (geo:rect-x rct0) (geo:rect-x rct1)))
	 (t nil)
	 ) ;cond
	) ;let*
  )

;;; 関数 dialog-box の :control 節へ渡す式のリスト
;;; コントロールはフォーカスの移動が一般的になるようにソートする
;;; （ edge-layout で :bottom を使った場合、ソートしないと
;;; 　下のコントロールが先に現れてしまう）
;;; @param obj レイアウト（構造体 layout）
;;; @return リスト。各要素は関数 dialog-box の :control 節へ渡す式
(defun description-layout (obj)
  (cond
   ((ctrl-p obj) (list (description-ctrl obj)))
   ((layout-p obj) (let* ((lis-elm (copy-list (layout-lis-elm obj)))
						  )
					 ;; 位置でソート
					 (setf lis-elm (sort lis-elm #'rect-pos< :key #'element-rect))
					 (mapcan #'description-layout lis-elm)
					 ) ;let*
	)
   ) ;cond
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; ■ラベル
;;;; 

(defstruct (label
			(:include ctrl
			 (description-name :static)
			 ;;+WS-GROUP+ の必要性が定かではないが、バージョンダイアログではセットされている
			 (style (logior +WS-CHILD+ +WS-VISIBLE+ +WS-GROUP+))
			 )
			(:constructor create-label (caption &key margin padding option))
			)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; ■ボタン
;;;; 

;;; パディング（ボタン用）
(defparameter *button-padding* 4)

;;; ボタン
(defstruct (button
			(:include ctrl
			 (description-name :button)
			 (padding *button-padding*)
			 (style (logior +WS-CHILD+ +BS-BUTTON+ +WS-VISIBLE+))
			 )
			(:constructor create-button (name caption &key margin padding option))
			)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; ■テキストボックス
;;;; 

;;; パディング（テキストボックス用）
(defparameter *edit-padding* 3)

;;; テキストボックス（継承用）
(defstruct (edit-base
			(:include ctrl
			 (description-name :edit)
			 (padding *edit-padding*)
			 )
			)
  )

;;; テキストボックス（単一行）
(defstruct (edit
			(:include edit-base
			 (style (logior +WS-CHILD+ +WS-VISIBLE+ +WS-BORDER+ #x00010000
							+ES-AUTOHSCROLL+ +ES-LEFT+))
			 )
			;; （コンストラクタ名に注意）
			(:constructor create-text (name &key margin padding option))
			)
  )

;;; テキストボックス（複数行）
(defstruct (edit-multi
			(:include edit-base
			 (style (logior +WS-CHILD+ +WS-VISIBLE+ +WS-BORDER+ #x00010000
							+ES-WANTRETURN+ +ES-AUTOVSCROLL+ +ES-MULTILINE+ +ES-LEFT+))
			 )
			;; :option 指定の値は、指定値をスロットに格納するのではなく、
			;; デフォルト値とマージした値を格納する
			(:constructor create-edit (name &key margin padding ((:option o) nil)
											&aux
											(default-option `(:min-width 80
															  :min-height ,(+ (* 2 padding) (* 2 *font-len*))
															  )
															)
											(option (merge-plist o default-option))))
			)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; ■チェックボックス
;;;; 

;;; パディング（チェックボックス用）
(defparameter *check-padding* 2)

;;; チェックボックス（継承用）
(defstruct (check-base
			(:include ctrl
			 (description-name :button) ;:button である点に注意
			 (padding *check-padding*)
			 )
			)
  )

;;; チェックボックス（２ステート）
(defstruct (check
			(:include check-base
			 (style (logior +WS-CHILD+ +WS-VISIBLE+ #x00010000 +BS-AUTOCHECKBOX+))
			 )
			(:constructor create-check (name caption &key margin padding option))
			)
  )

;;; チェックボックス（３ステート）
(defstruct (check3
			(:include check-base
			 (style (logior +WS-CHILD+ +WS-VISIBLE+ #x00010000 +BS-AUTO3STATE+))
			 )
			(:constructor create-check3 (name caption &key margin padding option))
			)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; ■コンボボックス
;;;; 

;;; パディング（コンボボックス用）
(defparameter *combobox-padding* 4)

;;; コンボボックス
(defstruct (combobox
			(:include ctrl
			 (description-name :combobox)
			 (padding *combobox-padding*)
			 (style (logior +WS-CHILD+ +WS-VISIBLE+ +WS-VSCROLL+ #x00010000 +CBS-DISABLENOSCROLL+ +CBS-DROPDOWN+))
			 )
			(:constructor create-combobox (name &key margin padding option))
			)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; ■リストボックス
;;;; 

;;; パディング（リストボックス用）
(defparameter *listbox-padding* 4)

;;; リストボックス
(defstruct (listbox
			(:include ctrl
			 (description-name :listbox)
			 (padding *listbox-padding*)
			 (style (logior +WS-CHILD+ +WS-VISIBLE+ +WS-BORDER+
							+WS-VSCROLL+ +WS-HSCROLL+ #x00010000 +CBS-SIMPLE+
							) ;logior
					)
			 )
			;; :option 指定の値は、指定値をスロットに格納するのではなく、
			;; デフォルト値とマージした値を格納する
			(:constructor create-listbox (name &key margin padding ((:option o) nil)
											   &aux
											   (default-option `(:min-width 80
																 :min-height ,(+ (* 2 padding) (* 4 *font-len*))
																 )
															   )
											   (option (merge-plist o default-option))))
			)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; ■columnレイアウト
;;;; 
;;;; 内部要素を縦に並べる。
;;;; 

;;; column レイアウト
(defstruct (column-layout
			(:include layout
			 )
			(:constructor create-column-layout (lis-elm &key align valign)
			 )
			)
  (align :left)  ;水平方向の寄せ　:left：左 / :center：中央 / :right：右
  (valign :top)  ;垂直方向の寄せ　:top：上 / :center：中央 / :bottom：下
  )

;;; 内部要素を含むサイズ
;;; @param obj column レイアウト（構造体 column-layout）
;;; @return レイアウトのサイズ
(defun size-column-layout (obj)
  (reduce #'(lambda (siz siz-elm)
			  (maxf (geo:size-w siz) (geo:size-w siz-elm))
			  (incf (geo:size-h siz) (geo:size-h siz-elm))
			  siz
			  ) ;lambda
		  (mapcar #'size-element (column-layout-lis-elm obj))
		  :initial-value (geo:create-size 0 0)
		  ) ;reduce
  )

;;; 内部要素の配置（各要素のスロット rect 値の決定）
;;; レイアウト（第１引数）の rect は設定済みであること
;;; 内部要素を縦に並べる
;;; @param obj column レイアウト（構造体 column-layout）
;;; @return 
(defun arrange-column-layout (obj)
  (let* ((align (column-layout-align obj))
		 (rct (geo:copy-rect* (element-rect obj)))
		 (left   (geo:rect-l rct))
		 (right  (geo:rect-r rct))
		 (bottom (geo:rect-b rct))
		 (lis-elm (column-layout-lis-elm obj))
		 (lis-elm-siz (mapcar #'size-element lis-elm))
		 (y (geo:rect-y rct))  ;内部要素配置開始する Y 座標
		 ;; 一番下に配置されたコントロールの下の位置
		 (last-y (reduce #'+ (mapcar #'geo:size-h lis-elm-siz)
						 :initial-value y
						 ) ;reduce
				 )
		 ;; 縦の寄せ幅
		 (off-y (case (column-layout-valign obj)
				  (:center (round (- bottom last-y) 2))
				  (:bottom (- bottom last-y))
				  (t 0)
				  ) ;case
				)
		 )
	;; 各内部要素の位置・サイズ（スロット rect）をセット
	(map nil #'(lambda (elm elm-siz
							&aux
							(elm-w (geo:size-w elm-siz))
							(elm-h (geo:size-h elm-siz))
							)
				 (setf (element-rect elm) (geo:create-rect (case align
															 (:right (- right elm-w))
															 (:center (round (- (+ left right) elm-w) 2))
															 (t left)
															 ) ;case
														   (+ y off-y)
														   elm-w
														   elm-h
														   ) ;geo:create-rect
					   ) ;setf
				 (incf y elm-h)
				 ) ;lambda
		 lis-elm
		 lis-elm-siz
		 ) ;map

	;; 内部要素にレイアウトがある場合、そのレイアウトの内部配置
	(map nil #'arrange-layout (remove-if-not #'layout-p lis-elm))
	) ;let*
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; ■row レイアウト
;;;; 
;;;; 内部要素を横に並べる。
;;;; 

;;; row レイアウト
(defstruct (row-layout
			(:include layout)
			(:constructor create-row-layout (lis-elm &key valign spread))
			)
  (valign :top)  ;垂直方向の寄せ　:top：上 / :center：中央 / :bottom：下
  (spread nil)   ;水平方向にコントロールを広げるかどうか
  )

;;; 内部要素を含むサイズ
;;; @param obj row レイアウト（構造体 row-layout）
;;; @return レイアウトのサイズ
(defun size-row-layout (obj)
  (reduce #'(lambda (siz siz-elm)
			  (incf (geo:size-w siz) (geo:size-w siz-elm))
			  (maxf (geo:size-h siz) (geo:size-h siz-elm))
			  siz
			  ) ;lambda
		  (mapcar #'size-element (row-layout-lis-elm obj))
		  :initial-value (geo:create-size 0 0)
		  ) ;reduce
  )

;;; 内部要素の配置（各要素のスロット rect 値の決定）
;;; レイアウト（第１引数）の rect は設定済みであること
;;; 内部要素を縦に並べる
;;; @param obj row レイアウト（構造体 row-layout）
;;; @return 
(defun arrange-row-layout (obj)
  (let* ((valign (row-layout-valign obj))
		 (spread (row-layout-spread obj))
		 (rct (element-rect obj))
		 (x      (geo:rect-x rct))
		 (top    (geo:rect-t rct))
		 (bottom (geo:rect-b rct))
		 (lis-elm (row-layout-lis-elm obj))
		 (lis-elm-siz (mapcar #'size-element lis-elm))
		 (sum-w (reduce #'+ (mapcar #'geo:size-w lis-elm-siz)
						:initial-value 0
						) ;reduce
				)
		 ;; 内部要素を水平方向に拡大する比率
		 (ratio (if spread (/ (geo:rect-w rct) sum-w) 1))
		 )
	;; 各内部要素の位置・サイズ（スロット rect）をセット
	(map nil #'(lambda (elm elm-siz
						&aux
						(elm-w-new (* ratio (geo:size-w elm-siz))) ;layout 幅全体を使うように、各要素の幅を拡大
						(elm-h (geo:size-h elm-siz))
						)
				 (setf (element-rect elm) (geo:create-rect x
														   (case valign
															 (:center (round (- (+ top bottom) elm-h) 2))
															 (:bottom (- bottom elm-h))
															 (t top)
															 ) ;case
														   elm-w-new
														   elm-h
														   ) ;geo:create-rect
					   ) ;setf

				 (incf x elm-w-new)
				 ) ;lambda
		 lis-elm
		 lis-elm-siz
		 ) ;map

	;; 内部要素にレイアウトがある場合、そのレイアウトの内部配置
	(map nil #'arrange-layout (remove-if-not #'layout-p lis-elm))
	) ;let*
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; ■edge レイアウト
;;;; 
;;;; 内部要素を辺（上下左右）に沿って配置する。
;;;; （位置 :center は残りの部分。最大１回のみ指定可能）
;;;; 

;;; edge レイアウト
(defstruct (edge-layout
			(:include layout)
			)
  (lis-edge nil)  ;位置指定（:left/:top/:right/:bottom/:center）のみ集めたリスト
  )

;;; edge レイアウトのコンストラクタ
;;; 次のチェックをする。
;;; 	位置指定 :center の指定回数が１回以下か
;;; 	位置指定 :center が現れる場合、その箇所は最後か
;;; （例）
;;; (create-edge-layout (list :left (create- ... )
;;;                           :right (create- ...)
;;;                           :center (create- ...)
;;;                      )
;;;  )
;;; @param lis 位置指定と要素をプロパティリストのように交互したリスト
;;; 	位置指定は :left/:top/:right/:bottom/:center のいずれか
;;; 	位置指定 :center は最大１回。指定可能箇所は最後のみ
;;; @return 構造体 edge-layout
(defun create-edge-layout (lis)
  (let* ((lis-edge nil)
		 (lis-elm nil)
		 )
	(do ((rem lis (cddr rem))
		 (domain '(:top :left :right :bottom :center)) ;指定可能な位置（制約）
		 )
		((null rem) (list lis-edge lis-elm))
	  (unless (member (car rem) domain)
		(error "制約が不適切。：~a" (car rem))
		) ;unless

	  ;; 位置指定（制約）と要素を分離して格納
	  (push (car rem) lis-edge)
	  (push (cadr rem) lis-elm)
	  ) ;do

	;; 位置指定（制約）と要素の順番が逆転しているので戻す
	(setf lis-edge (reverse lis-edge)
		  lis-elm (reverse lis-elm)
		  ) ;setf

	;; :center が存在する場合、最後かどうかチェック
	(when (cdr (member :center lis-edge))
	  (error "制約 :center は最後にのみ設定可能。")
	  )

	(make-edge-layout :lis-elm lis-elm
					  :lis-edge lis-edge
					  ) ;make-edge-layout
	) ;let*
  )

;;; 内部要素を含むサイズ
;;; @param obj edge レイアウト（構造体 edge-layout）
;;; @return レイアウトのサイズ
(defun size-edge-layout (obj)
  (let* ((w 0)
		 (h 0)
		 )
	(map nil #'(lambda (edge elm &aux (siz (size-element elm)))
				 (case edge
				   ((:top :bottom)
					(maxf w (geo:size-w siz))
					(incf h (geo:size-h siz))
					)
				   ((:left :right)
					(incf w (geo:size-w siz))
					(maxf h (geo:size-h siz))
					)
				   (:center
					(incf w (geo:size-w siz))
					(incf h (geo:size-h siz))
					)
				   ) ;case
				 ) ;lambda
		 ;; ※矩形を足していくようにサイズを求めるので逆順にする
		 (reverse (edge-layout-lis-edge obj))
		 (reverse (edge-layout-lis-elm obj))
		 ) ;map

	(geo:create-size w h)
	) ;let*
  )

;;; 内部要素の配置（各要素のスロット rect 値の決定）
;;; レイアウト（第１引数）の rect は設定済みであること
;;; 内部要素を矩形の辺に沿って配置
;;; @param obj edge レイアウト（構造体 edge-layout）
;;; @return 
(defun arrange-edge-layout (obj)
  (let* ((rct-rem (geo:copy-rect* (element-rect obj)))
		 (lis-elm (edge-layout-lis-elm obj))
		 rct-elm
		 siz-elm
		 )
	;; 各内部要素の位置・サイズ（スロット rect）をセット
	(map nil #'(lambda (edge elm)
				 (case edge
				   (:center
					(setf rct-elm rct-rem
						  rct-rem nil
						  ) ;setf
					)
				   ((:top :left :right :bottom)
					(setf siz-elm (size-element elm))
					(multiple-value-setq (rct-rem rct-elm)
					  (case edge
						((:top :bottom) (geo:cut-rect edge (geo:size-h siz-elm) rct-rem))
						((:left :right) (geo:cut-rect edge (geo:size-w siz-elm) rct-rem))
						) ;case
					  ) ;multiple-value-setq
					)
				   ) ;case

				 ;; 要素の位置・サイズ（スロット rect）をセット
				 (setf (element-rect elm) rct-elm)
				 ) ;lambda
		 (edge-layout-lis-edge obj) ;位置指定（制約）
		 lis-elm                    ;内部要素
		 ) ;map

	;; 内部要素にレイアウトがある場合、そのレイアウトの内部配置
	(map nil #'arrange-layout (remove-if-not #'layout-p lis-elm))
	) ;let*
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; ■総称関数
;;;; 

;;; 内部要素を含むサイズ
;;; @param obj レイアウト（構造体 layout を継承した構造体）
;;; @return レイアウトのサイズ
(defun size-layout (obj)
  (let* (siz
		 )
	(setf siz (cond
			   ((column-layout-p obj) (size-column-layout obj))
			   ((row-layout-p obj) (size-row-layout obj))
			   ((edge-layout-p obj) (size-edge-layout obj))
			   ) ;cond
		  ) ;setf
	;; オプションによるサイズ調整
	(adjust-size-by-option siz (element-option obj))
	siz
	) ;let*
  )

;;; 要素の矩形サイズ（マージンとパディングを含む）
;;; @param obj 要素（コントロールあるいはレイアウト）
;;; 	構造体 ctrl あるいは 構造体 layout を継承した構造体
;;; @return 要素のサイズ
(defun size-element (obj)
  ;; xyzzy では typep で構造体を判定できない
  (cond
   ((ctrl-p obj) (size-ctrl obj))
   ((layout-p obj) (size-layout obj))
   ) ;cond
  )

;;; レイアウト内部要素の配置（各要素のスロット rect 値の決定）
;;; レイアウト（第１引数）の rect は設定済みであること
;;; 内部要素を矩形の辺に沿って配置
;;; @param obj レイアウト（構造体 layout を継承した構造体）
;;; @return 
(defun arrange-layout (obj)
  ;; xyzzy では typep で構造体を判定できない
  (cond
   ((column-layout-p obj) (arrange-column-layout obj))
   ((row-layout-p obj) (arrange-row-layout obj))
   ((edge-layout-p obj) (arrange-edge-layout obj))
   ) ;cond
  )

;;; レイアウト内部要素に対応するコントロールを配置したダイアログを表示
;;; @param siz ダイアログのサイズ
;;; 	（※レイアウトの必要サイズの方が大きければ、そちらが用いられる）
;;; @param lay レイアウト
;;; @param caption キャプション
;;; @param &key font フォント名
;;; @param &key font-size フォントサイズ
;;; @param &key initial-value コントロールの初期値
;;; @param &key check-or-disable コントロール制御のハンドラ
;;; @return 
(defun display-dialog (siz lay caption &key (font "MS UI Gothic") (font-size 10)
						   initial-value check-or-disable)
  (let* ((siz-lay (size-layout lay))
		 (rct (geo:size-rect (geo:max-size siz-lay siz)))
		 )
	(setf (element-rect lay) rct)
	(arrange-layout lay)

	(multiple-value-bind (result alist)
		(dialog-box 
		 `(dialog 0 0 ,(geo:rect-w rct) ,(geo:rect-h rct)
				  (:caption ,caption)
				  (:font ,font-size ,font)
				  (:control
				   ,@(description-layout lay)
				   ) ;:control
				  ) ;dialog
		 initial-value
		 check-or-disable
		 )
	  (list result alist)
	  ) ;multiple-value-bind
	) ;let*
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; ■サンプルコード
;;;; 

;;; レイアウトサンプル
(defparameter *sample-layout*
  (create-edge-layout
   (list :top (create-row-layout (list (create-label "ラベル１")
									   (create-label "ラベル２")
									   ) ;list
								 :valign :center
								 :spread t
								 ) ;create-row-layout
		 :bottom (create-row-layout (list (create-check :chk1 "チェック１")
										  (create-check3 :chk2 "チェック２")
										  ) ;list
									) ;create-row-layout
		 :left (create-column-layout (list (create-edit :edt1)
										   (create-listbox :lst1)
										   (create-button :btn1 "ボタン１" :option '(:width 80 :height 20))
										   (create-button :btn2 "ボタン２")
										   ) ;list
									 ) ;create-column-layout
		 :right (create-column-layout (list (create-combobox :cmb1 :option '(:width 80))
											(create-button :btn3 "ボタン３")
											(create-button :btn4 "ボタン４")
											) ;list
									  :align :right
									  ) ;create-column-layout
		 ) ;list
   ) ;create-edge-layout
  ) ;defparameter

;;; サンプルレイアウトをダイアログ表示
(defun sample-dialog ()
  (display-dialog nil *sample-layout* "タイトル"
				  :initial-value '((:edt1 . "12345")
								   (:cmb1 . ("コンボ１" "コンボ２"))
								   (:lst1 . ("リスト１" "リスト２" "リスト３"))
								   )
				  )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; ■関数 dialog-box の説明
;;;; 
;;;; (dialog-box TEMPLATE INITIAL-VALUE CHECK-OR-DISABLE)
;;;; 
;;;; @param TEMPLATE 関数 dialog で作成するダイアログ／コントロール配置
;;;; @param INITIAL-VALUE 各コントロールの初期値。a-list
;;;; @param CHECK-OR-DISABLE コントロール制御
;;;; 	●例
;;;; 	(c1 :disable (one text-a))
;;;; 			コントロール c1 が ON なら、コントロール one text-a を不活性に
;;;; 
;;;; 	(text-a :non-null "何か入力して下さい" :enable (one))
;;;; 			コントロール text-a が未入力（空文字列）ならダイアログメッセージを表示
;;;; 			コントロール text-a に何か入力されたら、コントロール one を活性化
;;;; 
;;;; 	(link :url "mailto:aaa@bbb.ccc")
;;;; 			コントロール link（ static かつ caption 文字列中 \001 と \002 で括られた部分あり）
;;;; 			をクリックされたとき、指定 URL を開く
;;;; 			URL が "mailto:" で始まるときは、メール送信となる
;;;; 
;;;; 	(line :enable (IDOK) :type integer :min 1 :non-null t
;;;; 		:type-error "行番号を入力してしださい"
;;;; 		:range-error "1以上を指定してください")
;;;; 			入力値が数値でない場合はダイアログメッセージ表示
;;;; 			入力値が 1 以上でなければダイアログメッセージ表示
;;;; 
;;;; 	(ref :related cmd
;;;; 		 :file-name-dialog (:title "参照"
;;;; 							:filter (("実行ﾌｧｲﾙ(*.exe;*.com;*.cmd;*.bat;*.pif)" . "*.exe;*.com;*.cmd;*.bat;*.pif")
;;;; 									 ("すべてのﾌｧｲﾙ(*.*)" . "*.*"))
;;;; 							:must-exist t))
;;;; 			ボタン ref を押されたらファイルダイアログを表示し、ファイルダイアログで
;;;; 			選択されたファイルのパスをコントロールcmd へセット
;;;; 
;;;; 
;;;; ●関数 dialog-box の例
;;;; 
;;;; (multiple-value-bind (result alist)
;;;; 	(dialog-box
;;;; 	 `(dialog 0 0 232 76
;;;; 			  (:caption "てすと")
;;;; 			  (:font 9 "ＭＳ Ｐゴシック")
;;;; 			  (:control
;;;; 			   (:button backward "上検索(&P)" #x10010000 176 7 52 14)
;;;; 			   (:edit line nil ,(logior +WS-CHILD+ +WS-VISIBLE+
;;;; 										+WS-BORDER+ +BS-BUTTON+ +ES-AUTOHSCROLL+) 49 12 40 14)
;;;; 			   (:button IDCANCEL "ｷｬﾝｾﾙ" #x50030000 176 44 52 14))
;;;; 	  )
;;;; 	 nil  ;INITIAL-VALUE 各コントロールの初期値
;;;; 	 nil  ;CHECK-OR-DISABLE コントロール制御
;;;; 	)
;;;; )
