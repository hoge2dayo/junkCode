;;;; require - provide - defpackage - in-package - export

;;; 
(require :util-editor)
(require :util-encode)
(require :util-regexp)
(require :dlg)
;;; 
(provide :web-dictionary)
;;; 
(defpackage :web-dictionary
  (:use :lisp :editor)
  (:nicknames :webdic)
  )
;;; 
(in-package :web-dictionary)

;;; 
(export '(ej-dic-dialog
		  open-dic-or-trans
		  )
		)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; ダイアログ
;;;; 

;;; ダイアログのレイアウト
(defparameter *ej-dic-layout*
  (dlg:create-edge-layout
   (list :top (dlg:create-row-layout
			   (list (dlg:create-label "英和・和英辞典：" :option '(:width 70))
					 (dlg:create-text :word :option '(:width 100))
					 ) ;list
			   :valign :center
			   :spread t
			   ) ;create-row-layout
		 :bottom (dlg:create-row-layout
				  (list (dlg:create-button 'IDOK "ＯＫ(&O)" :option '(:width 60))
						(dlg:create-button :cancel "キャンセル" :option '(:width 60))
						) ;list
				  ) ;create-row-layout
		 ) ;list
   ) ;create-edge-layout
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; 英和辞典あるいは google 翻訳を開く
;;;; 

;;; 英和和英辞典検索用 URL を作成（goo 辞書）
;;; @param s 検索文字列
;;; @return 検索文字列にスペースや改行しかない場合は nil
(defun url-ej-dic (s)
  (let* ((pre-url "https://dictionary.goo.ne.jp/freewordsearcher.html?MT=")
		 (search-word (and s (string-trim " \t\n" s)))
		 (post-url "&mode=0&kind=en")
		 )
	(when search-word
	  (uenc:url-encode (format nil "~a~a~a" pre-url search-word post-url))
	  ) ;when
	) ;let*
  )

;;; 英和翻訳用 URL を作成（google 翻訳）
;;; @param s 検索文字列
;;; @return 検索文字列にスペースや改行しかない場合は nil
(defun url-ej-trans (s)
  (let* ((pre-url "https://translate.google.co.jp/?sl=en&tl=ja&text=")
		 (search-word (and s (string-trim " \t\n" s)))
		 (post-url "")
		 )
	(when search-word
	  (uenc:url-encode (format nil "~a~a~a" pre-url search-word post-url))
	  ) ;when
	) ;let*
  )

;;; 検索ワードとして有効か
;;; @param s
;;; @return 
(defun valid-search-word-p (s)
  (and s (< 0 (length (string-trim " \t\n" s))))
  )

;;; ダイアログを表示し、入力された単語で英和・和英辞典を検索
;;; @param &optional s 検索ワードの初期値
;;; @return 
(defun ej-dic-dialog (&optional (s ""))
  (let* ((r (dlg:display-dialog nil *ej-dic-layout* "検索ワード入力"
								:font "HGSGothicM"
								:font-size 12
								:initial-value (acons :word s nil)
								:check-or-disable '((:word :enable (IDOK) :non-null t)
													)
								)
			)
		 (s (when (eql (car r) 'IDOK)
			  (cdr (assoc :word (cadr r)))
			  ) ;when
			)
		 url
		 )
	(when s
	  (if (valid-search-word-p s)
		  (shell-execute (url-ej-dic s) t)
		(msgbox "検索ワードが無効です。")
		) ;if
	  ) ;when
	) ;let*
  )

;;; 文字列を辞書で引くべきか翻訳すべきか
;;; @param s
;;; @return
;;; 	辞書の場合： :dic
;;; 	翻訳の場合： :trans
;;; 	その他： nil
(defun dic-or-trans (s)
  (setf s (and s (string-trim " \t\n" s)))
  (let* ((n (count-if #'(lambda (ch) (find ch " \t\n")) s))
		 )
	(when (< 0 (length s))
	  (if (zerop n) :dic :trans)
	  ) ;when
	) ;let*
  )

;;; selection を英和辞典で引く
;;; あるいは翻訳ページを開く
(defun open-dic-or-trans ()
  (interactive)

  ;; selection がない場合、カーソル近辺の単語を選択
  (unless (uedit:selection-info)
	(let* ((range (uedit:find-word-near-point))
		   )
	  (uedit:set-selection 2 (car range) (cadr range))
	  ) ;let*
	) ;unless

  (uedit:restore-selection
	(let* ((info (uedit:selection-info))
		   (type (car info))
		   (start (cadr info))
		   (end (caddr info))
		   (s (and (find type '(1 2)) ;行選択及び文字選択のみ（矩形選択は除外）
				   (buffer-substring start end)
				   )
			  )
		   )
	  ;;; HTML タグを削除
	  (setf s (uregexp:remove-match-string uregexp:*html-tag* s))
	  ;;; &lt; ⇒ <
	  (setf s (uregexp:replace-match-string "<" "&lt;" s))
	  ;;; &gt; ⇒ >
	  (setf s (uregexp:replace-match-string ">" "&gt;" s))
	  ;;; &apos; ⇒ '
	  (setf s (uregexp:replace-match-string "'" "&apos;" s))
	  ;;; &quot; ⇒ "
	  (setf s (uregexp:replace-match-string "\"" "&quot;" s))
	  ;;; &amp; ⇒ &
	  (setf s (uregexp:replace-match-string "&" "&amp;" s))
	  ;;; 空白の連続を、半角スペース一個に変換
	  (setf s (uregexp:replace-match-string " " "[ \t\n]+" s))

	  (case (dic-or-trans s)
		(:dic (shell-execute (url-ej-dic s) t))
		(:trans (shell-execute (url-ej-trans s) t))
		) ;case
	  ) ;let*
	) ;restore-selection
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; キー設定
;;;; 

;;; 
(global-set-key '(#\ESC #\o) 'open-dic-or-trans)

