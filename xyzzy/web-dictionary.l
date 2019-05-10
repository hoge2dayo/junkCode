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
;;;; �_�C�A���O
;;;; 

;;; �_�C�A���O�̃��C�A�E�g
(defparameter *ej-dic-layout*
  (dlg:create-edge-layout
   (list :top (dlg:create-row-layout
			   (list (dlg:create-label "�p�a�E�a�p���T�F" :option '(:width 70))
					 (dlg:create-text :word :option '(:width 100))
					 ) ;list
			   :valign :center
			   :spread t
			   ) ;create-row-layout
		 :bottom (dlg:create-row-layout
				  (list (dlg:create-button 'IDOK "�n�j(&O)" :option '(:width 60))
						(dlg:create-button :cancel "�L�����Z��" :option '(:width 60))
						) ;list
				  ) ;create-row-layout
		 ) ;list
   ) ;create-edge-layout
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; �p�a���T���邢�� google �|����J��
;;;; 

;;; �p�a�a�p���T�����p URL ���쐬�igoo �����j
;;; @param s ����������
;;; @return ����������ɃX�y�[�X����s�����Ȃ��ꍇ�� nil
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

;;; �p�a�|��p URL ���쐬�igoogle �|��j
;;; @param s ����������
;;; @return ����������ɃX�y�[�X����s�����Ȃ��ꍇ�� nil
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

;;; �������[�h�Ƃ��ėL����
;;; @param s
;;; @return 
(defun valid-search-word-p (s)
  (and s (< 0 (length (string-trim " \t\n" s))))
  )

;;; �_�C�A���O��\�����A���͂��ꂽ�P��ŉp�a�E�a�p���T������
;;; @param &optional s �������[�h�̏����l
;;; @return 
(defun ej-dic-dialog (&optional (s ""))
  (let* ((r (dlg:display-dialog nil *ej-dic-layout* "�������[�h����"
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
		(msgbox "�������[�h�������ł��B")
		) ;if
	  ) ;when
	) ;let*
  )

;;; ������������ň����ׂ����|�󂷂ׂ���
;;; @param s
;;; @return
;;; 	�����̏ꍇ�F :dic
;;; 	�|��̏ꍇ�F :trans
;;; 	���̑��F nil
(defun dic-or-trans (s)
  (setf s (and s (string-trim " \t\n" s)))
  (let* ((n (count-if #'(lambda (ch) (find ch " \t\n")) s))
		 )
	(when (< 0 (length s))
	  (if (zerop n) :dic :trans)
	  ) ;when
	) ;let*
  )

;;; selection ���p�a���T�ň���
;;; ���邢�͖|��y�[�W���J��
(defun open-dic-or-trans ()
  (interactive)

  ;; selection ���Ȃ��ꍇ�A�J�[�\���ߕӂ̒P���I��
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
		   (s (and (find type '(1 2)) ;�s�I���y�ѕ����I���̂݁i��`�I���͏��O�j
				   (buffer-substring start end)
				   )
			  )
		   )
	  ;;; HTML �^�O���폜
	  (setf s (uregexp:remove-match-string uregexp:*html-tag* s))
	  ;;; &lt; �� <
	  (setf s (uregexp:replace-match-string "<" "&lt;" s))
	  ;;; &gt; �� >
	  (setf s (uregexp:replace-match-string ">" "&gt;" s))
	  ;;; &apos; �� '
	  (setf s (uregexp:replace-match-string "'" "&apos;" s))
	  ;;; &quot; �� "
	  (setf s (uregexp:replace-match-string "\"" "&quot;" s))
	  ;;; &amp; �� &
	  (setf s (uregexp:replace-match-string "&" "&amp;" s))
	  ;;; �󔒂̘A�����A���p�X�y�[�X��ɕϊ�
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
;;;; �L�[�ݒ�
;;;; 

;;; 
(global-set-key '(#\ESC #\o) 'open-dic-or-trans)
