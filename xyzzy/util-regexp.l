;;;; require - provide - defpackage - in-package - export

;;; 
;(require )
;;; 
(provide :util-regexp)
;;; 
(defpackage :util-regexp (:use :lisp :editor)
  (:nicknames :uregexp)
  )
;;; 
(in-package :util-regexp)

;;; 
(export '(*html-tag*
		  combine-regexp          ;���K�\����v�f�Ɋ܂ރ��X�g���P�̐��K�\��������ɂ���
		  replace-match-string    ;�����񒆂̐��K�\���ɍ��v����ӏ���u��������
		  remove-match-string     ;�����񒆂̐��K�\���ɍ��v����ӏ����폜
		  )
		)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; 
;;;; 

;;; ���K�\����v�f�Ɋ܂ރ��X�g���A�P�̐��K�\��������ɂ���
;;; @param e
;;; 	or �Ŏn�܂郊�X�g�́A\(?:�`\)\|\(?:�`\) �ƂȂ�B
;;; @return ���K�\��������
(defun combine-regexp (e)
  (cond
   ((null e) "")
   ((stringp e) e)
   ((listp e) (case (car e)
				(or (format nil "\\(?:~{\\(?:~a\\)~^\\|~}\\)"
							(mapcar #'combine-regexp (cdr e))
							) ;format
				 )
				(t (format nil "\\(?:~{~a~}\\)" (mapcar #'combine-regexp e)))
				) ;case
	)
   ((keywordp e) (symbol-name e))
   ((symbolp e) (case e
				  ((+ * ?) (symbol-name e))
				  (t (symbol-value e))
				  ) ;case
	)
   (t "")
   ) ;cond
  )

;;; �󔒁i�P�����j
(defparameter *ws* "[ \t\n]")
;;; �^�O���^������
(defparameter *name* "[^ \t\n<>=\"']+")
;;; �l�i���̂P�j
(defparameter *value-1* "[^ \t\n<>=\"']*")
;;; �l�i���̂Q�j
(defparameter *value-2* "\"[^\"]*\"")
;;; �������ƒl
(defparameter *name-value*
  (combine-regexp '(*name* ("=" (or *value-1* *value-2*)) ?))
  )
;;; HTML �^�O
(defparameter *html-tag*
  (combine-regexp '("</?" *name*
					(*ws* + *name-value*) *
					*ws* * "/?>")
				  )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; 
;;;; 
;;;; 

;;; �����񒆂��琳�K�\�����������A���v�����ӏ���u��������B
;;; �i���̕�����͕ς��Ȃ��j
;;; @param rpl �u��������̕�����B���v���������� \1 - \9 ���܂߂邱�Ƃ��\
;;; @param regexp ���K�\��
;;; @param s �����敶����
;;; @param &key case-fold �^�̏ꍇ�A�啶������������ʂ��Ȃ�
;;; 	���K�\�����R���p�C���ς݂̏ꍇ�A���Ӗ�
;;; @return 
(defun replace-match-string (rpl regexp s &key (case-fold nil))
  (do* ((len (length s))
		(fnc-match (if case-fold #'string-matchp #'string-match))
		(lis nil)
		(start 0 (match-end 0))
		)
	  ((<= len start) (format nil "~{~a~}" (reverse lis)))
	(cond
	 ((funcall fnc-match regexp s start)
	  (push (subseq s start (match-beginning 0)) lis)
	  (push (string-replace-match s rpl) lis)
	  )
	 (t (push (subseq s start) lis)
		(return (format nil "~{~a~}" (reverse lis)))
	  )
	 ) ;cond
	) ;do*
  )

#|
;;; �����񒆂��琳�K�\�����������A���v�����ӏ����폜
;;; �i���̕�����͕ς��Ȃ��j
;;; @param regexp ���K�\��
;;; @param s �����敶����
;;; @param &key case-fold �^�̏ꍇ�A�啶������������ʂ��Ȃ�
;;; 	���K�\�����R���p�C���ς݂̏ꍇ�A���Ӗ�
;;; @return 
(defun remove-match-string (regexp s &key (case-fold nil))
  (do* ((len (length s))
		(fnc-match (if case-fold #'string-matchp #'string-match))
		(lis nil)
		(start 0 (match-end 0))
		)
	  ((<= len start) (format nil "~{~a~}" (reverse lis)))
	(cond
	 ((funcall fnc-match regexp s start) (push (subseq s start (match-beginning 0)) lis)
	  )
	 (t (push (subseq s start) lis)
		(return (format nil "~{~a~}" (reverse lis)))
		)
	 ) ;cond
	) ;do*
  )
|#

;;; �����񒆂��琳�K�\�����������A���v�����ӏ����폜
;;; �i���̕�����͕ς��Ȃ��j
;;; @param regexp ���K�\��
;;; @param s �����敶����
;;; @param &key case-fold �^�̏ꍇ�A�啶������������ʂ��Ȃ�
;;; 	���K�\�����R���p�C���ς݂̏ꍇ�A���Ӗ�
;;; @return 
(defun remove-match-string (regexp s &key (case-fold nil))
  (replace-match-string "" regexp s :case-fold case-fold)
  )
