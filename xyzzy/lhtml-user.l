;;;; require - provide - defpackage - in-package - export

;(require )

(provide :lhtml-user)

;;; �� :lhtml �� use ���Ȃ�
(defpackage :lhtml-user (:use :lisp :editor))

(in-package :lhtml-user)

(export '(print-parsed-lhtml
		  print-main-header
		  print-main-footer
		  def-lhtml
		  *register-info*
		  get-info
		  clear-info
		  *request*
		  )
		)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; ��html �t�@�C���o�͂��s���p�b�P�[�W
;;;; �@lhtml �t�@�C���ɋL�ڂ��������́A���̃p�b�P�[�W���Ŏ��s�����B
;;;; 

;;; def-lhtml �Œ�`�����֐��̖��O
;;; lobj �t�@�C���̃��[�h�E�]�����ɃZ�b�g
(defparameter *register-info* nil)

;;; *register-info* ���擾
(defun get-info ()
  *register-info*
  )

;;; *register-info* ���N���A
(defun clear-info ()
  (setf *register-info* nil)
  )

;;; �֐��ďo���ɒl��n���ׂ̕ϐ�
(defparameter *request* nil)

;;; *request* �� p-list ���Z�b�g����Ă���O��Œl�����o��
;;; @param p �v���p�e�B
;;; @param &optional val-if-nil �l�� nil �̏ꍇ�ɗp����l
(defmacro req (p &optional (val-if-nil nil)) `(or (getf *request* ,p) ,val-if-nil))

;;; html �t�@�C���֏o��
;;; �i���_�C���N�g����̂ŁA�W���o�͂ւ̏o�͂ł悢�j
;;; @param e
(defmacro out (e) `(princ ,e))

;;; �֐� parse-lhtml �œ������ʂ̂P�Z�N�V������ Lisp �R�[�h�Ƃ��ďo��
;;; @param section �Z�N�V�����i p-list �̃v���p�e�B�j
;;; @param parsed-lhtml �֐� parse-lhtml �œ�������
;;; @return 
(defun print-parsed-lhtml-section (section parsed-lhtml)
  ;; �J�����g�p�b�P�[�W��ύX���ďo��
  (let* ((lis (getf parsed-lhtml section))
		 )
	(dolist (e lis)
	  (cond
	   ;; ������
	   ((stringp e) (format t "(~s ~s)~%" 'out e))
	   ;; ������ȊO
	   (t (case (car e)
			(:sexp (format t "(~s ~a)~%" 'out (cadr e)) ;(cadr e) �͂r����������������
			 )
			(:block (format t "~a~%" (cadr e))
			 )
			(:comment
			 )
			(:require (format t "~s~%" `(require ,(cadr e)))
			 )
			(:use-package (format t "~s~%" `(use-package ,(cadr e)))
			 )
			(t
			 )
			) ;case
		  )
	   ) ;cond
	  ) ;dolist
	) ;let*
  )

;;; �e�s�ɕ����ċL�q������������P�ɂ���
;;; @param &rest lis-str �e�v�f�͕�����
(defmacro string-lines (&rest lis-str)
  (format nil "~{~a~%~}" lis-str)
  )

;;; use-package �O�ɏo�͂�����e
(defparameter *main-header-fmt-1*
  (string-lines ""
				"(require :lhtml-user)"
				"(in-package :lhtml-user)"
				) ;string-lines
  )

;;; ���C���������O�ɏo�͂�����e
(defparameter *main-header-fmt-2*
  (string-lines ""
				"(def-lhtml ~a (request)"
				"  (let* ((*request* (copy-list request)) ;�l��ύX����\���ɔ����ĕ���"
				"         )"
				";;;;----> ���C������"
				) ;string-lines
  )

;;; ���C����������ɏo�͂�����e
(defparameter *main-footer-fmt*
  (string-lines ""
				";;;;<---- ���C������"
				"   ) ;let*"
				" ) ;def-lhtml"
				) ;string-lines
  )

;;; �֐� parse-lhtml �œ������ʂ� Lisp �R�[�h�Ƃ��ďo��
;;; �W���o�͂֏o�͂���̂ŁA�K�v�ɉ����Ď��O�Ƀ��_�C���N�g���Ă�������
;;; @param parsed-lhtml �֐� parse-lhtml �œ�������
;;; @param func-name ���C���֐��̖��O
;;; @return 
(defun print-parsed-lhtml (parsed-lhtml func-name)
  (let* ((*package* (in-package :lhtml-user))
		 )
	;; ---- require
	(print-parsed-lhtml-section :require parsed-lhtml)
	;; ---- in-package
	(format t *main-header-fmt-1* func-name)
	;; ---- use-package
	(print-parsed-lhtml-section :use-package parsed-lhtml)

	;; ---- ���C������
	;; �w�b�_
	(format t *main-header-fmt-2* func-name)
	;; ����
	(print-parsed-lhtml-section :main parsed-lhtml)
	;; �t�b�^
	(format t *main-footer-fmt*)
	) ;let*
  )

;;; �֐����`
;;; lobj �̎��� �֐� load �Ń��[�h�E�]�������ׁA���ږ߂�l��Ԃ��Ȃ��B
;;; �O���[�o���ϐ����g���āA�߂�l���ԐړI�ɖ߂�l��Ԃ��B
;;; @param name �֐����i�V���{���j
;;; @param lambda-list �����_���X�g
;;; @param &body body
(defmacro def-lhtml (name lambda-list &body body)
  `(progn
	 (defun ,name ,lambda-list
	   ,@body
	   ) ;defun

	 ;; �֐������Z�b�g
	 (push ',name *register-info*)

	 ',name
	) ;progn
  )
