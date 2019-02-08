;;;; require - provide - defpackage - in-package - export

;;(require )

(provide :my-menu)

(defpackage :my-menu (:use :lisp :editor))

(in-package :my-menu)

(export '(
		  )
		) ;export

;;;; 
;;;; �����j���[�o�[�ɁmLisp(L)�n�ǉ�
;;;; 
;;;; �����ڂƓ���
;;;; 
;;;; �m�o�b�t�@��]��(B)�n
;;;; �@ �o�b�t�@�S�̂�]���ieval-buffer �Ɠ����j
;;;; 
;;;; �m�Ō�̂r����]��(&L)�n
;;;; �@ �J�[�\�����O�̂r����]���ieval-last-sexp �Ɠ����j
;;;; 
;;;; �m���[�W�������͑I��͈͂�]��(&R)�n
;;;; �@ ���[�W�����܂��� SELECTION �̂r����]��
;;;; 

;;; ���݂̃o�b�t�@�ɑ΂��� eval-buffer �����s
;;; @return 
(defun eval-selected-buffer ()
  (interactive)

  (eval-buffer (selected-buffer))
  )

;;; �I���iselection�j��]��
;;; @return �]�������ꍇ�iselection���������ꍇ�jt
(defun eval-selection ()
  (interactive)

  (case (get-selection-type)
	((1 2)  ;�s���[�h�I���A�������[�h�I��
	 (setf mrk (selection-mark)
		   pnt (selection-point)
		   ) ;setf
	 (when (< pnt mrk) (rotatef mrk pnt))
	 (eval-region mrk pnt)
	 t
	 )
	(t nil)  ;�I���Ȃ��A��`�I��
	) ;case
  )

;;; �I���i selection �j�܂��̓��[�W������]��
;;; @return 
(defun eval-selection-or-region ()
  (interactive)
  (or (eval-selection)
	  (let ((mrk (mark))
			(pnt (point))
			)
		(when (< pnt mrk) (rotatef mrk pnt))
		(eval-region mrk pnt)
		) ;let
	  t
	  ) ;or
  )

;;; ���j���[�̃��[�g���ڐ����擾
;;; @param &optional menu
;;; @param &optional limit
;;; @return 
(defun get-menu-count (&optional (menu *app-menu*) (limit 20))
  (dotimes (i limit)
	(unless (get-menu menu i t)
	  (return i)
	  )
	) ;dotimes
  )


;;; �ǉ����郁�j���[
(defparameter *custom-menu-lisp*
  (define-popup-menu
	(:item nil "�o�b�t�@��]��(&B)" 'eval-selected-buffer)
	:sep
	(:item nil "�Ō�̂r����]��(&L)" 'eval-last-sexp)
	(:item nil "���[�W�������͑I��͈͂�]��(&R)" 'eval-selection-or-region)
	)
  )

;;; �E�B���h�E���j���[�ɒǉ�
;;; �Ōォ��Q�Ԗڂɒǉ��i�Ō�́m�w���v(?)�n�j
(add-hook '*init-app-menus-hook*
		  #'(lambda () (insert-popup-menu *app-menu* (1- (get-menu-count))
										  *custom-menu-lisp* "Lisp(&L)"
										  ) ;insert-popup-menu
			  ) ;lambda
		  )

