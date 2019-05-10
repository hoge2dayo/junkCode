;;;; require - provide - defpackage - in-package - export

;;; 
;(require )
;;; 
(provide :util-editor)
;;; 
(defpackage :util-editor (:use :lisp :editor)
  (:nicknames :uedit)
  )
;;; 
(in-package :util-editor)

;;; 
(export '(define-key*            ;define-key �̕s�萔�o�^��
;		  with-selected-buffer

		  goto-point-min         ;�t�@�C���擪�ֈړ��� point-min ��Ԃ�
		  goto-point-max         ;�t�@�C�������ֈړ��� point-max ��Ԃ�
		  goto-bol-point         ;�s���ֈړ��� point ��Ԃ�
		  goto-eol-point         ;�s���ֈړ��� point ��Ԃ�
		  goto-line-number       ;�w��s�ֈړ�
		  line-number-point      ;�w��s�̍s������эs���� point ���擾
		  selection-info         ;selection �̏��
		  set-selection          ;selection ��ݒ�
		  restore-selection      ;������i�������ꂽ�j selection ��߂�
		  selection-or-region    ;selection �܂��� region �擾
		  find-word-near-point   ;�J�[�\���ߕӂɂ���P��̈ʒu���擾
		  buffer-line-string     ;�o�b�t�@�P�s���̕�����i�����̉��s�͊܂܂Ȃ��j���擾
		  delete-until-eol       ;�s���܂ō폜
		  do-buffer-line-string  ;�o�b�t�@�e�s�ł̃��[�v
		  match-line-list        ;�����p�^�[����������s�̃��X�g��Ԃ�
		  set-buffer*            ;�o�b�t�@�����w��\�� set-buffer
		  restore-buffer         ;�w�菈���I����A�I���o�b�t�@�����ɖ߂�
		  create-working-buffer  ;��Ɨp�o�b�t�@���쐬
		  with-working-buffer    ;�ꎞ�I�ɍ�Ɨp�o�b�t�@���쐬
		  )
		)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; �}�N��
;;;; 

;;; define-key �̕s�萔�o�^��
;;; @param keymap �L�[�}�b�v�Bmake-keymap�Ȃǂō쐬�B
;;; @param &rest key-cmd-pairs �L�[�ƃn���h���֐������݂Ɏw��
(defmacro define-key* (keymap &rest key-cmd-pairs)
  `(progn
	 ,@(do* ((remain key-cmd-pairs (cddr remain))
			 (result nil))
		   ((null (cdr remain)) (reverse result))
		 (push `(define-key ,keymap ,(car remain) ,(cadr remain)) result)
		 ) ;do*
	 ) ;progn
  )

#| output.l �Ŏg�p����Ă��邪�Aoutput.l ���̔p�~�����B
   ���Ȃ݂� with-set-buffer �Ƃ����}�N���� xyzzy �Ŏg���Ă���B
   �z������@�\�ł͂Ȃ��B
;;; �t�H�[�����s��A�I���o�b�t�@�����s�O�̃o�b�t�@�ɖ߂�
;;; @param &body body
(defmacro with-selected-buffer (&body body)
  (let* ((var-buf (gensym))
		 )
	`(let ((,var-buf (selected-buffer))
		   )
	   (unwind-protect
		   (progn
			 ,@body
			 )
		 ;; clean-up
		 (set-buffer ,var-buf)
		   ) ;unwind-protect
	   ) ;let
	) ;let*
  )
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; 
;;;; 

;;; �t�@�C���̐擪�i point-min �j�ֈړ����A point ��Ԃ�
;;; @return point-min
(defun goto-point-min (&aux (pnt (point-min)))
  (goto-char pnt)
  pnt
  )

;;; �t�@�C���̖����i point-max �j�ֈړ����A point ��Ԃ�
;;; @return point-max
(defun goto-point-max (&aux (pnt (point-max)))
  (goto-char pnt)
  pnt
  )

;;; �s���ֈړ��� point ��Ԃ�
;;; @return �s���� point
(defun goto-bol-point ()
  (goto-bol)
  (point)
  )

;;; �s���ֈړ��� point ��Ԃ�
;;; @return �s���� point
(defun goto-eol-point ()
  (goto-eol)
  (point)
  )

;;; �w��s�ֈړ�
;;; @param line �ړ���̍s
;;; @return �w��s�Ɉړ��ł��Ȃ������ꍇ�� nil
(defun goto-line-number (line)
  (goto-line line)
  (eql line (current-line-number))
  )

;;; �w��s�̍s������эs���� point ���擾
;;; @param ln �s
;;; @return �s������эs���� point �̃��X�g
;;; 	�w��s�Ɉړ��ł��Ȃ������ꍇ�� nil
(defun line-number-point (ln)
  (when (goto-line-number ln)
	(list (goto-bol-point) (goto-eol-point))
	) ;when
  )

;;; selection �̏��
;;; @param &optional sort-pnt �^�̏ꍇ�Apoint ���\�[�g����B
;;; @return selection �����݂��Ȃ��ꍇ nil�B
;;; 	selection �����݂���ꍇ�A( <selection �̎��> <point> <point> )
;;; 		<selection �̎��> �F1=�s�I�� / 2=�����I�� / ��`�I��
(defun selection-info (&optional (sort-pnt nil))
  (let* ((type (get-selection-type))
		 (mrk (and type (selection-mark)))
		 (pnt (and type (selection-point)))
		 )
	(when type
	  (when (and sort-pnt (< pnt mrk)) (rotatef mrk pnt))
	  (list type mrk pnt)
	  ) ;when
	) ;let*
  )

;;; selection ��ݒ肷��B
;;; @param type 1=�s�I�� / 2=�����I�� / 3=��`�I��
;;; 	����`�I���͋@�\���Ȃ�
;;; @param mrk
;;; @param pnt
;;; @return
(defun set-selection (type mrk pnt)
  (start-selection type t mrk)
  (goto-char pnt)
  )

;;; ������i�������ꂽ�j selection ��߂��B
;;; ��selection ����`�̏ꍇ�ɂ��܂����삵�Ȃ�
;;; @param &body body
(defmacro restore-selection (&body body)
  (let ((var-info (gensym))
		)
	`(let* ((,var-info (selection-info))
			)
	   (unwind-protect
		   (progn
			 ,@body
			 )
		 ;; �N���[���A�b�v
		 (when ,var-info
		   (set-selection (car ,var-info) (cadr ,var-info) (caddr ,var-info))
		   ) ;when
		 ) ;unwind-protect
	   ) ;let*
	) ;let
  )

;;; selection �܂��� region ���擾
;;; @param &optional sort-pnt �^�̏ꍇ�Apoint ���\�[�g����B
;;; @param &optional rect-selection selection �̋�`�I�����擾���邩
;;; @return
;;; 	selection �����݂���ꍇ�F(:selection <point> <point> <selection �̎��>)
;;; 		<selection �̎��> �F1=�s�I�� / 2=�����I�� / ��`�I��
;;; 	region �����݂���ꍇ�F(:region <point> <point>)
;;; 	���̑��Fnil
(defun selection-or-region (&optional (sort-pnt nil) (rect-selection nil))
  (or
   ;; selection
   (let* ((info (selection-info sort-pnt))
		  (type (car info))
		  )
	 (when (and type (find type (if rect-selection '(1 2 3) '(1 2))))
	   (list :selection (cadr info) (caddr info) (car info))
	   ) ;when
	 ) ;let*

   ;; region
   (let* ((mrk (mark))
		  (pnt (point))
		  )
	 (when mrk
	   (when (and sort-pnt (< pnt mrk)) (rotatef mrk pnt))
	   (list :region mrk pnt)
	   ) ;when
	 ) ;let*
   ) ;or
  )

;;; �J�[�\���ߕӂ̒P����擾
;;; @param &optional pnt �P���T���n�߂�ʒu�Bnil �̏ꍇ�A�J�[�\���ʒu�B
;;; @return �P��̊J�n�ʒu�ƏI���ʒu��v�f�Ƃ��郊�X�g�B
;;; 	�P�ꂪ������Ȃ������ꍇ�� nil
(defun find-word-near-point (&optional pnt)
  (let* (start
		 end
		 )
	(save-excursion
	  (when pnt
		(goto-char pnt)
		) ;when
	  (setf end (progn
				  (or (skip-syntax-spec-forward "w")
					  (skip-syntax-spec-backward "^w")
					  ) ;or
				  (point)
				  ) ;progn
			start (progn
					(skip-syntax-spec-backward "w")
					(point)
					) ;progn
			) ;setf
	  ) ;save-excursion
	(when (< start end)
	  (list start end)
	  ) ;when
	) ;let*
  )

;;; �o�b�t�@�P�s���̕�����i�����̉��s�͊܂܂Ȃ��j���擾
;;; @param &optional line �ړ���̍s
;;; @return �P�s���̕�����i�����̉��s�͊܂܂Ȃ��j
;;; 	�ړ���̍s�ֈړ��ł��Ȃ������ꍇ�� nil
(defun buffer-line-string (&optional (line nil))
  (when
	;; line �� nil �̏ꍇ�͉������Ȃ��i�`�F�b�N�Ȃ��j
	;; line �� ��nil �̏ꍇ�A���̍s�ւ̈ړ������݂�B���s����� nil
   (or
	(null line)  ;nil �̏ꍇ�A�s�ړ����Ȃ� ��or �𔲂���
	(goto-line-number line)
	) ;or

   (buffer-substring (goto-bol-point) (goto-eol-point))
   ) ;when
  )

;;; �s���܂ō폜
;;; @return 
(defun delete-until-eol ()
  (let* ((start (point))
		 (end (goto-eol-point))
		 )
	(delete-region start end)
	) ;let*
  )

;;; �o�b�t�@�e�s�ł̃��[�v
;;; �i��j(do-buffer-line-string (str-line 1 10) ...)
;;; @param var-str-line ���[�v�ϐ��B�o�b�t�@�̊e�s������ɑ��������
;;; @param &optional ln-start �J�n�s�Bnil�̏ꍇ���ݍs
;;; @param &optional ln-end �I���s�Bnil�܂��͏ȗ��̏ꍇ�o�b�t�@�ŏI�s
;;; @param &optional result-from ���[�v�I�����̃t�H�[���l
;;; @param &body body �e���[�v�Ŏ��s����t�H�[��
(defmacro do-buffer-line-string ((var-str-line &optional ln-start ln-end result-form) &body body)
  (let ((var-ln (gensym))
		(var-ln-end (gensym))
		)
	`(do ((,var-ln (or ,ln-start (current-line-number)) (1+ ,var-ln))
		  (,var-ln-end (or ,ln-end (buffer-lines)))
		  (,var-str-line)
		  )
		 ((< ,var-ln-end ,var-ln) ,result-form)
	   (setf ,var-str-line (buffer-line-string ,var-ln))
	   ,@body
	   ) ;do
	) ;let
  )

;;; �o�b�t�@���猟���p�^�[�������������s�̃��X�g���擾
;;; @param ptn �����p�^�[��
;;; @param &key regexp �����p�^�[�������K�\�����ǂ���
;;; @return �����p�^�[�������������s�̃��X�g
(defun match-line-list (ptn &key (regexp nil))
  (goto-char (point-min))

  (do* ((hit (scan-buffer ptn :regexp regexp :tail t)
			 (scan-buffer ptn :regexp regexp :tail t)
			 )
		(lis-ln nil)
		)
	  ((null hit) (reverse lis-ln))
	(push (current-line-number) lis-ln)
	(goto-eol)
	(forward-char)
	) ;do*
  )

;;; �o�b�t�@�����w��\�� set-buffer
;;; @param buf-or-name �o�b�t�@���邢�̓o�b�t�@��
;;; @return 
(defun set-buffer* (buf-or-name)
  (if (bufferp buf-or-name)
	  (set-buffer buf-or-name)
	(set-buffer (find-buffer buf-or-name))
	) ;if
  )

;;; �w�菈���I����A�I���o�b�t�@�����ɖ߂�
;;; ��with-set-buffer �Ƃ����}�N������肽���������A
;;; �@�����̃}�N���i����͖]�݂ƈقȂ�j�����ɂ���̂łł��Ȃ�
;;; @param &body body
(defmacro restore-buffer (&body body)
  (let ((var-buf (gensym))
		)
	`(let* ((,var-buf (selected-buffer))
			)
	   (unwind-protect
		   (progn ,@body)
		 (set-buffer ,var-buf)
		 ) ;unwind-protect
	   ) ;let*
	) ;let
  )

;;; ��Ɨp�o�b�t�@���쐬
;;; ���Ɏw�肵���o�b�t�@���Ɠ����̃o�b�t�@�������Ă��쐬
;;; @param name �o�b�t�@��
;;; @param &optional path �쐬�����o�b�t�@�ɓǂݍ��ރt�@�C���̃p�X
;;; @return �쐬�����o�b�t�@�B�t�@�C���Ǎ����s���� nil
(defun create-working-buffer (name &optional path)
  (let* ((buf (create-new-buffer name))
		 )
	(setup-temp-buffer buf)
	(set-buffer buf)

	(when path
	  ;; �t�@�C���Ǎ��B�Ǎ����s���̓o�b�t�@�폜
	  (handler-case
		  (read-file path)
		(error (c)
		  (delete-buffer buf)
		  (error c)
		  )
		) ;handler-case
	  ) ;when

	buf
	) ;let*
  )

;;; ��Ɨp�o�b�t�@���쐬���A������폜����
;;; @param var-buf �쐬������Ɨp�o�b�t�@�ɑ��������ϐ�
;;; @param &rest args ��Ɨp�o�b�t�@�쐬�ׁ̈A�֐� create-working-buffer �ɓn��������
;;; @param &body body
(defmacro with-working-buffer ((var-buf &rest args) &body body)
  `(let* ((,var-buf (create-working-buffer ,@args))
		  )
	 (unwind-protect
		 (progn
		   ,@body
		   ) ;progn
	   (delete-buffer ,var-buf)
	   ) ;unwind-protect
	 ) ;let*
  )



#|
;;; �J�����g�E�B���h�E�̍ŏ㕔�ɕ\������Ă�s���w��s�ɂ���
;;; @param ln 
(defun set-window-start-line (ln)
  (when (<= 1 ln (buffer-lines))
	(scroll-window (- ln (get-window-start-line)))
	t
	) ;when
  )
|#

#|
;;; �w��E�B���h�E�̍ŏI�s���\�������l�X�N���[��������
;;; �i���O�o�͂Ȃǂ̃E�B���h�E�ɗ��p�j
;;; ���擾����� ln-start �� wnd-lines �͉�ʕ\���Ɖ��̂��قȂ�
;;; �@�������ʂƂ��ăX�N���[���ʂ͐������B���R�s��
;;; @param wnd
(defun scroll-window-to-final-line (wnd)
  (let* ((ln-start (get-window-start-line wnd))
		 (wnd-lines (window-lines wnd))
		 (buf-lines (buffer-lines (window-buffer wnd)))
		 ;; ���s�X�N���[��������΍ŏI�s�����[�ɗ��邩
		 (line-offset (- buf-lines (+ ln-start wnd-lines -1)))
		 )
;(message "ln-start=~a, wnd-lines=~a, buf-lines=~a, line-offset=~a" ln-start wnd-lines buf-lines line-offset)
	;; �o�b�t�@�ŏI�s���E�B���h�E���[��艺�Ȃ�X�N���[��
	(when (< 0 line-offset)
	  (with-selected-window
		(set-window wnd)
		(scroll-window line-offset)
		) ;with-selected-window
	  ) ;when
	) ;let*
  )
|#

#|
;;; �w��o�b�t�@��\������S�ẴE�B���h�E�ɉ����āA
;;; �ŏI�s���\�������l�X�N���[��������
;;; @param buf
(defun scroll-buffer-window-to-final-line (buf)
  (let* ((lis-wnd (get-buffer-windows buf))
		 )
	(dolist (wnd lis-wnd)
	  (scroll-window-to-final-line wnd)
	  ) ;dolist
	) ;let*
  )
|#

#|
;;; �S�ẴE�B���h�E���擾
;;; @param &optional wnd-start �ŏ��Ɏ擾����E�B���h�E
(defun get-all-windows (&optional wnd-start)
  (let* ((wnd (or wnd-start (selected-window)))
		 (lis-wnd nil)
		 )
	(dotimes (i (count-windows) lis-wnd)
	  (push wnd lis-wnd)
	  (setf wnd (next-window wnd))
	  ) ;dotimes
	) ;let*
  )
|#

#|
;;; �w��o�b�t�@�̃E�B���h�E��S�Ď擾
;;; ��get-buffer-window �Ŏ擾���悤�Ƃ��Ă����܂������Ȃ��i�o�O�H�j�̂�
;;; �@�S�ẴE�B���h�E���擾���Ă���I��
;;; @param buf
(defun get-buffer-windows (buf)
  (let* ((lis-wnd (get-all-windows)) ; �S�ẴE�B���h�E
		 )
	;; �w��o�b�t�@�ƈقȂ�E�B���h�E���폜
	(remove-if-not #'(lambda (w) (eql (window-buffer w) buf))
				   lis-wnd
				   ) ;remove-if-not
	) ;let*
  )
|#

#|
;;; �E�B���h�E���ʒu�Ń\�[�g����ۂɗp����֐�
(defparameter *fnc-pos-priority*
  (list
   :left-top #'(lambda (c1 c2) (or (< (cadr c1) (cadr c2)) ;���
								   (and (eql (cadr c1) (cadr c2))
										(< (car c1) (car c2)) ;����
										) ;and
								   ) ;or
				 ) ;lambda
   :right-top #'(lambda (c1 c2) (or (< (cadr c1) (cadr c2)) ;���
									(and (eql (cadr c1) (cadr c2))
										 (< (caddr c2) (caddr c1)) ;�E��
										 ) ;and
									) ;or
				  ) ;lambda
   :left-bottom #'(lambda (c1 c2) (or (< (cadddr c2) (cadddr c1)) ;����
									  (and (eql (cadddr c1) (cadddr c2))
										   (< (car c1) (car c2)) ;����
										   ) ;and
									  ) ;or
					) ;lambda
   :right-bottom #'(lambda (c1 c2) (or (< (cadddr c2) (cadddr c1)) ;����
									   (and (eql (cadddr c1) (cadddr c2))
											(< (caddr c2) (caddr c1)) ;�E��
											) ;and
									   ) ;or
					 ) ;lambda
   ) ;list
  )
|#

#|
;;; �E�B���h�E���ʒu�Ń\�[�g
;;; @param lis-wnd �E�B���h�E�̃��X�g
;;; @param direction :left-top/:left-bottom/:right-top/:right-bottom
(defun sort-window-by-position (lis-wnd direction)
  (let* ((fnc (getf *fnc-pos-priority* direction))
		 )
	(if fnc
		;; (�E�B���h�E ���W�j �� �E�B���h�E
		(mapcar #'car
				(sort
				 ;; �E�B���h�E �� (�E�B���h�E ���W�j
				 (mapcar #'(lambda (wnd) (list wnd (window-coordinate wnd)))
						 lis-wnd
						 ) ;mapcar
				 fnc
				 :key #'cadr
				 ) ;sort
				) ;mapcar
		;; else direction ���s�K��
		lis-wnd
	  ) ;if
	) ;let*
  )
|#



