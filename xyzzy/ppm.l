;;;; require - provide - defpackage - in-package - export

;(require )

(provide :ppm)

(defpackage :ppm (:use :lisp :editor))

(in-package :ppm)

(export '(8color
		  clear-img
		  draw-rect
		  fill-rect
		  draw-line
		  *tmp-filepath*
		  save-img
		  open-imgfile-by-viewer
		  )
		)

;;;; 
;;;; ��ppm �t�@�C���i�摜�j�p�@�`��E�t�@�C���o�͋@�\
;;;; 

;;; �f�W�^���W�F��ێ������z��
(defvar *8color* (make-array 8 :initial-contents '(#x000000 #x0000ff #xff0000 #xff00ff
												   #x00ff00 #x00ffff #xffff00 #xffffff
												   )
							 ) ;make-array
  )

;;; �f�W�^���W�F�̓��P�F���擾
;;; @param idx �C�ӂ̐����i�W�Ŋ�������]���p������j
;;; @return �f�W�^���W�F�̓��̂P�F
(defun 8color (idx)
  (elt *8color* (mod idx 8))
  )

;;; �摜�̍ő啝����эő卂
(defvar *img-len* 1024)

;;; �摜�̉�f�l��ێ������z��
(defvar *img-data* (make-array (list *img-len* *img-len*)
							   :element-type '(unsigned-byte 24)
							   :initial-element 0
							   ) ;make-array
  )

;;; �摜�S�ʃN���A
;;; �����삪�d�����̂ŁA�R���p�C�����邱��
;;; @param &optional pix �N���A�ɗp�����f�l
;;; @return 
(defun clear-img (&optional (pix 0))
  (dotimes (i (* *img-len* *img-len*))
	(setf (row-major-aref *img-data* i) pix)
	) ;dotimes
  )

;;; �n�_�ƏI�_�i�n�_�{�����j���摜���Ɏ��܂�悤�ɒ���
;;; �l�ɂ���Ă͒����s�\�ȏꍇ������A���̏ꍇ�͈����̒l�����̂܂ܕԂ�
;;; @param start �J�n�ʒu�i X ���W���邢�� Y ���W�j
;;; @param len �J�n�ʒu����̒����i X �������邢�� Y �����j
;;; @return ���l
;;; 	[0]: ������� start
;;; 	[1]: ������� len
(defun adjust-start-len (start len)
  (when (and (< start *img-len*)
			 (<= 0 (+ start len))
			 ) ;and
	(when (< start 0)
	  (incf len start)
	  (setf start 0)
	  ) ;when

	(when (<= *img-len* (+ start len))
	  (incf len (- *img-len* (+ start len)))
	  ) ;when
	) ;when

  (values start len)
  )

;;; ��������`��i�J�n�ʒu����E�����ցj
;;; @param x �J�n�ʒu�� X ���W
;;; @param y �J�n�ʒu�� Y ���W
;;; @param width �������̒����i�� 0 �ȏ�j
;;; @param pix ��f�l
;;; @return 
(defun draw-horizontal (x y width pix)
  (when (<= 0 y (1- *img-len*))
	(multiple-value-setq (x width) (adjust-start-len x width))

	(when (and (<= 0 x (1- *img-len*))
			   (<= 0 (+ x width) (1- *img-len*))
			   ) ;and
	  (dotimes (i width)
		(setf (aref *img-data* y (+ x i)) pix)
		) ;dotimes
	  ) ;when
	) ;when
  )

;;; ��������`��i�J�n�ʒu���牺�����ցj
;;; @param x �J�n�ʒu�� X ���W
;;; @param y �J�n�ʒu�� Y ���W
;;; @param height �������̒����i�� 0 �ȏ�j
;;; @param pix ��f�l
;;; @return 
(defun draw-vertical (x y height pix)
  (when (<= 0 x (1- *img-len*))
	(multiple-value-setq (y height) (adjust-start-len y height))

	(when (and (<= 0 y (1- *img-len*))
			   (<= 0 (+ y height) (1- *img-len*))
			   ) ;and
	  (dotimes (i height)
		(setf (aref *img-data* (+ y i) x) pix)
		) ;dotimes
	  ) ;when
	) ;when
  )

;;; ��`��`��
;;; @param x ��`�̍���ʒu�� X ���W
;;; @param y ��`�̍���ʒu�� Y ���W
;;; @param w ��`�̕��i�� 0 �ȏ�j
;;; @param h ��`�̍����i�� 0 �ȏ�j
;;; @param pix ��f�l
;;; @return 
(defun draw-rect (x y w h pix)
  (when (and (< 0 w) (< 0 h))
	(draw-horizontal x y          w pix)
	(draw-horizontal x (+ y h -1) w pix)
	(draw-vertical x          (1+ y) (- h 2) pix)
	(draw-vertical (+ x w -1) (1+ y) (- h 2) pix)
	) ;when
  )

;;; �h��Ԃ��ꂽ��`��`��
;;; @param x ��`�̍���ʒu�� X ���W
;;; @param y ��`�̍���ʒu�� Y ���W
;;; @param w ��`�̕��i�� 0 �ȏ�j
;;; @param h ��`�̍����i�� 0 �ȏ�j
;;; @param pix ��f�l
;;; @return 
(defun fill-rect (x y w h pix)
  (dotimes (i h)
	(draw-horizontal x (+ y i) w pix)
	) ;dotimes
  )

;;; �J�E���^�i�ĕϐ��j�����Z���A 0 �ȉ��ɂȂ�Ύw��̒l�����Z���ď������s��
;;; @param place-counter �����Z�Ώۂ̔ĕϐ�
;;; @param dec �ĕϐ������Z�����
;;; @param inc �ĕϐ��� 0 �ȉ��ɂȂ������ɉ��Z�����
;;; @param &body body-inc �ĕϐ��� 0 �ȉ��ɂȂ������̏���
;;; @return 
(defmacro dec-inc (place-counter dec inc &body body-inc)
  `(when (<= (decf ,place-counter ,dec) 0)
	 (incf ,place-counter ,inc)
	 (progn
	   ,@body-inc
	   ) ;progn
	 )
  )

;;; ������`��
;;; @param x0 �n�_�� X ���W
;;; @param y0 �n�_�� Y ���W
;;; @param x1 �I�_�� X ���W
;;; @param y1 �I�_�� Y ���W
;;; @param pix �`��F
;;; @return
(defun draw-line (x0 y0 x1 y1 pix)
  (let* ((dx (signum (- x1 x0)))
		 (dy (signum (- y1 y0)))
		 (w (abs (- x1 x0)))
		 (h (abs (- y1 y0)))
		 (inc (1+ (max w h)))
		 (dec (1+ (min w h)))
		 (count inc)
		 (x x0)
		 (y y0)
		 )
	(cond
	 ((<= h w)
	  (dotimes (i (1+ w))
		(when (and (<= 0 x (1- *img-len*))
				   (<= 0 y (1- *img-len*))
				   ) ;and
		  (setf (aref *img-data* y x) pix)
		  ) ;when
		(incf x dx)
		(dec-inc count dec inc
		  (incf y dy)
		  ) ;dec-inc
		) ;dotimes
	  )
	 (t
	  (dotimes (i (1+ h))
		(when (and (<= 0 x (1- *img-len*))
				   (<= 0 y (1- *img-len*))
				   ) ;and
		  (setf (aref *img-data* y x) pix)
		  ) ;when
		(incf y dy)
		(dec-inc count dec inc
		  (incf x dx)
		  ) ;dec-inc
		) ;dotimes
	  )
	 ) ;cond
	) ;let*
  )

;;; �e���|�����t�@�C����z�u����f�B���N�g��
(defparameter *tmp-dir* (si:getenv "TMP"))
;;; �e���|�����t�@�C���̃t�@�C����
(defparameter *tmp-file* "tmp.ppm")
;;; �e���|�����t�@�C���̃p�X
(defparameter *tmp-filepath* (merge-pathnames *tmp-file* *tmp-dir*))

;;; �摜�t�@�C���ippm �t�H�[�}�b�g�j��ۑ�
;;; �z�� *img-data* �̍��W( 0, 0 ) ����w��̕��E�����܂ł�ۑ�
;;; @param width �ۑ�����摜�t�@�C���̕��i�ő� *img-len* �j
;;; @param height �ۑ�����摜�t�@�C���̍����i�ő� *img-len* �j
;;; @param &optional filepath �o�͂��� ppm �t�@�C���̃p�X
;;; @return 
(defun save-img (width height &optional (filepath *tmp-filepath*))
  (with-open-file (os filepath
					  :direction :output
					  :if-exists :supersede
					  :if-does-not-exist :create
					  :encoding :binary
					  )
	(format os "P6~%~d ~d 255~%" width height)
	(let* (pix
		   )
	  (dotimes (y height)
		(dotimes (x width)
		  (setf pix (aref *img-data* y x))
		  (princ (code-char (logand (ash pix -16) #xff)) os) ;R
		  (princ (code-char (logand (ash pix -8) #xff)) os)  ;G
		  (princ (code-char (logand pix #xff)) os)           ;B
		  ) ;dotimes
		) ;dotimes
	  ) ;let*
	t
	) ;with-open-file
  )

;;; ppm �t�@�C�����A�v���ŊJ��
;;; ��ppm �t�@�C�����J���f�t�H���g�A�v�����ݒ�ς݂ł��邱��
;;; @param &optional filepath �J�� ppm �t�@�C���̃p�X
;;; @return 
(defun open-imgfile-by-viewer (&optional (filepath *tmp-filepath*))
  (shell-execute filepath nil nil :verb :open)
  )

