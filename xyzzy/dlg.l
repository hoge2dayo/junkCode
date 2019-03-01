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
		  sample-dialog
		  )
		)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; ���E�B���h�E�X�^�C���p�萔
;;;; 

;;;; ���E�B���h�E

(defvar +WS-POPUPWINDOW+       #x80880000) ; +WS-POPUP+�A+WS-BORDER+�A+WS-SYSMENU+ �̕��p�B�R���g���[�����j���[�����ɂ���ɂ́A+WS-CAPTION+ �� +WS-POPUPWINDOW+ ���K�v
(defvar +WS-POPUP+             #x80000000) ; �|�b�v�A�b�v�E�B���h�E�B+WS-CHILD+ �Ƃ͕��p�s��
(defvar +WS-CHILD+             #x40000000) ; �q�E�B���h�E�B+WS-POPUP+ �Ƃ͕��p�s��
(defvar +WS-MINIMIZE+          #x20000000) ; ������ԂŃA�C�R�������ꂽ�E�B���h�E���쐬�B+WS-OVERLAPPED+ �K�{
(defvar +WS-VISIBLE+           #x10000000) ; ������Ԃŉ��̃E�B���h�E
(defvar +WS-DISABLED+          #x08000000) ; ������ԂŎg�p�֎~�̃E�B���h�E
(defvar +WS-MAXIMIZE+          #x01000000) ; �ő�\�����ꂽ�E�B���h�E
(defvar +WS-OVERLAPPEDWINDOW+  #x00CF0000) ; +WS-CAPTION+�A+WS-SYSMENU+�A+WS-THICKFRAME+�A+WS-MINIMIZEBOX+�A+WS-MAXIMIZEBOX+�A+WS-OVERLAPPED+ �̕��p
(defvar +WS-CAPTION+           #x00C00000) ; �^�C�g�� �o�[�����E�B���h�E�i�Öق� +WS-BORDER+ �j�B+WS-DLGFRAME+ �Ƃ͕��p�s��
(defvar +WS-BORDER+            #x00800000) ; ���E�����E�B���h�E
(defvar +WS-DLGFRAME+          #x00400000) ; ��d���E�������A�^�C�g���������Ȃ��E�B���h�E
(defvar +WS-VSCROLL+           #x00200000) ; �����X�N���[���o�[�����E�B���h�E
(defvar +WS-HSCROLL+           #x00100000) ; �����X�N���[���o�[�����E�B���h�E
(defvar +WS-SYSMENU+           #x00080000) ; �^�C�g���o�[�ɃR���g���[�����j���[�{�b�N�X�����E�B���h�E�B�^�C�g���o�[�����E�B���h�E�ɂ̂ݎg�p
(defvar +WS-THICKFRAME+        #x00040000) ; �E�B���h�E�̃T�C�Y�ύX�Ɏg�p�\�ȑ����g�����E�B���h�E
(defvar +WS-GROUP+             #x00020000) ; �O���[�v�̍ŏ��̃R���g���[���̎w��B�ŏ��̃R���g���[���̌�� +WS-GROUP+ �X�^�C���łȂ��R���g���[���͓����O���[�v�ɑ�����
(defvar +WS-MINIMIZEBOX+       #x00020000) ; �A�C�R�����{�^�������E�B���h�E
(defvar +WS-MAXIMIZEBOX+       #x00010000) ; �ő�\���{�^�������E�B���h�E
(defvar +WS-SIZEBOX+           #x0000F2C0) ; +WS-THICKFRAME+ �Ɠ���
(defvar +WS-OVERLAPPED+        #x00000000) ; �I�[�o���b�v �E�B���h�E���쐬

;;;; ���h���b�v�_�E�����X�g�A�R���{�{�b�N�X

(defvar +CBS-LOWERCASE+        #x00004000) ; �R���{�{�b�N�X�ɓ��͂��ꂽ�啶�����������ɕϊ�
(defvar +CBS-UPPERCASE+        #x00002000) ; �R���{�{�b�N�X�ɓ��͂��ꂽ��������啶���ɕϊ�
(defvar +CBS-DISABLENOSCROLL+  #x00000800) ; ���ڂ����Ȃ��Ƃ��ɐ����X�N���[���o�[��s�����ŕ\��
(defvar +CBS-NOINTEGRALHEIGHT+ #x00000400) ; �R���{�{�b�N�X�̃T�C�Y���A�v���P�[�V�����w��̃T�C�Y�ƂȂ�B�ʏ�� Windows ���T�C�Y��ύX
(defvar +CBS-SORT+             #x00000100) ; ���X�g�{�b�N�X���̕�����������I�Ƀ\�[�g
(defvar +CBS-OEMCONVERT+       #x00000080) ; ���͂��ꂽ�e�L�X�g���AANSI �����Z�b�g��OEM �����Z�b�g��ANSI �����Z�b�g�ɕϊ��i�߂��j�B�t�@�C������ێ�����R���{�{�b�N�X�ŗL�p�B+CBS-SIMPLE+ �܂��� +CBS-DROPDOWN+ �̃R���{�{�b�N�X�ł̂ݎw��\
(defvar +CBS-DROPDOWN+         #x00000002)
(defvar +CBS-SIMPLE+           #x00000001) ; ���X�g�{�b�N�X����ɕ\���B���X�g�{�b�N�X�̌��݂̑I�����ڂ��G�f�B�b�g�R���g���[���ɕ\�������

;;;; �����X�g�{�b�N�X

(defvar +LBS-NOSEL+            #x00004000) ; �ǎ���p�i�I��s�j
(defvar +LBS-DISABLENOSCROLL+  #x00001000) ; ���ڂ����Ȃ��Ƃ��ɐ����X�N���[���o�[��s�����ŕ\��
(defvar +LBS-EXTENDEDSEL+      #x00000800) ; ����ȃL�[�̑g�ݍ��킹�ŁA���[�U�[���������ڑI���\
(defvar +LBS-NOINTEGRALHEIGHT+ #x00000100) ; ���X�g�{�b�N�X�̃T�C�Y���A�v���P�[�V�����w��̃T�C�Y�ƂȂ�B�ʏ�� Windows ���T�C�Y��ύX
(defvar +LBS-USETABSTOPS+      #x00000080) ; �����񒆂̃^�u������F�����W�J����B�f�t�H���g�̃^�u�ʒu�� 32 �_�C�A���O�P��
(defvar +LBS-MULTIPLESEL+      #x00000008) ; �N���b�N�܂��̓_�u���N���b�N�ɂ��A������̑I�����؂�ւ��
(defvar +LBS-SORT+             #x00000002) ; ���X�g�{�b�N�X���̕�������A���t�@�x�b�g���Ƀ\�[�g

;;;; ���G�f�B�b�g�{�b�N�X

(defvar +ES-NUMBER+            #x00002000) ; �����̓��͂̂ݎ󂯕t����
(defvar +ES-WANTRETURN+        #x00001000) ; �����s�̃R���g���[���ł̓��͎� Enter �L�[�ŉ��s
(defvar +ES-READONLY+          #x00000800) ; �e�L�X�g�̓��͂�ҏW���ł��Ȃ��悤�ɂ���
(defvar +ES-OEMCONVERT+        #x00000400) ; ���͂��ꂽ�e�L�X�g���AANSI �����Z�b�g��OEM �����Z�b�g��ANSI �����Z�b�g�ɕϊ��i�߂��j�B�t�@�C������ێ�����G�f�B�b�g�{�b�N�X�ŗL�p�B
(defvar +ES-NOHIDESEL+         #x00000100) ; ���̓t�H�[�J�X���󂯎�����Ƃ��̋����\�������Ȃ�
(defvar +ES-AUTOHSCROLL+       #x00000080) ; �s���ł̓��͂Ńe�L�X�g�������I�� 10 �������E�փX�N���[��
(defvar +ES-AUTOVSCROLL+       #x00000040) ; �ŏI�s�ł� Enter �L�[�Ŏ����I�� 1 �y�[�W����ɃX�N���[��
(defvar +ES-PASSWORD+          #x00000020) ; ���͂��ꂽ���ׂĂ̕����� �A�X�^���X�N (*) �ŕ\��
(defvar +ES-LOWERCASE+         #x00000010) ; ���͂��ꂽ���������ׂď������ɕϊ�
(defvar +ES-UPPERCASE+         #x00000008) ; ���͂��ꂽ���������ׂđ啶���ɕϊ�
(defvar +ES-MULTILINE+         #x00000004) ; �����s
(defvar +ES-RIGHT+             #x00000002) ; �e�L�X�g���E�����ŕ\��
(defvar +ES-CENTER+            #x00000001) ; �e�L�X�g�𒆉������ŕ\��
(defvar +ES-LEFT+              #x00000000)

;;;; ���{�^���A�`�F�b�N�{�b�N�X�A���W�I�{�^���A�O���[�v�{�b�N�X

(defvar +BS-BUTTON+            #x00010000)
(defvar +BS-FLAT+              #x00008000) ; ���R�ȃ{�^��
(defvar +BS-NOTIFY+            #x00004000) ; BN-DBLCLK,BN-KILLFOCUS,BN-SETFOCUS �ʒm���b�Z�[�W��e�ɑ���
(defvar +BS-MULTILINE+         #x00002000) ; �����s�e�L�X�g��\��
(defvar +BS-PUSHLIKE+          #x00001000) ; �v�b�V���{�^���̂悤�ȃ`�F�b�N�{�b�N�X�܂��̓��W�I�{�^��
(defvar +BS-VCENTER+           #x00000C00) ; �e�L�X�g�𐂒������Œ�����
(defvar +BS-BOTTOM+            #x00000800) ; �e�L�X�g������
(defvar +BS-TOP+               #x00000400) ; �e�L�X�g�����
(defvar +BS-CENTER+            #x00000300) ; �e�L�X�g�𐅕������Œ�����
(defvar +BS-RIGHT+             #x00000200) ; �e�L�X�g���E��
(defvar +BS-LEFT+              #x00000100) ; �e�L�X�g������
(defvar +BS-BITMAP+            #x00000080) ; �r�b�g�}�b�v��\��
(defvar +BS-ICON+              #x00000040) ; �A�C�R����\��
(defvar +BS-LEFTTEXT+          #x00000020) ; �`�F�b�N�{�b�N�X�Ƃ烉�W�I�{�^���̃`�F�b�N�����E�ɔz�u
(defvar +BS-OWNERDRAW+         #x00000011) ; �I�[�i�[�`��{�^��
(defvar +BS-AUTORADIOBUTTON+   #x00000009) ; �O���[�v���Ŏ����I�Ɉ�����`�F�b�N��Ԃɂ��郉�W�I�{�^��
(defvar +BS-USERBUTTON+        #x00000008) ; Win32 �ł͎g��Ȃ�
(defvar +BS-GROUPBOX+          #x00000007) ; �O���[�v�{�b�N�X
(defvar +BS-AUTO3STATE+        #x00000006) ; �O�̏�Ԃ������I�ɏ��񂷂�`�F�b�N�{�b�N�X
(defvar +BS-3STATE+            #x00000005) ; �`�F�b�N�E�A���`�F�b�N�̂ق��s���\������`�F�b�N�{�b�N�X
(defvar +BS-RADIOBUTTON+       #x00000004) ; ���W�I�{�^��
(defvar +BS-AUTOCHECKBOX+      #x00000003) ; �`�F�b�N�}�[�N�������I�ɕύX�����`�F�b�N�{�b�N�X
(defvar +BS-CHECKBOX+          #x00000002) ; �`�F�b�N�{�b�N�X
(defvar +BS-DEFPUSHBUTTON+     #x00000001) ; ���g�̃f�t�H���g�{�^���B�_�C�A���O�{�b�N�X�ł� Enter �L�[�����őI���\
(defvar +BS-TEXT+              #x00000000) ; �{�^���̓e�L�X�g��\���i�f�t�H���g�j

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; ��
;;;; 

;;; �v���p�e�B���X�g�̃}�[�W
;;; �Q�����ɋ��ʂ���v���p�e�B�� plis-add �̒l���D�悳���
;;; @param plis-add �D�悳��� p-list
;;; @param plis-base �f�t�H���g�� plist
;;; @return �v���p�e�B���X�g
(defun merge-plist (plis-add plis-base)
  (do ((rem plis-add (cddr rem))
	   (plis (copy-list plis-base)) ;�萔�w�莞���l���ĕ���
	   )
	  ((null rem) plis)
	 (setf (getf plis (car rem)) (cadr rem))
	) ;do
  )

;;; place ����� �w�肵�����l�̍ő�l�� place ���X�V
;;; @param place �ĕϐ�
;;; @param number ���l
;;; @param &rest more-numbers ���l�i0 �ȏ�j
;;; @return 
(defmacro maxf (place number &rest more-numbers)
  `(setf ,place (max ,place ,number ,@more-numbers))
  )

;;; place �� place ����� �w�肵�����l�̍ŏ��l�ōX�V
;;; @param place �ĕϐ�
;;; @param number ���l
;;; @param &rest more-numbers ���l�i0 �ȏ�j
;;; @return 
(defmacro minf (place number &rest more-numbers)
  `(setf ,place (min ,place ,number ,@more-numbers))
  )


;;; ���p���������P�A�S�p���������Q�Ƃ���������̕�
;;; @param s ������
;;; @return ������̕��i���p�������P�A�S�p�������Q�Ƃ���j
(defun string-columns (s)
  (reduce #'(lambda (w ch) (+ w (char-columns ch))) s
		  :initial-value 0
		  ) ;reduce
  )

;;; ������̍s���ƕ�
;;; ���͔��p�������P�A�S�p�������Q�Ƃ���
;;; @param s ������
;;; @return ���X�g�B��P�v�f�F�s���A��Q�v�f�F�ő啝
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

;;; �����̍������邢�͑S�p�����̕�
(defparameter *font-len* 9)

;;; �����񂪐�߂�T�C�Y
;;; @param s ������
;;; @return �T�C�Y�i*font-len* �P�ʁj
(defun string-size (s)
  (let* ((row-col (caption-rows-columns s))
		 (col (cadr row-col))
		 (row (max (car row-col) 1)) ;�󕶎���ł��P�s�Ƃ���
		 )
	(geo:create-size (round (* *font-len* 1/2 col)) (* *font-len* row))
	) ;let*
  )

;;; �I�v�V�����w��ɂ��A�T�C�Y�𒲐�
;;; ���j��I
;;; @param siz �T�C�Y�i���X���b�g�l���ύX�����j
;;; @param option �I�v�V�����w��̃v���p�e�B���X�g
;;; @return ������̃T�C�Y�i���� siz �j
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
;;;; ���R���g���[���A���C�A�E�g
;;;; 

;;; �R���g���[���i ctrl �j����у��C�A�E�g�i layout �j�̐e�N���X�i�p���p�j
(defstruct element
  (rect (geo:create-rect 0 0 0 0)) ;���̗v�f�ׂ̈Ɋm�ۂ��ꂽ��`�̈�
  (option nil)                     ;�I�v�V�����i�v���p�e�B���X�g�j
  )

;;; �}�[�W���i�R���g���[���p�j
(defparameter *ctrl-margin* 4)
;;; �p�f�B���O�i�R���g���[���p�j
(defparameter *ctrl-padding* 0)

;;; �R���g���[���i�p���p�j
(defstruct (ctrl (:include element))
  (description-name nil)   ;�֐� dialog-box �� :control �߂Ŏg�p����^���B�p����Œl��ݒ肷�邱��
  (margin *ctrl-margin*)   ;�}�[�W��
  (padding *ctrl-padding*) ;�p�f�B���O
  (name nil)               ;�e�R���g���[���ʂ̖���
  (caption "")             ;�L���v�V����
  (style nil)              ;�E�B���h�E�X�^�C���p�萔�̑g�ݍ��킹
  )

;;; �R���g���[���̋�`�T�C�Y�i�}�[�W���ƃp�f�B���O���܂ށj
;;; @param obj �\���� ctrl
;;; @return �R���g���[���̃T�C�Y
(defun size-ctrl (obj)
  (let* ((margin (ctrl-margin obj))
		 (padding (ctrl-padding obj))
		 (siz (string-size (ctrl-caption obj)))
		 )
	;; padding ���܂߂���ɃI�v�V�����ɂ��T�C�Y����
	(geo:enlarge-size* siz (* 2 padding) (* 2 padding))
	(adjust-size-by-option siz (element-option obj))

	(geo:enlarge-size* siz (* 2 margin) (* 2 margin))
	) ;let*
  )

;;; �֐� dialog-box �� :control �߂֓n����
;;; �}�[�W���͊܂߂Ȃ��R���g���[���݂̂̋�`��Ԃ�
;;; @param obj �R���g���[���i�\���� ctrl�j
;;; @return �֐� dialog-box �� :control �߂֓n����
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

;;; ���C�A�E�g�i�p���p�j
(defstruct (layout (:include element))
  (lis-elm nil)  ;�����ɕێ��E�z�u����v�f
  )

;;; ��`���ʒu�Ń\�[�g���邽�߂̔�r�֐�
;;; Y ���W���قȂ�ꍇ�AY ���W���������������Ƃ���
;;; Y ���W�������ꍇ�́AX ���W���������������Ƃ���
;;; �i�R���g���[�����t�H�[�J�X�ړ����ɕ��ׂ�̂Ɏg�p�j
;;; @param rct0 ��`
;;; @param rct1 ��`
;;; @return rct0 < rct1 �̏ꍇ�ɐ^
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

;;; �֐� dialog-box �� :control �߂֓n�����̃��X�g
;;; �R���g���[���̓t�H�[�J�X�̈ړ�����ʓI�ɂȂ�悤�Ƀ\�[�g����
;;; �i edge-layout �� :bottom ���g�����ꍇ�A�\�[�g���Ȃ���
;;; �@���̃R���g���[������Ɍ���Ă��܂��j
;;; @param obj ���C�A�E�g�i�\���� layout�j
;;; @return ���X�g�B�e�v�f�͊֐� dialog-box �� :control �߂֓n����
(defun description-layout (obj)
  (cond
   ((ctrl-p obj) (list (description-ctrl obj)))
   ((layout-p obj) (let* ((lis-elm (copy-list (layout-lis-elm obj)))
						  )
					 ;; �ʒu�Ń\�[�g
					 (setf lis-elm (sort lis-elm #'rect-pos< :key #'element-rect))
					 (mapcan #'description-layout lis-elm)
					 ) ;let*
	)
   ) ;cond
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; �����x��
;;;; 

(defstruct (label
			(:include ctrl
			 (description-name :static)
			 ;;+WS-GROUP+ �̕K�v�����肩�ł͂Ȃ����A�o�[�W�����_�C�A���O�ł̓Z�b�g����Ă���
			 (style (logior +WS-CHILD+ +WS-VISIBLE+ +WS-GROUP+))
			 )
			(:constructor create-label (caption &key margin padding option))
			)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; ���{�^��
;;;; 

;;; �p�f�B���O�i�{�^���p�j
(defparameter *button-padding* 4)

;;; �{�^��
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
;;;; ���e�L�X�g�{�b�N�X
;;;; 

;;; �p�f�B���O�i�e�L�X�g�{�b�N�X�p�j
(defparameter *edit-padding* 3)

;;; �e�L�X�g�{�b�N�X�i�p���p�j
(defstruct (edit-base
			(:include ctrl
			 (description-name :edit)
			 (padding *edit-padding*)
			 )
			)
  )

;;; �e�L�X�g�{�b�N�X�i�P��s�j
(defstruct (edit
			(:include edit-base
			 (style (logior +WS-CHILD+ +WS-VISIBLE+ +WS-BORDER+ #x00010000
							+ES-AUTOHSCROLL+ +ES-LEFT+))
			 )
			;; �i�R���X�g���N�^���ɒ��Ӂj
			(:constructor create-text (name &key margin padding option))
			)
  )

;;; �e�L�X�g�{�b�N�X�i�����s�j
(defstruct (edit-multi
			(:include edit-base
			 (style (logior +WS-CHILD+ +WS-VISIBLE+ +WS-BORDER+ #x00010000
							+ES-WANTRETURN+ +ES-AUTOVSCROLL+ +ES-MULTILINE+ +ES-LEFT+))
			 )
			;; :option �w��̒l�́A�w��l���X���b�g�Ɋi�[����̂ł͂Ȃ��A
			;; �f�t�H���g�l�ƃ}�[�W�����l���i�[����
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
;;;; ���`�F�b�N�{�b�N�X
;;;; 

;;; �p�f�B���O�i�`�F�b�N�{�b�N�X�p�j
(defparameter *check-padding* 2)

;;; �`�F�b�N�{�b�N�X�i�p���p�j
(defstruct (check-base
			(:include ctrl
			 (description-name :button) ;:button �ł���_�ɒ���
			 (padding *check-padding*)
			 )
			)
  )

;;; �`�F�b�N�{�b�N�X�i�Q�X�e�[�g�j
(defstruct (check
			(:include check-base
			 (style (logior +WS-CHILD+ +WS-VISIBLE+ #x00010000 +BS-AUTOCHECKBOX+))
			 )
			(:constructor create-check (name caption &key margin padding option))
			)
  )

;;; �`�F�b�N�{�b�N�X�i�R�X�e�[�g�j
(defstruct (check3
			(:include check-base
			 (style (logior +WS-CHILD+ +WS-VISIBLE+ #x00010000 +BS-AUTO3STATE+))
			 )
			(:constructor create-check3 (name caption &key margin padding option))
			)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; ���R���{�{�b�N�X
;;;; 

;;; �p�f�B���O�i�R���{�{�b�N�X�p�j
(defparameter *combobox-padding* 4)

;;; �R���{�{�b�N�X
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
;;;; �����X�g�{�b�N�X
;;;; 

;;; �p�f�B���O�i���X�g�{�b�N�X�p�j
(defparameter *listbox-padding* 4)

;;; ���X�g�{�b�N�X
(defstruct (listbox
			(:include ctrl
			 (description-name :listbox)
			 (padding *listbox-padding*)
			 (style (logior +WS-CHILD+ +WS-VISIBLE+ +WS-BORDER+
							+WS-VSCROLL+ +WS-HSCROLL+ #x00010000 +CBS-SIMPLE+
							) ;logior
					)
			 )
			;; :option �w��̒l�́A�w��l���X���b�g�Ɋi�[����̂ł͂Ȃ��A
			;; �f�t�H���g�l�ƃ}�[�W�����l���i�[����
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
;;;; ��column���C�A�E�g
;;;; 
;;;; �����v�f���c�ɕ��ׂ�B
;;;; 

;;; column ���C�A�E�g
(defstruct (column-layout
			(:include layout
			 )
			(:constructor create-column-layout (lis-elm &key align valign)
			 )
			)
  (align :left)  ;���������̊񂹁@:left�F�� / :center�F���� / :right�F�E
  (valign :top)  ;���������̊񂹁@:top�F�� / :center�F���� / :bottom�F��
  )

;;; �����v�f���܂ރT�C�Y
;;; @param obj column ���C�A�E�g�i�\���� column-layout�j
;;; @return ���C�A�E�g�̃T�C�Y
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

;;; �����v�f�̔z�u�i�e�v�f�̃X���b�g rect �l�̌���j
;;; ���C�A�E�g�i��P�����j�� rect �͐ݒ�ς݂ł��邱��
;;; �����v�f���c�ɕ��ׂ�
;;; @param obj column ���C�A�E�g�i�\���� column-layout�j
;;; @return 
(defun arrange-column-layout (obj)
  (let* ((align (column-layout-align obj))
		 (rct (geo:copy-rect* (element-rect obj)))
		 (left   (geo:rect-l rct))
		 (right  (geo:rect-r rct))
		 (bottom (geo:rect-b rct))
		 (lis-elm (column-layout-lis-elm obj))
		 (lis-elm-siz (mapcar #'size-element lis-elm))
		 (y (geo:rect-y rct))  ;�����v�f�z�u�J�n���� Y ���W
		 ;; ��ԉ��ɔz�u���ꂽ�R���g���[���̉��̈ʒu
		 (last-y (reduce #'+ (mapcar #'geo:size-h lis-elm-siz)
						 :initial-value y
						 ) ;reduce
				 )
		 ;; �c�̊񂹕�
		 (off-y (case (column-layout-valign obj)
				  (:center (round (- bottom last-y) 2))
				  (:bottom (- bottom last-y))
				  (t 0)
				  ) ;case
				)
		 )
	;; �e�����v�f�̈ʒu�E�T�C�Y�i�X���b�g rect�j���Z�b�g
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

	;; �����v�f�Ƀ��C�A�E�g������ꍇ�A���̃��C�A�E�g�̓����z�u
	(map nil #'arrange-layout (remove-if-not #'layout-p lis-elm))
	) ;let*
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; ��row ���C�A�E�g
;;;; 
;;;; �����v�f�����ɕ��ׂ�B
;;;; 

;;; row ���C�A�E�g
(defstruct (row-layout
			(:include layout)
			(:constructor create-row-layout (lis-elm &key valign spread))
			)
  (valign :top)  ;���������̊񂹁@:top�F�� / :center�F���� / :bottom�F��
  (spread nil)   ;���������ɃR���g���[�����L���邩�ǂ���
  )

;;; �����v�f���܂ރT�C�Y
;;; @param obj row ���C�A�E�g�i�\���� row-layout�j
;;; @return ���C�A�E�g�̃T�C�Y
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

;;; �����v�f�̔z�u�i�e�v�f�̃X���b�g rect �l�̌���j
;;; ���C�A�E�g�i��P�����j�� rect �͐ݒ�ς݂ł��邱��
;;; �����v�f���c�ɕ��ׂ�
;;; @param obj row ���C�A�E�g�i�\���� row-layout�j
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
		 ;; �����v�f�𐅕������Ɋg�傷��䗦
		 (ratio (if spread (/ (geo:rect-w rct) sum-w) 1))
		 )
	;; �e�����v�f�̈ʒu�E�T�C�Y�i�X���b�g rect�j���Z�b�g
	(map nil #'(lambda (elm elm-siz
						&aux
						(elm-w-new (* ratio (geo:size-w elm-siz))) ;layout ���S�̂��g���悤�ɁA�e�v�f�̕����g��
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

	;; �����v�f�Ƀ��C�A�E�g������ꍇ�A���̃��C�A�E�g�̓����z�u
	(map nil #'arrange-layout (remove-if-not #'layout-p lis-elm))
	) ;let*
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; ��edge ���C�A�E�g
;;;; 
;;;; �����v�f��Ӂi�㉺���E�j�ɉ����Ĕz�u����B
;;;; �i�ʒu :center �͎c��̕����B�ő�P��̂ݎw��\�j
;;;; 

;;; edge ���C�A�E�g
(defstruct (edge-layout
			(:include layout)
			)
  (lis-edge nil)  ;�ʒu�w��i:left/:top/:right/:bottom/:center�j�̂ݏW�߂����X�g
  )

;;; edge ���C�A�E�g�̃R���X�g���N�^
;;; ���̃`�F�b�N������B
;;; 	�ʒu�w�� :center �̎w��񐔂��P��ȉ���
;;; 	�ʒu�w�� :center �������ꍇ�A���̉ӏ��͍Ōォ
;;; �i��j
;;; (create-edge-layout (list :left (create- ... )
;;;                           :right (create- ...)
;;;                           :center (create- ...)
;;;                      )
;;;  )
;;; @param lis �ʒu�w��Ɨv�f���v���p�e�B���X�g�̂悤�Ɍ��݂������X�g
;;; 	�ʒu�w��� :left/:top/:right/:bottom/:center �̂����ꂩ
;;; 	�ʒu�w�� :center �͍ő�P��B�w��\�ӏ��͍Ō�̂�
;;; @return �\���� edge-layout
(defun create-edge-layout (lis)
  (let* ((lis-edge nil)
		 (lis-elm nil)
		 )
	(do ((rem lis (cddr rem))
		 (domain '(:top :left :right :bottom :center)) ;�w��\�Ȉʒu�i����j
		 )
		((null rem) (list lis-edge lis-elm))
	  (unless (member (car rem) domain)
		(error "���񂪕s�K�؁B�F~a" (car rem))
		) ;unless

	  ;; �ʒu�w��i����j�Ɨv�f�𕪗����Ċi�[
	  (push (car rem) lis-edge)
	  (push (cadr rem) lis-elm)
	  ) ;do

	;; �ʒu�w��i����j�Ɨv�f�̏��Ԃ��t�]���Ă���̂Ŗ߂�
	(setf lis-edge (reverse lis-edge)
		  lis-elm (reverse lis-elm)
		  ) ;setf

	;; :center �����݂���ꍇ�A�Ōォ�ǂ����`�F�b�N
	(when (cdr (member :center lis-edge))
	  (error "���� :center �͍Ō�ɂ̂ݐݒ�\�B")
	  )

	(make-edge-layout :lis-elm lis-elm
					  :lis-edge lis-edge
					  ) ;make-edge-layout
	) ;let*
  )

;;; �����v�f���܂ރT�C�Y
;;; @param obj edge ���C�A�E�g�i�\���� edge-layout�j
;;; @return ���C�A�E�g�̃T�C�Y
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
		 ;; ����`�𑫂��Ă����悤�ɃT�C�Y�����߂�̂ŋt���ɂ���
		 (reverse (edge-layout-lis-edge obj))
		 (reverse (edge-layout-lis-elm obj))
		 ) ;map

	(geo:create-size w h)
	) ;let*
  )

;;; �����v�f�̔z�u�i�e�v�f�̃X���b�g rect �l�̌���j
;;; ���C�A�E�g�i��P�����j�� rect �͐ݒ�ς݂ł��邱��
;;; �����v�f����`�̕ӂɉ����Ĕz�u
;;; @param obj edge ���C�A�E�g�i�\���� edge-layout�j
;;; @return 
(defun arrange-edge-layout (obj)
  (let* ((rct-rem (geo:copy-rect* (element-rect obj)))
		 (lis-elm (edge-layout-lis-elm obj))
		 rct-elm
		 siz-elm
		 )
	;; �e�����v�f�̈ʒu�E�T�C�Y�i�X���b�g rect�j���Z�b�g
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

				 ;; �v�f�̈ʒu�E�T�C�Y�i�X���b�g rect�j���Z�b�g
				 (setf (element-rect elm) rct-elm)
				 ) ;lambda
		 (edge-layout-lis-edge obj) ;�ʒu�w��i����j
		 lis-elm                    ;�����v�f
		 ) ;map

	;; �����v�f�Ƀ��C�A�E�g������ꍇ�A���̃��C�A�E�g�̓����z�u
	(map nil #'arrange-layout (remove-if-not #'layout-p lis-elm))
	) ;let*
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; �����̊֐�
;;;; 

;;; �����v�f���܂ރT�C�Y
;;; @param obj ���C�A�E�g�i�\���� layout ���p�������\���́j
;;; @return ���C�A�E�g�̃T�C�Y
(defun size-layout (obj)
  (let* (siz
		 )
	(setf siz (cond
			   ((column-layout-p obj) (size-column-layout obj))
			   ((row-layout-p obj) (size-row-layout obj))
			   ((edge-layout-p obj) (size-edge-layout obj))
			   ) ;cond
		  ) ;setf
	;; �I�v�V�����ɂ��T�C�Y����
	(adjust-size-by-option siz (element-option obj))
	siz
	) ;let*
  )

;;; �v�f�̋�`�T�C�Y�i�}�[�W���ƃp�f�B���O���܂ށj
;;; @param obj �v�f�i�R���g���[�����邢�̓��C�A�E�g�j
;;; 	�\���� ctrl ���邢�� �\���� layout ���p�������\����
;;; @return �v�f�̃T�C�Y
(defun size-element (obj)
  ;; xyzzy �ł� typep �ō\���̂𔻒�ł��Ȃ�
  (cond
   ((ctrl-p obj) (size-ctrl obj))
   ((layout-p obj) (size-layout obj))
   ) ;cond
  )

;;; ���C�A�E�g�����v�f�̔z�u�i�e�v�f�̃X���b�g rect �l�̌���j
;;; ���C�A�E�g�i��P�����j�� rect �͐ݒ�ς݂ł��邱��
;;; �����v�f����`�̕ӂɉ����Ĕz�u
;;; @param obj ���C�A�E�g�i�\���� layout ���p�������\���́j
;;; @return 
(defun arrange-layout (obj)
  ;; xyzzy �ł� typep �ō\���̂𔻒�ł��Ȃ�
  (cond
   ((column-layout-p obj) (arrange-column-layout obj))
   ((row-layout-p obj) (arrange-row-layout obj))
   ((edge-layout-p obj) (arrange-edge-layout obj))
   ) ;cond
  )

;;; ���C�A�E�g�����v�f�ɑΉ�����R���g���[����z�u�����_�C�A���O��\��
;;; @param siz �_�C�A���O�̃T�C�Y
;;; 	�i�����C�A�E�g�̕K�v�T�C�Y�̕����傫����΁A�����炪�p������j
;;; @param lay ���C�A�E�g
;;; @param caption �L���v�V����
;;; @param &key font �t�H���g��
;;; @param &key font-size �t�H���g�T�C�Y
;;; @param &key initial-value �R���g���[���̏����l
;;; @param &key check-or-disable �R���g���[������̃n���h��
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
;;;; ���T���v���R�[�h
;;;; 

;;; ���C�A�E�g�T���v��
(defparameter *sample-layout*
  (create-edge-layout
   (list :top (create-row-layout (list (create-label "���x���P")
									   (create-label "���x���Q")
									   ) ;list
								 :valign :center
								 :spread t
								 ) ;create-row-layout
		 :bottom (create-row-layout (list (create-check :chk1 "�`�F�b�N�P")
										  (create-check3 :chk2 "�`�F�b�N�Q")
										  ) ;list
									) ;create-row-layout
		 :left (create-column-layout (list (create-edit :edt1)
										   (create-listbox :lst1)
										   (create-button :btn1 "�{�^���P" :option '(:width 80 :height 20))
										   (create-button :btn2 "�{�^���Q")
										   ) ;list
									 ) ;create-column-layout
		 :right (create-column-layout (list (create-combobox :cmb1 :option '(:width 80))
											(create-button :btn3 "�{�^���R")
											(create-button :btn4 "�{�^���S")
											) ;list
									  :align :right
									  ) ;create-column-layout
		 ) ;list
   ) ;create-edge-layout
  ) ;defparameter

;;; �T���v�����C�A�E�g���_�C�A���O�\��
(defun sample-dialog ()
  (display-dialog nil *sample-layout* "�^�C�g��"
				  :initial-value '((:edt1 . "12345")
								   (:cmb1 . ("�R���{�P" "�R���{�Q"))
								   (:lst1 . ("���X�g�P" "���X�g�Q" "���X�g�R"))
								   )
				  )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; ���֐� dialog-box �̐���
;;;; 
;;;; (dialog-box TEMPLATE INITIAL-VALUE CHECK-OR-DISABLE)
;;;; 
;;;; @param TEMPLATE �֐� dialog �ō쐬����_�C�A���O�^�R���g���[���z�u
;;;; @param INITIAL-VALUE �e�R���g���[���̏����l�Ba-list
;;;; @param CHECK-OR-DISABLE �R���g���[������
;;;; 	����
;;;; 	(c1 :disable (one text-a))
;;;; 			�R���g���[�� c1 �� ON �Ȃ�A�R���g���[�� one text-a ��s������
;;;; 
;;;; 	(text-a :non-null "�������͂��ĉ�����" :enable (one))
;;;; 			�R���g���[�� text-a �������́i�󕶎���j�Ȃ�_�C�A���O���b�Z�[�W��\��
;;;; 			�R���g���[�� text-a �ɉ������͂��ꂽ��A�R���g���[�� one ��������
;;;; 
;;;; 	(link :url "mailto:aaa@bbb.ccc")
;;;; 			�R���g���[�� link�i static ���� caption ������ \001 �� \002 �Ŋ���ꂽ��������j
;;;; 			���N���b�N���ꂽ�Ƃ��A�w�� URL ���J��
;;;; 			URL �� "mailto:" �Ŏn�܂�Ƃ��́A���[�����M�ƂȂ�
;;;; 
;;;; 	(line :enable (IDOK) :type integer :min 1 :non-null t
;;;; 		:type-error "�s�ԍ�����͂��Ă�������"
;;;; 		:range-error "1�ȏ���w�肵�Ă�������")
;;;; 			���͒l�����l�łȂ��ꍇ�̓_�C�A���O���b�Z�[�W�\��
;;;; 			���͒l�� 1 �ȏ�łȂ���΃_�C�A���O���b�Z�[�W�\��
;;;; 
;;;; 	(ref :related cmd
;;;; 		 :file-name-dialog (:title "�Q��"
;;;; 							:filter (("���ş��(*.exe;*.com;*.cmd;*.bat;*.pif)" . "*.exe;*.com;*.cmd;*.bat;*.pif")
;;;; 									 ("���ׂĂ�̧��(*.*)" . "*.*"))
;;;; 							:must-exist t))
;;;; 			�{�^�� ref �������ꂽ��t�@�C���_�C�A���O��\�����A�t�@�C���_�C�A���O��
;;;; 			�I�����ꂽ�t�@�C���̃p�X���R���g���[��cmd �փZ�b�g
;;;; 
;;;; 
;;;; ���֐� dialog-box �̗�
;;;; 
;;;; (multiple-value-bind (result alist)
;;;; 	(dialog-box
;;;; 	 `(dialog 0 0 232 76
;;;; 			  (:caption "�Ă���")
;;;; 			  (:font 9 "�l�r �o�S�V�b�N")
;;;; 			  (:control
;;;; 			   (:button backward "�㌟��(&P)" #x10010000 176 7 52 14)
;;;; 			   (:edit line nil ,(logior +WS-CHILD+ +WS-VISIBLE+
;;;; 										+WS-BORDER+ +BS-BUTTON+ +ES-AUTOHSCROLL+) 49 12 40 14)
;;;; 			   (:button IDCANCEL "��ݾ�" #x50030000 176 44 52 14))
;;;; 	  )
;;;; 	 nil  ;INITIAL-VALUE �e�R���g���[���̏����l
;;;; 	 nil  ;CHECK-OR-DISABLE �R���g���[������
;;;; 	)
;;;; )