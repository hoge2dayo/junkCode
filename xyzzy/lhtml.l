;;;; require - provide - defpackage - in-package - export

;;; �i�֐� compile-lhtml �� �֐� lhtml:print-parsed-lhtml ���Ăяo���̂ɕK�v�j
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
;;;; ��HTML �� Lisp �R�[�h�����݂��� lhtml �t�@�C�����R���p�C���E���s���A
;;;; �@html �t�@�C���𐶐�����
;;;; 
;;;; �� lhtml �t�@�C��
;;;; HTML �t�@�C���̈ꕔ�� Razor �\���������ꂽ���́B
;;;; 
;;;; �� @(�`)
;;;; ���ʂł�����ꂽ�S�̂��r���t�H�[���ƌ��Ȃ��A�]�����ʂ����̈ʒu�ɏo�́B
;;;; 
;;;; �� @{ �` }
;;;; ���s���� Lisp �R�[�h�����q�B�]���l�͒��ړI�ɂ͏o�͂���Ȃ��B
;;;; �r���� @{ �` } ���Ŋ������Ă���K�v�͂Ȃ��B
;;;; 
;;;; �� @@
;;;; ���� `@` �����̈ʒu�ɏo��
;;;; 
;;;; �� @require <�p�b�P�[�W��>
;;;; lhtml �t�@�C�����Ŏg�p����p�b�P�[�W���w��B
;;;; �w��ł���p�b�P�[�W�͈�ŁA���̌�̊��q�͍s���܂Ŗ��������B
;;;; �����̃p�b�P�[�W�����߂�ꍇ�͕������q�B
;;;; 
;;;; �� @using <�p�b�P�[�W��>
;;;; �w�肵���p�b�P�[�W�̊O���V���{�����A�p�b�P�[�W�C���Ȃ��ŋL�q�\�ɂ���B
;;;; �w��ł���p�b�P�[�W�͈�ŁA���̌�̊��q�͍s���܂Ŗ��������B
;;;; �����̃p�b�P�[�W�����߂�ꍇ�͕������q�B
;;;; 
;;;; 
;;;; �� lhtml �̗�
;;;;   <html>
;;;;   <head></head>
;;;;   <body>
;;;;   @{
;;;;   (dotimes (i 5)
;;;;   }
;;;;   <img src="./img/@(1+ i).jpg" /><br/>
;;;;   @{
;;;;    ) ;dotimes �̏I�[
;;;;   }
;;;;   </body>
;;;;   </html>
;;;; 
;;;; ��̊��q�ɂ��o�́E��������� html �t�@�C���͎��̂悤�ɂȂ�B
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

;;; �o�b�t�@���r������ǂݎ��
;;; @param &optional pnt �ǎ�J�n�ʒu
;;; @param &optional buf �o�b�t�@
;;; @return ���l
;;; 	[0]: �ǎ�I���ʒu+1  [1]: �ǂݎ�����r��
(defun read-sexp-from-buffer (&optional (pnt (point)) (buf (selected-buffer)))
  (let* ((is (make-buffer-stream buf pnt))
		 (sexp (read is))
		 )
	(values (buffer-stream-point is) sexp)
	) ;let*
  )

;;; �o�b�t�@�̌��݈ʒu���J�n������Ɉ�v����Ȃ�A�I����������������Ă��̈ʒu��Ԃ�
;;; @param str-start �u���b�N�̊J�n������
;;; @param str-end �u���b�N�̏I��������
;;; @return �I��������̈ʒu
;;;		���݈ʒu�̕����񂪊J�n������ɍ��v���Ȃ��ꍇ => nil
;;;		�I�������񂪑��݂��Ȃ��ꍇ => �G���[
(defun read-block-end-pos (str-start str-end)
  (let* (pnt-end
		 )
	(when (looking-for str-start)
	  (setf pnt-end (progn (scan-buffer str-end) (point)))
	  (unless pnt-end
		(error "�I�[������܂���B�F~a" str-end)
		) ;unless

	  pnt-end
	  ) ;when
	) ;let*
  )

;;; �J�[�\���ʒu�̕����񂪎w�蕶����ƈ�v����Ȃ�A
;;; ���̕������̂r����ǎ��A���̍s���ʒu���r����Ԃ�
;;; @param token �o�b�t�@��ő����Ǝv���镶����
;;; @return ���X�g�B��P�v�f�F���̍s���ʒu�B��Q�v�f�F�ǂݎ�����r��
;;; 	�ďo���̃J�[�\���ʒu������ token �ƕs��v�ȏꍇ nil
(defun read-token-sexp-line (token)
  (when (looking-at (format nil "~a[ \\t]+" token))
	;; �r���ǎ�
	(multiple-value-bind (pnt-end sexp)
	  (read-sexp-from-buffer (match-end 0))

	  ;; �r���̏I�[�Ƃ��ĉ��s��ǂݔ�΂����ꍇ�́A���s���O�ֈʒu���ړ�
	  (when (eql (preceding-char) #\NewLine)
		(goto-char (1- pnt-end))
		) ;when

	  ;; ���s����i���̍s���j�̈ʒu�Ƃr����Ԃ�
	  (list (if (scan-buffer "\n" :tail t) (point) (point-max))
			sexp
			) ;list
	  ) ;multiple-value-setq
	) ;when
  )


;;; lhtml �i Razor �\�����܂� html �j�̃o�b�t�@��ǂݎ��A�����͌��ʂ�Ԃ�
;;; @return �����͌��ʂ��Z�N�V�����ʂɂ��� p-list
;;; 	(:require <���X�g> :use-package <���X�g> :main <���X�g>)
(defun parse-lhtml ()
  (goto-char (point-min))

  (do* ((lis-main nil)
		(lis-req nil) ;(:require �`) �̃��X�g
		(lis-use nil) ;(:use-package �`) �̃��X�g
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
	;; �i�󕶎���͒ǉ����Ȃ��j
	(when (< pnt-start pnt-end)
	  (push (buffer-substring pnt-start pnt-end) lis-main)
	  ) ;when

	;; @ �̎��̈ʒu
	(setf pnt-start (progn (forward-char) (point)))

	(cond
	 ;; @( �` )
	 ((looking-for "(")
	  (setf pnt-end (read-sexp-from-buffer pnt-start)) ;�J�����ʂ��r���̈ꕔ
	  (push `(:sexp ,(buffer-substring pnt-start pnt-end)) lis-main)
	  )
	 ;; @{ �` }
	 ((setf pnt-end (read-block-end-pos "{" "}"))
	  (push `(:block ,(buffer-substring (1+ pnt-start) pnt-end)) lis-main)
	  (incf pnt-end)
	  )
	 ;; @* �` *@
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
	  (push `(:require ,(cadr pnt-sexp)) lis-req) ;lis-req �� push

	  (setf pnt-end (car pnt-sexp))
	  )
	 ;; @using
	 ((setf pnt-sexp (read-token-sexp-line "using"))
	  (push `(:use-package ,(cadr pnt-sexp)) lis-use) ;lis-use �� push

	  (setf pnt-end (car pnt-sexp))
	  )
	 ;; ���̑�
	 (t
	  (error "�s���ȃg�[�N���ł��B")
	  )
	 ) ;cond

	(setf pnt-start pnt-end)
	(goto-char pnt-start)
	) ;do
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; lhtml �R���p�C���i=> lobj �j
;;;; lobj ���[�h
;;;; html �t�@�C���쐬�i lobj => html �j
;;;; 

;;; �o�b�t�@�����Z�b�g
;;; @param name �o�b�t�@��
;;; @param &optional buf �o�b�t�@
;;; @return �o�b�t�@���i�������Ƃ͈قȂ�\������j
(defun set-buffer-name (name &optional (buf (selected-buffer)))
  (rename-buffer name buf)
  (buffer-name buf)
  )

;;; �G���[�������ɔ�Ǐ��E�o�����Ɏw�肵���l��]��
;;; @param value-if-error �G���[�������̒l
;;; @param &body body
(defmacro ignore-error (value-if-error &body body)
  `(handler-case
	   (progn ,@body) ;progn
	 (error (c) ,value-if-error)
	 ) ;handler-case
  )

;;; �o�b�t�@���쐬���A�t�@�C�������[�h����B
;;; @param path 
;;; @param &key encoding �o�b�t�@�̕����R�[�h
;;; @param &key eol �o�b�t�@�̉��s�R�[�h
;;; @return �t�@�C�������݂��Ȃ������ꍇ nil
;;; 	�t�@�C�������݂����ꍇ�A�Ǎ���̃o�b�t�@
(defun create-and-read-file (path &key encoding eol)
  (let* ((buf-org (selected-buffer))
		 (name (file-namestring path)) ;�o�b�t�@��
		 (buf (create-new-buffer name))
		 )
	(block nil
	  (unwind-protect
		  (progn
			(set-buffer buf)

			(handler-case
				(read-file path)
			  ;; �t�@�C���Ǎ����s
			  (error (c)
				(delete-buffer buf) ;�o�b�t�@�폜
				(return nil)
				)
			  ) ;handler-case

			;; �����R�[�h����щ��s�R�[�h�̃Z�b�g
			(ignore-error nil
			  (when encoding (set-buffer-fileio-encoding encoding))
			  (when eol (set-buffer-eol-code eol))
			  ) ;ignore-error

			buf
			) ;progn

		;; �ȉ��A�N���[���A�b�v
		(set-buffer buf-org)
		) ;unwind-protect
	  ) ;block
	) ;let*
  )

;;; �g���q�݈̂قȂ�t�@�C���p�X���쐬
;;; @param type �g���q�i���s���I�h�͊܂܂Ȃ��j
;;; @param pathname-base ��b�Ƃ���t�@�C���p�X
;;; @return �t�@�C���g���q�̂ݑ�Q�����ƈقȂ�t�@�C���p�X
(defun merge-filetype (type pathname-base)
  (let* ((name (pathname-name pathname-base))
		 )
	(merge-pathnames (format nil "~a.~a" name type)
					 (directory-namestring pathname-base)
					 ) ;merge-pathnames
	) ;let*
  )

;;; lhtml-user �̃��C���֐���
;;; @param title 
;;; @return ���C���֐����i������j
(defun main-func-name (title)
  (format nil "~a.main" title)
  )

;;; �w�肵�����̂̃V���{�����擾
;;; �O���V���{���^�����V���{������v���Ȃ���΁A�V���{�����擾�ł��Ȃ��̂Œ��ӁB
;;; @param sym-name �p�b�P�[�W�����܂ރV���{�����B�i��jlisp:car
;;; @return �V���{���i�V���{�����Ȃ������ꍇ nil�j
(defun find-symbol* (sym-name)
  (when (string-match "^\\([^:]*\\)\\(::?\\)\\([^:]+\\)$" sym-name)
	(let* ((pre-pack (match-string 1))
		   (pack (if (< 0 (length pre-pack)) pre-pack "keyword"))
		   (name (match-string 3))
		   (access (nth (length (match-string 2)) '(nil :external :internal)))
		   (lis (ignore-error nil (multiple-value-list (find-symbol name pack))))
		   )
	  ;; �A�N�Z�X�C���q�i:external / :internal�j����v�����ꍇ�̂݁A�V���{����Ԃ�
	  (and lis (eql access (cadr lis)) (car lis))
	  ) ;let*
	) ;when
  )

;;; �w�肵�����̂̃V���{���Ɋ֘A�t����ꂽ�֐����擾
;;; @param sym-name �p�b�P�[�W�����܂ރV���{�����B�i��jlisp:car
;;; @return �֐�
(defun find-symbol-function (sym-name)
  (let* ((sym (find-symbol* sym-name))
		 (func (and sym (ignore-error nil (symbol-function sym))))
		 )
	(cond
	 ((null sym) (error "�V���{��������܂���B�F~a" sym-name)
	  )
	 ((null func) (error "�V���{���Ɋ֘A�t����ꂽ�֐�������܂���B�F~a" sym-name)
	  )
	 (t func
		)
	 ) ;cond
	) ;let*
  )


;;; lhtml �t�@�C�����R���p�C���i lhtml => lobj �j
;;; @param path-lhtml lhtml �t�@�C���̃p�X
;;; @return 
(defun compile-lhtml (path-lhtml)
  (let* ((path-lobj (merge-filetype "lobj" path-lhtml))
		 (name-lobj (file-namestring path-lobj))
		 (title-lobj (pathname-name path-lobj))
		 (buf-lhtml (create-and-read-file path-lhtml)) ;lhtml �t�@�C���Ǎ�
		 buf-lobj
		 parsed-lhtml
		 (func-print (find-symbol-function "lhtml-user:print-parsed-lhtml"))
		 )
	(unless buf-lhtml
	  (error "lhtml �t�@�C���Ǎ��Ɏ��s���܂����B")
	  ) ;unless

	;; lhtml ��́A�o�b�t�@�폜
	(set-buffer buf-lhtml)
	(setf parsed-lhtml (parse-lhtml))
	(delete-buffer buf-lhtml)

	;; lobj �p�o�b�t�@
	(setf buf-lobj (create-new-buffer name-lobj))
	(set-buffer buf-lobj)
	;; �t�@�C���p�X�Z�b�g
	(set-buffer-file-name path-lobj)
	;; �o�b�N�A�b�v�Ƃ�Ȃ��悤�ɂ���
	(make-local-variable 'make-backup-files)
	(setq make-backup-files nil)

	(with-output-to-buffer (buf-lobj)
	  (funcall func-print parsed-lhtml (main-func-name title-lobj))
	  ) ;with-output-to-buffer

	;; lobj �Z�[�u
	(save-buffer)
	;; lobj �p�o�b�t�@�폜
	(delete-buffer buf-lobj)

	t
	) ;let*
  )

;;; �p�b�P�[�W lhtml-user ���폜���čă��[�h
;;; �ߋ��̎��s���ꂽ lobj �̉e�����Ȃ����ړI
(defun reload-lhtml-user ()
  (delete-package :lhtml-user)
  (load-library "lhtml-user")
  )

;;; lobj �t�@�C���̃��[�h�E�]��
;;; @param path-lobj lobj �t�@�C���̃p�X
;;; @return 
(defun load-lobj (path-lobj)
  (let* ((title-lobj (pathname-name path-lobj))
		 )
	(when (equal title-lobj "")
	  (error "�g���q�݂̂̃t�@�C�����͋�����܂���B�F~a" title-lobj)
	  ) ;when

	;; �p�b�P�[�W lhtml-user ���폜���čă��[�h
	(reload-lhtml-user)

	;; �ߋ��̃��[�h��̏����N���A
	(funcall (find-symbol-function "lhtml-user:clear-info"))
	;; ���[�h
	(load path-lobj)
	;; ���[�h��̏��i�o�^���ꂽ�֐����j
	(funcall (find-symbol-function "lhtml-user:get-info"))
	) ;let*
  )

;;; html �t�@�C���쐬�i lobj => html �j
;;; lobj �t�@�C���œo�^����Ă�֐������s���āAhtml �t�@�C�����쐬
;;; lobj �t�@�C���̓��[�h�E�]���ς݂ł��邱��
;;; @param path-lobj lobj �t�@�C���̃p�X
;;; @param &key request ���N�G�X�g�i p-list �j
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
	;; �����R�[�h�Z�b�g
	(set-buffer-fileio-encoding *encoding-utf8*)
	;; ���s�R�[�h�Z�b�g
	(set-buffer-eol-code *eol-lf*)
	;; �t�@�C���p�X�Z�b�g
	(set-buffer-file-name path-html)
	;; �o�b�N�A�b�v�Ƃ�Ȃ��悤�ɂ���
	(make-local-variable 'make-backup-files)
	(setq make-backup-files nil)

	(unwind-protect
		(progn
		  (with-output-to-buffer (buf-html)
			;; lobj �̃��C���֐��ďo
			(funcall func request)
			) ;with-output-to-buffer

		  ;; html �Z�[�u
		  (save-buffer)
		  ) ;progn

	  ;; �N���[���A�b�v
	  ;; html �p�o�b�t�@�폜
	  (delete-buffer buf-html)
	  ) ;unwind-protect

	t
	) ;let*
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; lhtml �t�@�C������ html �t�@�C�����쐬
;;;; lhtml �t�@�C���A lobj �t�@�C���A html �t�@�C���̑��݋y�уt�@�C���������A
;;;; makefile �̗l�ɍŏ����̏����� html �t�@�C�����쐬
;;;; 

;;; �t�@�C���̍X�V�������擾
;;; @param path �t�@�C���p�X
;;; @return ���l
;;; 	[0]: ���j�o�[�T�������`���̒l
;;; 	[1]: �N [2]: �� [3]: �� [4]: ��  [5]: �� [6]: �b
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

;;; �ˑ��t�@�C�����V���������t�@�C�������݂��邩
;;; @param path-target �����t�@�C���̃p�X
;;; @param path-source �ˑ��t�@�C���̃p�X
;;; @return �^�̎��A�V���������t�@�C�������ɂ���
(defun newer-target-exist-p (path-target path-source)
  (let* ((ut-target (file-datetime path-target))
		 (ut-source (file-datetime path-source))
		 )
	(and ut-source ut-target (<= ut-source ut-target))
	) ;let*
  )

;;; �w�胂�W���[������ html �t�@�C�����쐬
;;; �K�v�ɉ����āAlhtml �̃R���p�C������� lobj �t�@�C���̃��[�h�E�]�����s���B
;;; @param module-name ���W���[�����ilhtml �t�@�C���̊g���q�������������j
;;; 	lhtml �t�@�C���� require �œǂݍ��߂�t�H���_�ɂ���K�v����B
;;; 	lobj ����� html �t�@�C���������t�H���_�ɍ쐬�����B
;;; @param &key request lobj ���s���̈����Bp-list �����B
;;; @return 
(defun make-html (module-name &key request)
  (let* ((lhtml-name (format nil "~a.lhtml" module-name))
		 (path-lhtml (find-load-path lhtml-name)) ;�t�@�C���� *load-path* ���猟�������t���p�X
		 path-lobj
		 path-html
		 )
	(unless path-lhtml
	  (error "lhtml �t�@�C����������܂���B")
	  ) ;unless

	;; lhtml => lobj
	(setf path-lobj (merge-filetype "lobj" path-lhtml))
	(unless (newer-target-exist-p path-lobj path-lhtml)
	  (compile-lhtml path-lhtml)
	  ) ;unless

	;; lobj => html
	;; request �̒l�ɂ��A�o�͂��ς��\������ɂ���̂ŁA
	;; �ŐV�t�@�C�����ǂ����ɍS��炸���s
	(setf path-html (merge-filetype "html" path-lhtml))
	(load-lobj path-lobj)                  ; lobj ���[�h
	(exec-lobj path-lobj :request request) ; lobj ���s

	(msgbox "html �t�@�C�����쐬���܂����B~%~a" path-html)

	t
	) ;let*
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; ��Razor �\������
;;;; 
;;;; ���C�����C�����F�P��̃v���p�e�B�^���\�b�h�^�t�B�[���h�ϐ��̒l�̏o��
;;;; 
;;;; <p>�v���p�e�B�F@DateTime.Now</p>
;;;; 
;;;; <p>���\�b�h�F  @DateTime.Now.ToLongDateString() ���X�y�[�X�ŏI��</p>
;;;; 
;;;; <p>�t�B�[���h�F@DateTime.MaxValue�F���S�p�R�����ŃR�[�h�I��</p>
;;;; 
;;;; <p>@("���݂̎����F" + DateTime.Now)</p>
;;;; 
;;;; ���R�[�h�E�u���b�N�F�����̕�����Ȃ�u���b�N�̋L�q
;;;; 
;;;; @{
;;;;   int year = DateTime.Now.Year;
;;;;   string msg = (DateTime.IsLeapYear(year)) ? "���邤�N" : "���N";
;;;; }
;;;; 
;;;; ���R�[�h�E�u���b�N���ł̕ϐ��╶����̏o��
;;;; 
;;;; @{
;;;;   string msg = DateTime.Now.ToLongDateString();
;;;;   <p>�R�[�h�E�u���b�N���̕ϐ����AHTML�^�O�t���ŏo�́F@msg</p>
;;;; }
;;;; 
;;;; @{
;;;;   string msg = DateTime.Now.ToLongDateString();
;;;;   <text>�R�[�h�E�u���b�N���̕ϐ����Atext�^�O�ŏo�́F@msg</text>
;;;; }
;;;; 
;;;; @{
;;;;   string msg = DateTime.Now.ToLongDateString();
;;;;   @:����1�s���A���̂܂܏o�͂����B<br>�ϐ����g����F@msg
;;;; }
;;;; 
;;;; ���R�����g
;;;; 
;;;; @* ���̕����̓R�����g�ɂȂ� *@
;;;; @*
;;;;   ���R�A�����s�ŋL�q���邱�Ƃ��ł���
;;;; *@
;;;; 
;;;; @{
;;;;   @* ���̕����̓R�����g�ɂȂ� *@
;;;;   // ���R�AC#�̃R�����g���g����
;;;;   /* C#�ɂ����镡���s�̃R�����g */
;;;; }
;;;; 
;;;; ���u@�v�̃G�X�P�[�v����
;;;; 
;;;; <p>@@isshiki���Ƀc�C�[�g���Ă��������B</p>
;;;; 
;;;; �����O��Ԃ̃C���|�[�g
;;;; 
;;;; @using System.Text.RegularExpressions;
;;;; @{
;;;;   string str= "����ɂ���";
;;;;   if (Regex.IsMatch(str, @"^\p{IsHiragana}*$"))
;;;;   {
;;;;     @:�S���A�Ђ炪�Ȃł��B
;;;;   }
;;;;   else
;;;;   {
;;;;     @:�Ђ炪�Ȃł͂Ȃ��������܂܂�Ă��܂��B
;;;;   }
;;;; }
;;;; 
;;;; ����������
;;;; 
;;;; @* if�R�[�h�E�u���b�N *@
;;;; @{int num = 1;}
;;;; @if (num > 0) {
;;;;   <p>���̐�</p>
;;;; } else if (num == 0) {
;;;;   <p>0</p>
;;;; } else {
;;;;   <p>���̐�</p>
;;;; }
;;;; 
;;;; @* switch�R�[�h�E�u���b�N *@
;;;; @{int i = 1;}
;;;; @switch (i) {
;;;;   case 1:
;;;;     <p>�u1�v�ł��B</p>
;;;;     break;
;;;;   default:
;;;;     <p>�u1�v�ȊO�ł��B</p>
;;;;     break;
;;;; }
;;;; 
;;;; ���J��Ԃ�����
;;;; 
;;;; @* for�R�[�h�E�u���b�N *@
;;;; @for (int i = 0; i < 3; i++) {
;;;;   <p>for���̃��[�v�́u@i�v��ڂł��B</p>
;;;; }
;;;; 
;;;; @* foreach�R�[�h�E�u���b�N *@
;;;; @{int[] items = {1,2,3};}
;;;; @foreach (var item in items) {
;;;;   <p>foreach���̃��[�v�ŏ��������ʍ��ڂ̒l�́u@item�v�ł��B</p>
;;;; }
;;;; 
;;;; @* while�R�[�h�E�u���b�N *@
;;;; @{var j = 0;}
;;;; @while (j < 3) {
;;;;   <p>for���̃��[�v�́u@j�v��ڂł��B</p>
;;;;   j++;
;;;; }
;;;; 
;;;; ���֐�
;;;; 
;;;; @functions {
;;;; 
;;;;   // ���\�b�h�̒�`
;;;;   int Sum(int a, int b)
;;;;   {
;;;;     return a + b;
;;;;   }
;;;; 
;;;;   // �v���p�e�B�̒�`
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
;;;; <p>�P�{�Q�� @Sum(1, 2)</p>
;;;; <p>�v���p�e�B�̒l�F @Num</p>
;;;; 

