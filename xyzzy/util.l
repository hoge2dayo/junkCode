;;;; �u Common Lisp ��Q�Łv�i�����o�Łj p245 ���
;;;; 1. provide
;;;; (1.5. defpackage)
;;;; 2. in-package
;;;; 3. shadow
;;;; 4. export
;;;; 5. require
;;;; 6. use-package
;;;; 7. import

;;;; 1. provide
(provide :util)

;;;; (1.5. defpackage)
(defpackage :util (:use :lisp :editor))

;;;; 2. in-package
(in-package :util)

;;;; 3. shadow

;;;; 4. export
(export '(error-when            ;�^�U�l���^�̏ꍇ�ɃG���[�𔭂���B�i�}�N���j
		  error-unless          ;�^�U�l���U�̏ꍇ�ɃG���[�𔭂���B�i�}�N���j
		  defalias              ;�֐����邢�̓}�N���̕ʖ���^����
		  cons*                 ;list* �̕ʖ�
		  require*              ;������ require �ďo
		  cats                  ;�����̕��������������������i�}�N���j
		  cat-string            ;�e���������������������
		  list-string           ;���X�g�̊e�v�f�����������悤�ȕ�������쐬
		  catfun                ;�֐��ďo��A��������֐��i�}�N���j
		  eql-oneof             ;�ǂꂩ�Ɉ�v���邩
		  cdrf                  ;�ĕϐ��̒l������ cdr �ɍX�V
		  orf                   ;�ĕϐ��̒l�������̒l or ���Ƃ����l�ɍX�V
		  andf                  ;�ĕϐ��̒l�������̒l and ���Ƃ����l�ɍX�V
		  with-gensyms          ;�w��̃V���{����(gensym)�ő�������let�t�H�[��
		  loop-while            ;�w���������������Ԃ̃��[�v

		  not-eql               ;(not (eql x y)) �Ɠ��`
		  and-eql               ;(and (eql �`) (eql �`) ...) �Ɠ��`
		  nth*                  ;�ʒu�𕡐��w��ł��� nth
		  nth-place*            ;nth* �Ŏ擾�ł���l�����Z��
		  first-char            ;������̍ŏ��̕������擾
		  last-char             ;������̍Ō�̕������擾
		  char*                 ;�����񒆂̕������擾
		  last1                 ;���X�g�̍Ō�̗v�f
		  length-0              ;���X�g���邢�͔z��̒������O��
		  length-0<             ;���X�g���邢�͔z��̒������O���߂�
		  length-n<             ;���X�g���邢�͔z��̒������w�蒴�߂�
		  char-number           ;�����ꕶ���𐔒l�ɕϊ�
		  char-number*          ;�����ꕶ���𐔒l�ɕϊ��B�S�p�����ɂ��Ή��B
		  char-shift            ;�w�肵�������̕����R�[�h�ɐ��l�����Z�����������擾
		  symbol-name-equal     ;�V���{���̒Ԃ肪��v���邩�i�啶���������̋�ʂȂ��j
		  symbol-name=          ;�V���{���̒Ԃ肪��v���邩�i�啶���������̋�ʂ���j
		  parse-integer*        ;�S�p������������ parse-integer
		  string-columns        ;������̕\����
		  into-range            ;����l�����������ꍇ�͏�����A�����������ꍇ�͉�����Ԃ��B
		  maxf                  ; place ����� �w�肵�����l�̍ő�l�� place ���X�V�i�}�N���j
		  minf                  ; place ����� �w�肵�����l�̍ŏ��l�� place ���X�V�i�}�N���j
		  max*                  ;nil ���w��\�� max
		  min*                  ;nil ���w��\�� min
		  sortf                 ;�ĕϐ��̒l���\�[�g���A�ĕϐ��֔��f�i�}�N���j
		  sort-list-onetime     ;�\�[�g��̒l���쐬���āA���X�g�\�[�g�B�i��j��֐��j
		  substitute-list       ;���X�g�̊e�v�f�̒u���������s���B�i��j��֐��j
		  substitute-tree       ;�؂̊e�v�f�̒u���������s���B�i��j��֐��j
		  stable-pairlis        ;�����Ɏw�肳�ꂽ�L�[�ƒl�̏��Ԃ�ۂ� pairlis
		  ignore-error          ;�G���[�𖳎�����

		  ;; --------

		  cond/                 ;
		  if/                   ;

		  ;; --------

		  @@                    ;�����Ŏg�p����Ă���V���{�� $1 �` $9 �������_���̃p�����[�^�ɂ���i�}�N���j

		  ;; --------

		  collector-push        ;�\���� collector �ɗv�f��ǉ��i�~�ρj
		  collector-append      ;�\���� collector �ɁA���X�g�̗v�f��ǉ��i�~�ρj
		  get-collector-list    ;�\���� collector �ɒ~�ς������ʁi���X�g�j���擾
		  with-collector        ;�i�}�N���j�\���� controller ���쐬���A�t�H�[���I�����ɒ~�ό��ʁi���X�g�j��Ԃ�

		  collect-while         ;�w��֐����^��Ԃ��v�f��v�f�Ƃ��郊�X�g

		  pme-1                 ;macroexpand-1 �̌��ʂ���

		  ;; --------

		  date-info             ;���s�������t�ɂ�����N�����Ȃǂ̏���������
		  )
		)

;;;; 5. require

;;;; 6. use-package

;;;; 7. import


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; 
;;;; 

;;; �^�U�l���^�̏ꍇ�ɃG���[�𔭂���B
;;; @param condition �^�U�l
;;; @param fmt �t�H�[�}�b�g������B
;;; @param &rest args �t�H�[�}�b�g������̈����B
;;; @return �^�U�l���U�̏ꍇ nil �B
(defmacro error-when (condition fmt &rest args)
  `(when ,condition (error ,fmt ,@args))
  )

;;; �^�U�l���U�̏ꍇ�ɃG���[�𔭂���B
;;; @param condition �^�U�l
;;; @param fmt �t�H�[�}�b�g������B
;;; @param &rest args �t�H�[�}�b�g������̈����B
;;; @return �^�U�l���^�̏ꍇ nil �B
(defmacro error-unless (condition fmt &rest args)
  `(unless ,condition (error ,fmt ,@args))
  )

;;; �֐����邢�̓}�N���̕ʖ���^����
;;; �i�p�r�j
;;; �E�����֐������ȗ����ď�������
;;; �E�ʃp�b�P�[�W�̈�֐��̂ݎg���������A���ׂ̈����� use-package �������Ȃ�
;;; @param alias �ʖ��i�V���{���j
;;; @param name ���ۂɌĂяo�����֐����邢�̓}�N���i�V���{���j
(defmacro defalias (alias name)
  `(defmacro ,alias (&rest args)
	 `(,',name ,@args)
	 )
  )
;;;; ��L�}�N����`�����̂悤�ɏ����Ă����܂������Ȃ��B
;;;; `(',,name ,@args)
;;;;
;;;; ���̂悤�ɓW�J����邽�߂Ɛ��������B�i�ϐ� name �̒l�� xxx �Ƃ���j
;;;; �i���̂P�j`(',,name ,@args) => `((quote ,,name) ,@args) => (cons (list 'quote xxx) args)
;;;; �i���̂Q�j`(,',name ,@args) => `(,(quote ,name) ,@args) => (cons (quote xxx) args)

;;; list* �̕ʖ�
(defalias cons* list*)

;;; ���� operator ���P�v�f�Ƃ��A���� lis-args �̗v�f���Q�����ȍ~�ɂ���
;;; ���X�g�̃��X�g���쐬�B
;;; �i�܂蓯���֐��̌ďo�̘A���ƂȂ�悤�ȃ��X�g���쐬�B�j
;;; �i��j
;;; (expand-args-list 'require '((:web-dictionary "�`") :html-edit))
;;; => ((require :web-dictionary "�`") (require :html-edit))
;;; @param operator
;;; @param lis-args ���v�f�� nil �̏ꍇ�A�Ή��̃��X�g�� (operator) �ɂȂ�B
;;; 	(operator nil) �ɂ������ꍇ�́A(nil) �Ǝw�肷��B
;;; @return 
(defun expand-args-list (operator lis-args)
  (mapcar #'(lambda (arg)
			  (cond
			   ((listp arg) (cons operator arg))
			   (t (list operator arg))
			   ) ;cond
			  ) ;lambda
		  lis-args
		  ) ;mapcar
  )

;;; ������ require �ďo
;;; �i��j
;;; (require* :web-dictionary :html-edit)
;;; �� (list (require :web-dictionary) (require :html-edit))
;;; @param lis-args require �ɓn���������e�v�f�Ƃ��郊�X�g
(defmacro require* (&rest lis-args)
  `(list ,@(expand-args-list 'require lis-args)) ;progn
  )

;;; �����̕��������������������
;;; �A���e������̖����ɂ͉��s��t��
;;; �i��j
;;; (cats "aaa" "bbb")
;;; => "aaa
;;;    bbb
;;;    "
;;; @param &rest lis-str ������萔�̃��X�g
;;; @return
(defmacro cats (&rest lis-str)
  (format nil "~{~a~%~}" lis-str)
  )

;;; ���������������������
;;; @param opt 
;;; 	:no-newline �F �e������̖����ɉ��s��t�����Ȃ��B
;;; 	:no-last-newline �F �Ō�̕�����ɂ̂݉��s��t�����Ȃ��B
;;; 	���̑� �F �e������̖����ɉ��s��t���B
;;; @param &rest lis-string 
;;; @return 
(defun cat-string (opt &rest lis-string)
  (let* (fmt
		 )
	(case opt
	  (:no-newline (setf fmt "~{~a~}")
	   ) ;:no-newline
	  (:no-last-newline (setf fmt "~{~a~^~%~}")
	   ) ;:no-last-newline
	  (otherwise (setf fmt "~{~a~%~}")
	   ) ;otherwise
	  ) ;case

	(format nil fmt lis-string)
	) ;let*
  )

;;; ���X�g�̗v�f�����������悤�ȕ�������쐬
;;; (format nil "~{~a~}" lis) �Ƃقړ����B
;;; @param lis
;;; @param &key recursive �v�f�����X�g�̏ꍇ�ɁA�ċA�I�ɏ������邩�ǂ����B
;;; 	�f�t�H���g�� nil �B
;;; @return 
(defun list-string (lis &key (recursive nil))
  (with-output-to-string (os)
	(labels ((sub (os lis recur)
			   (dolist (e lis)
				 (cond
				  ((not (consp e)) (princ e os))
				  ((and (consp e) recur) (sub os e recur))
				  ) ;cond
				 ) ;dolist
			   )
			 )
	  (sub os lis recursive)
	  ) ;labels
	) ;with-output-to-string
  )

;;; �֐��ďo��A��������֐��i�}�N���j
;;; fnc1 ����� fnc-n �̍Ō�������v�f�́A������ďo�ƂȂ�B
;;; �i��j
;;; (catfun hoge #'(lambda (x) (piyo x)) #'fuga))
;;; => #'(lambda (&rest args) (funcall hoge (funcall #'(lambda (x) (piyo x)) (apply #'fuga args))))
;;; @param fnc1 �Ō�ɓK�p����֐�
;;; @param &rest fnc-n 
(defmacro catfun (fnc1 &rest fnc-n)
  (let ((lis-fnc (reverse (cons fnc1 fnc-n)))
		)
	(cond (fnc-n (do ((lis-fnc (cdr lis-fnc) (cdr lis-fnc))
					  (result `(apply ,(car lis-fnc) args) `(funcall ,(car lis-fnc) ,result))
					  )
					 ((null lis-fnc) `#'(lambda (&rest args) ,result))
				   ) ;do
				 )
		  (t fnc1)
		  ) ;cond
	) ;let
  )

;;; �ǂꂩ�Ɉ�v���邩
;;; �i��j(eql-oneof x y z) => (or (eql x y) (eql x z))
;;; �����Ώۂ̒l���K�v���ɕ]�������_�� find �� member �ƈقȂ�
;;; @param val
;;; @param &rest args
(defmacro eql-oneof (val &rest args)
  ;; ��P�������֐��ďo�Ȃ�A�]���l��ϐ��ɑ������悤�ɓW�J
  (cond ((consp val) (let ((var (gensym)))
					   `(let ((,var ,val))
						  (or ,@(mapcar #'(lambda (e) `(eql ,var ,e)) args))
						  ) ;let
					   ) ;let
		 )
		;; ��P�������֐��ďo�łȂ��Ȃ�A�]���l��ϐ��ɑ�����Ȃ�
		(t `(or ,@(mapcar #'(lambda (e) `(eql ,val ,e)) args))
		   )
		) ;cond
  )

;;; �ĕϐ��̒l������ cdr �ɍX�V
;;; @param place �ĕϐ�
;;; @param &optional N
(defmacro cdrf (place &optional (N 1))
  `(setf ,place (nthcdr ,N ,place))
  )

;;; �ĕϐ��̒l�������̒l or ���Ƃ����l�ɍX�V
;;; @param place �ĕϐ�
;;; @param &rest args
(defmacro orf (place &rest args)
  `(setf ,place (or ,place ,@args))
  )

;;; �ĕϐ��̒l�������̒l and ���Ƃ����l�ɍX�V
;;; @param place �ĕϐ�
;;; @param &rest args
(defmacro andf (place &rest args)
  `(setf ,place (and ,place ,@args))
  )

;;; �w��̃V���{����(gensym)�ő�������let�t�H�[��
;;; �i��j
;;; (with-gensyms (a b) ...)
;;; => (let ((a (gensym))
;;;          (b (gensym))
;;;         )
;;;     ...
;;;    )
(defmacro with-gensyms (lis-var &body body)
  `(let ,(mapcar #'(lambda (var) `(,var (gensym))) lis-var)
	   ,@body
	   ) ;let
  )

;;; �w���������������Ԃ̃��[�v
(defmacro loop-while (condition &body body)
  `(loop
	 (unless ,condition (return))
	 ,@body
	 )
  )

;;; (not (eql x y)) �Ɠ��`
;;; @param x
;;; @param y
;;; @return 
(defun not-eql (x y) (not (eql x y)))

;;; (and (eql �`) (eql �`) ...) �Ɠ��`
;;; @param &rest args �v�f���͋����ł��邱�ƁB
(defmacro and-eql (&rest args)
  `(and ,@(do ((args args (cddr args))
			   (pairs nil (cons (list 'eql (car args) (cadr args)) pairs))
			   )
			  ((not (consp (cdr args))) (reverse pairs))
			) ;do
	) ;and
  )

;;; �ʒu�𕡐��w��ł��� nth
;;; @param a1 �i�����̐��𐧌�����ׂ̂��́j
;;; @param a2 �i�����̐��𐧌�����ׂ̂��́j
;;; @param &rest args
(defun nth* (a1 a2 &rest args)
  (let* ((all (reverse (list* a1 a2 args)))
		 (lis (car all))
		 (lis-n (cdr all))
		 )
	(dolist (n lis-n lis)
	  (when (null lis) (return))
	  (setf lis (nth n lis))
	  ) ;dolist
	) ;let*
  )

;;; nth* �Ŏ擾�ł���l�����Z���i�Z���� car �� nth* �Ŏ擾�����l�j
;;; �i�Z�p�I�Ȓ��j
;;; (nth n lis) �Ŏ擾�����l���i car �Ɂj���Z���́A(nthcdr n lis) �Ŏ擾�ł���B
;;; @param a1 �i�����̐��𐧌�����ׂ̂��́j
;;; @param a2 �i�����̐��𐧌�����ׂ̂��́j
;;; @param &rest args
(defun nth-place* (a1 a2 &rest args)
  (cond
   ((null args)
	(nthcdr a1 a2)
	)
   (t
	(let* ((lis (apply #'nth* a2 args))
		   )
	  (cond
	   ((null lis) lis)
	   (t (nthcdr a1 lis))
	   ) ;cond
	  ) ;let*
	)
   ) ;cond
  )

;;; ������̍ŏ��̕������擾
;;; @param str ������
;;; @return �ŏ��̕������邢�� nil
(defun first-char (str)
  (when (< 0 (length str))
	(char str 0)
	) ;when
  )

;;; ������̍Ō�̕������擾
;;; @param str ������
;;; @return �Ō�̕������邢�� nil
(defun last-char (str)
  (when (< 0 (length str))
	(char str (1- (length str)))
	) ;when
  )

;;; �����񒆂̕������擾�Bindex ���͈͊O�̏ꍇ nil
;;; @param str ������
;;; @return �������邢�� nil
(defun char* (str idx)
  (when (<= 0 idx (1- (length str)))
	(char str idx)
	) ;when
  )

;;; ���X�g�̍Ō�̗v�f
;;; �iPaul Graham �쐬�̂��̂Ɠ����j
;;; @param lis
;;; @return
(defun last1 (lis)
  (car (last lis))
  )

;;; ���X�g���邢�͔z��̒������O��
;;; @param o ���X�g���邢�͔z��
;;; @return 
(defun length-0 (o)
  (cond
   ((null o) t)
   ((consp o) nil)
   (t (zerop (length o)))
   ) ;cond
  )

;;; ���X�g���邢�͔z��̒������O���߂�
;;; @param o ���X�g���邢�͔z��
;;; @return 
(defun length-0< (o)
  (not (length-0 o))
  )

;;; ���X�g���邢�͔z��̒������w�蒴�߂�
;;; @param n ����
;;; @param o ���X�g���邢�͔z��
;;; @return ������ n ���߂��ǂ���
(defun length-n< (n o)
  (cond
   ((minusp n) t)
   ((consp o) (and (nthcdr n o) t))
   (t (< n (length o)))
   ) ;cond
  )

;;; 
(defvar *ch-code-0* (char-code #\0))
;;; 
(defvar *ch-code-upper-a* (char-code #\A))
;;; 
(defvar *ch-code-lower-a* (char-code #\a))

;;; �����ꕶ���𐔒l�ɕϊ�
;;; @param ch ����
;;; @param &optional radix ��B�ȗ��� 10 �B
;;; @return 
(defun char-number (ch &optional (radix 10))
  (let* ((code (char-code ch))
		 (num (cond
			   ((char<= #\0 ch #\9) (- code *code-0*))
			   ((char<= #\A ch #\Z) (+ 10 (- code *code-upper-a*)))
			   ((char<= #\a ch #\z) (+ 10 (- code *code-lower-a*)))
			   ) ;cond
			  )
		 )
	(error-unless (<= 2 radix 36) "�͈͊O�̊�ł��B : ~a" radix)

	(and num (< num radix) num)
	) ;let*
  )

;;; 
(defvar *ch-code-0f* (char-code #\�O))
;;; 
(defvar *ch-code-upper-af* (char-code #\�`))
;;; 
(defvar *ch-code-lower-af* (char-code #\��))

;;; �����ꕶ���𐔒l�ɕϊ��B�S�p�����ɂ��Ή��B
;;; @param ch ����
;;; @param &optional radix ��B�ȗ��� 10 �B
;;; @return 
(defun char-number* (ch &optional (radix 10))
  (let* ((code (char-code ch))
		 (num (cond
			   ((char<= #\0 ch #\9) (- code *ch-code-0*))
			   ((char<= #\A ch #\Z) (+ 10 (- code *ch-code-upper-a*)))
			   ((char<= #\a ch #\z) (+ 10 (- code *ch-code-lower-a*)))
			   ((char<= #\�O ch #\�X) (- code *ch-code-0f*))
			   ((char<= #\�` ch #\�y) (+ 10 (- code *ch-code-upper-af*)))
			   ((char<= #\�� ch #\��) (+ 10 (- code *ch-code-lower-af*)))
			   ) ;cond
			  )
		 )
	(error-unless (<= 2 radix 36) "�͈͊O�̊�ł��B : ~a" radix)

	(and num (< num radix) num)
	) ;let*
  )

;;; �w�肵�������̕����R�[�h�ɐ��l�����Z�����������擾
;;; �i��j(char-shift #\0 9) =>#\9
;;; @param ch-base ����
;;; @param offset ���Z����l�i�����j
(defun char-shift (ch-base offset)
  (code-char (+ (char-code ch-base) offset))
  )

;;; �V���{���̒Ԃ肪��v���邩�i�啶���������̋�ʂȂ��j
;;; @param s1 �V���{��
;;; @param s2 �V���{��
;;; @return 
(defun symbol-name-equal (s1 s2)
  (string-equal (symbol-name s1) (symbol-name s2))
  )

;;; �V���{���̒Ԃ肪��v���邩�i�啶���������̋�ʂ���j
;;; @param s1 �V���{��
;;; @param s2 �V���{��
;;; @return 
(defun symbol-name= (s1 s2)
  (string= (symbol-name s1) (symbol-name s2))
  )

;;; �S�p������������ parse-integer
;;; ��junk-allowed t �Ő������Ȃ��ꍇ�̓���� xyzzy �����B�G���[�ɂȂ炸 nil ��Ԃ��B
;;; @param string 
;;; @param &key start
;;; @param &key end
;;; @param &key junk-allowed �i������� xyzzy �����j
;;; @return 
(defun parse-integer* (string &key (start 0) (end nil) (radix 10) (junk-allowed nil))
  (let ((neg-flag nil)
		idx
		(result 0)
		)
	(flet ((space-p (ch) (find ch " �@"))
		   (return-value () (values (if neg-flag (- result) result) idx))
		   )
	  ;; end �� nil �̏ꍇ�A���l�ɕϊ�
	  (unless end
		(setf end (length string))
		) ;unless

	  ;; ������擪�̃X�y�[�X���΂�
	  (when junk-allowed
		(setf start (or (position-if-not #'space-p string :start start :end end) end))
		) ;when

	  ;; xyzzy �݊��ׁ̈A������ idx ��������
	  (setf idx start)

	  (when (< idx end)
		(cond
		 ;; �擪���v���X���H
		 ((find (char string idx) "+�{") (incf idx))
		 ;; �擪���}�C�i�X���H
		 ((find (char string idx) "-�|") (setf neg-flag t) (incf idx))
		 ) ;cond
		) ;when

	  (cond
	   ((<= end idx) (cond (junk-allowed (if (< start end) (values result idx) (values nil idx)))
						   (t (error "�s���Ȑ��l�̌`���ł�: ~s" string))
						   ) ;cond
		)
	   (t (do* ((ch)
				(n)
			   )
			  ((<= end idx) (return-value))
			(setf ch  (char string idx)
				  idx (1+ idx)
				  n (char-number* ch radix)
				  ) ;setf
			(unless n
			  (if junk-allowed
				  (return (return-value))
				(error "�s���Ȑ��l�̌`���ł�: ~s" string)
				) ;if
			  ) ;unless
			(setf result (+ (* radix result) n))
			) ;do*
		)
	   ) ;cond
	  ) ;flet
	) ;let
  )

;;; ������̕\����
;;; @param s ������
;;; @param &key start �J�n index
;;; @param &key end �I�� index + 1
;;; @return ���p�� 1 �Ƃ������B
(defun string-columns (s &key (start 0) (end nil))
  (reduce #'(lambda (w ch) (+ w (char-columns ch)))
		  s
		  :initial-value 0
		  :start start :end end
		  ) ;reduce
  )

;;; ����l�����������ꍇ�͏�����A�����������ꍇ�͉�����Ԃ��B
;;; @param n
;;; @param low ����
;;; @param up ���
(defun into-range (n low up)
  (cond
   ((< n low) low)
   ((< up n) up)
   (t n)
   ) ;cond
  )

;;; place ����� �w�肵�����l�̍ő�l�� place ���X�V
;;; @param place �ĕϐ�
;;; @param number ���l
;;; @param &rest more-numbers ���l�i0 �ȏ�j
;;; @return 
(defmacro maxf (place number &rest more-numbers)
  `(setf ,place (max ,place ,number ,@more-numbers))
  )

;;; place ����� �w�肵�����l�̍ŏ��l�� place ���X�V
;;; @param place �ĕϐ�
;;; @param number ���l
;;; @param &rest more-numbers ���l�i0 �ȏ�j
;;; @return 
(defmacro minf (place number &rest more-numbers)
  `(setf ,place (min ,place ,number ,@more-numbers))
  )

;;; nil ���w��\�� max
;;; @param number
;;; @param &rest more-numbers
;;; @return �����S�Ă�nil�̏ꍇ��nil
(defun max* (number &rest more-numbers)
  (let* ((lis-val (remove nil (cons number more-numbers)))
		 )
	(and lis-val (apply #'max lis-val))
	) ;let*
  )

;;; nil ���w��\�� min
;;; @param number
;;; @param &rest more-numbers
;;; @return �����S�Ă�nil�̏ꍇ��nil
(defun min* (number &rest more-numbers)
  (let* ((lis-val (remove nil (cons number more-numbers)))
		 )
	(and lis-val (apply #'min lis-val))
	) ;let*
  )

;;; �ĕϐ��̒l���\�[�g���A�ĕϐ��֔��f
;;; @param predicate �\�[�g����̂ɗp����֐�
;;; @param &rest lis-place �ĕϐ��i�����j
(defmacro sortf (predicate &rest lis-place)
  (let ((gvect (gensym))
		)
	`(let ((,gvect (stable-sort (vector ,@lis-place) ,predicate))
		   )
	   (setf ,@(do ((len (length lis-place))
					(lis nil (nconc lis (list (elt lis-place i) `(elt ,gvect ,i))))
					(i 0 (1+ i))
					)
				   ((<= len i) lis)
				) ;do
			 ) ;setf
	   ) ;let
	) ;let
  )

;;; �\�[�g��̒l���쐬���āA���X�g�\�[�g�B�i��j��֐��j
;;; �\�[�g�̊�Ƃ���l���쐬�A�\�[�g�A�쐬�����l���폜�B
;;; �i��j
;;; (sort-list-onetime #'(lambda (n) (rem n 2)) #'< '(1 2 3 4 5 6))
;;; => (2 4 6 1 3 5)
;;; @param fnc-keyvalue �\�[�g�̊�Ƃ���l�����߂�֐��B
;;; 	��P�����F���X�g�̊e�v�f
;;; @param fnc-compare �\�[�g���邽�߂̔�r�֐�
;;; @param lis �\�[�g���郊�X�g
;;; @return 
(defun sort-list-onetime (fnc-keyvalue fnc-compare lis)
  (map 'list #'cdr
	   (stable-sort (map 'vector #'(lambda (e) (cons (funcall fnc-keyvalue e) e))
						 lis
						 ) ;map
					fnc-compare
					:key #'car
					) ;sort
	   ) ;map
  )

;;; ���X�g�̊e�v�f�̒u���������s���B�i��j��֐��j
;;; �u���������s�����̔��f�ƁA�u��������̒l���֐��ŋ��߂�B
;;; �i��j
;;; (substitute-list #'1+ #'numberp '(a 1 2 (12 13)))
;;; => (a 2 3 (12 13))
;;; @param fnc-new-item �֐� fnc-test ���^��Ԃ����ꍇ�ɁA
;;; 	�u��������̒l�����߂�֐�
;;; @param fnc-test �v�f��u�������邩���f����֐��B
;;; @param lis ���X�g
;;; @return 
(defun substitute-list (fnc-new-item fnc-test lis)
  (mapcar #'(lambda (e) (if (funcall fnc-test e)
							(funcall fnc-new-item e)
						  e
						  ) ;if
			  ) ;lambda
		  lis
		  ) ;mapcar
  )

;;; �؂̊e�v�f�̒u���������s���B�i��j��֐��j
;;; �u���������s�����̔��f�ƁA�u��������̒l���֐��ŋ��߂�B
;;; �v�f�����X�g�ŁA�֐� fnc-test ���U��Ԃ����ꍇ�́A
;;; ���̃��X�g�̊e�v�f�ɂ��Ă��ċA�I�ɏ������s���B
;;; �i��j
;;; (substitute-tree #'1+ #'numberp '(a 1 2 (12 13)))
;;; => (a 2 3 (13 14))
;;; @param fnc-new-item �֐� fnc-test ���^��Ԃ����ꍇ�ɁA
;;; 	�u��������̒l�����߂�֐�
;;; @param fnc-test �v�f��u�������邩���f����֐��B
;;; @param tree ��
;;; @return 
(defun substitute-tree (fnc-new-item fnc-test tree)
  (mapcar #'(lambda (e) (cond
						 ((funcall fnc-test e) (funcall fnc-new-item e))
						 ((consp e) (substitute-tree fnc-new-item fnc-test e))
						 (t e)
						 ) ;cond
			  ) ;lambda
		  tree
		  ) ;mapcar
  )

;;; �����Ɏw�肳�ꂽ�L�[�ƒl�̏��Ԃ�ۂ� pairlis
;;; @param keys
;;; @param data
;;; @param &optional a-list
;;; @return a-list
(defun stable-pairlis (keys data &optional (a-list nil))
  (nconc (mapcar #'cons keys data) a-list)
  )

;;; �G���[�𖳎�����
;;; @param &body body
(defmacro ignore-error (&body body)
  `(handler-case
	   (progn ,@body)
	 (error (c) )
	 ) ;handler-case
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; cond/ / if/
;;;;

;;; ���X�g�����̗v�f�ŕ�������B�i��j��j
;;; @param lis �����Ώۂ̃��X�g�B
;;; @param &key fnc �敪�̈�𔻕ʂ���֐��B
;;; @param &key initial ��ԍŏ��̋敪�̈�B
;;; @return ���� fnc �Ŕ��ʂ��ꂽ�v�f�Ŏn�܂郊�X�g�̃��X�g
(defun split-list-by (lis &key (fnc #'keywordp) (initial nil))
  (util:with-collector (d)
	(util:with-collector (c)
	  (util:collector-push initial c)
	  (do* ((e)
			)
		  ((null lis))
		(when (funcall fnc (setf e (pop lis)))
		  (util:collector-push (util:get-collector-list c) d)
		  ) ;when

		(util:collector-push e c)
		) ;do*

	  (util:collector-push (util:get-collector-list c) d)
	  ) ;with-collector
	) ;with-collector
  )

;;; ���X�g���w��̃V���{���ŕ���
;;; @param lis-symbol �敪�̈�Ƃ���V���{���̃��X�g
;;; @param lis �����Ώۂ̃��X�g
;;; @param &key initial ��ԍŏ��̋敪�̈�B
;;; @return 
(defun split-list-by-symbol (lis-symbol lis &key (initial nil))
  (split-list-by lis :fnc #'(lambda (e) (find e lis-symbol))
						 :initial initial
						 ) ;split-list-into-chunk
  )

;;; cond �� (p e1 e2 ... en) ��W�J����悤�ɋL�q���� cond
;;; (p e1 e2 ... en) �� :case p e1 e2 ... en
;;; @param &rest lis 
;;; @return 
(defmacro cond/ (&rest lis)
  (let* ((lis-chunk (split-list-by-symbol '(:case) lis))
		 )
	;; �ŏ��� :case ��Y��Ă�����G���[�i�e�؁j
	(when (cdar lis-chunk)
	  (error ":case ���w�肳��Ă��܂���B")
	  ) ;when
	(pop lis-chunk) ;�ŏ��̗v�f��j��

	`(cond
	  ,@(mapcar #'(lambda (chunk)
					(unless (cdr chunk)
					  (error ":case �߂� predicate ������܂���B")
					  ) ;unless
					`(,(cadr chunk) ,@(cddr chunk))
					) ;lambda
				lis-chunk
				) ;mapcar
	  ) ;cond
	) ;let*
  )

;;; if �� then �߂� else �߂� :else �ŋ�؂� if �B
;;; @param test 
;;; @param &rest lis 
;;; @return 
(defmacro if/ (test &rest lis)
  (let* ((pos-else (position :else lis))
		 (then (subseq lis 0 pos-else))
		 (else (if pos-else (nthcdr (1+ pos-else) lis)))
		 )
	(flet ((sexp-1 (lis) (if (cdr lis) `(progn ,@lis) (car lis)))
		   )
	  `(if ,test ,(sexp-1 then) ,@(if else `(,(sexp-1 else))))
	  ) ;flet
	) ;let*
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; @@
;;;; 

;;; �p�����[�^�Ɏg����V���{�����H
;;; @param sym 
;;; @return 
(defun parameter-symbol-p (sym &aux (name (symbol-name sym)))
  (and (eql (length name) 2)
	   (eql (char name 0) #\$)
	   (char<= #\1 (char name 1) #\9)
	   ) ;and
  )

;;; ���X�g�Ɋ܂܂��p�����[�^�Ɏg����V���{�������W�B
;;; �ŏ�ʗv�f�݂̂łȂ��A�������X�g�������W�ΏہB
;;; @param lis 
;;; @return 
(defun collect-parameter-symbol (lis)
  (let* ((result nil)
		 )
	(labels ((collect (lis)
			   (dolist (e lis)
				 (cond ((and e (symbolp e) (parameter-symbol-p e)) (pushnew e result))
					   ((consp e) (collect e))
					   ) ;cond
				 ) ;dolist
			   )
			 )
	  (collect lis)
	  result
	  ) ;labels
	) ;let*
  )

;;; �V���{���̃p�����[�^�ԍ�
;;; @param sym $1 �` $9
;;; @return 1 �` 9
(defun parameter-number (param)
  (parse-integer (symbol-name param) :start 1)
  )

;;; �p�����[�^���g�p���������_���X�g�B
;;; ���݂��Ȃ��ԍ��ɊY�����鉼������ gensym �Ő��������V���{���B
;;; @param lis-param �p�����[�^�i $1 �` $9 �j��v�f�Ƃ��郊�X�g
;;; @return 
(defun lambda-list-for-@@ (lis-param)
  (let* ((ary (make-array 10))
		 num
		 (num-max 0)
		 )
	;; �p�����[�^�i $1 �` $9 �j��ԍ��ɑΉ�����ӏ��ɃZ�b�g
	(dolist (param lis-param)
	  (setf num (parameter-number param)
			num-max (max num-max num)
			(aref ary num) param
			) ;setf
	  ) ;dolist

	;; �󂫔ԂɂȂ��Ă���p�����[�^�ԍ��̉ӏ��� gensym ���Z�b�g
	(do* ((idx 1 (1+ idx))
		  )
		((< num-max idx))
	  (unless (aref ary idx)
		(setf (aref ary idx) (gensym))
		) ;unless
	  ) ;do*

	(coerce (subseq ary 1 (1+ num-max)) 'list)
	) ;let*
  )

;;; �����Ŏg�p����Ă���V���{�� $1 �` $9 �������_���̃p�����[�^�ɂ���
;;; �i��j
;;; (@@ (hoge $1 a))  => #'(lambda ($1) (hoge $1 a))
;;; �i������ $$ �Ƃ������O���������A LispWorks �ɓ����̃V���{��������̂ŕύX�j
;;; @param &rest lis-sexp
(defmacro @@ (&rest lis-sexp)
  (let* ((lis-param (collect-parameter-symbol lis-sexp)
					)
		 )
	;; ������A���o�����p�����[�^�̌����ȃ`�F�b�N�͂��Ă��Ȃ��B
	;; �@�i�ǉ����ׂ��`�F�b�N�̗�j
	;; �@�E�������ʃp�b�P�[�W�̃V���{�������݂��邩�ǂ���
	;; �@�E�L�[���[�h�łȂ����ǂ���
	`#'(lambda ,(lambda-list-for-@@ lis-param) ,@lis-sexp)
	) ;let*
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; collector
;;;; 

;;;; ********************************************************************************
;;;; ���J���̒� (collector)
;;;; �O����������ϐ���ی삷��ׁA�����N���[�W�����g�����������Ă������A
;;;; �p�b�P�[�W�𕪂��Ă��邱�Ƃ���A�\���̂ɂ��Ă����͂Ȃ��ƋC�Â��āA
;;;; ���̂悤�Ȏ����ɕύX�����B
;;;; �i����������V���{���ɃA�N�Z�X���Ȃ�������Ȃ��j
;;;; ********************************************************************************

;;; ���X�g�����ɗv�f�ǉ�����ׂ̍\����
;;; ���X���b�g�̐錾�́A(head (list nil)) (tail head)�Ə����������A
;;; �@initform�ŕʃX���b�g���Q�Ƃł���̂��s���Ȃ̂ŁA�����_���X�g��&aux�Ŏw��
(defstruct (collector
			(:constructor make-collector (&aux (head (list nil)) (tail head)))
			)
  head
  tail
  )

;;; �\���� collector �ɗv�f��ǉ��i�~�ρj
;;; @param e �ǉ�����v�f
;;; @param c �\����collector
;;; @return ����e
(defun collector-push (e c)
  (let* ((new-node (list e))
		 )
	(setf (cdr (collector-tail c)) new-node
		  (collector-tail c) new-node
		  ) ;setf
	e
	) ;let*
  )

;;; �\���� collector �ɁA���X�g�̗v�f��ǉ��i�~�ρj
;;; @param lis �ǉ�����v�f���܂ރ��X�g
;;; @param c �\����collector
;;; @return ����lis
(defun collector-append (lis c)
  (dolist (e lis)
	(collector-push e c)
	) ;dolist
  )

;;; �\���� collector �ɒ~�ς������ʁi���X�g�j���擾
;;; �~�ς͏��������
;;; @param c �\���� collector
;;; @return �~�ό���
(defun get-collector-list (c)
  (prog1
	  (cdr (collector-head c))

	;; �~�ς�����
	(setf (cdr (collector-head c)) nil
		  (collector-tail c) (collector-head c)
		  ) ;setf
	) ;prog1
  )

;;; �\���� controller ���쐬���A�t�H�[���I�����ɒ~�ό��ʁi���X�g�j��Ԃ�
;;; �i��j
;;; (with-collector (c) (collector-push 'a c) (collector-push 'b c))
;;; 	=> (a b)
;;; ���t�H�[�����Œ~�ό��ʂ𓾂����ꍇ�́Aget-collector-list ���g���΂悢
;;; @param var collector���Z�b�g����ϐ��i�V���{���j
;;; @param &body body ���s�t�H�[��
;;; @return controller �ɒ~�ς��ꂽ�v�f�i���X�g�j
(defmacro with-collector ((var) &body body)
  `(let* ((,var (make-collector))
		  )
	 ,@body
	 (get-collector-list ,var)
	 ) ;let*
  )

;;; �w��֐����^��Ԃ��v�f��v�f�Ƃ��郊�X�g
;;; mapcar �ƈقȂ�A�w��֐����U��Ԃ��Ǝ��W���I������B
;;; @param predicate �v�f�����ʂɒǉ����邩�ǂ����𔻒肷��֐��B
;;; @param lis 
;;; @param &key key predicate �֓n���O�ɁA���X�g�̗v�f�֓K�p����֐�
;;; @param &key fnc-value �ǉ�����l�֓K�p����֐��B
;;; 	��P�����F���X�g�̗v�f
;;; 	��Q�����F predicate ���Ԃ����l
;;; @return 
(defun collect-while (predicate lis &key (key #'identity) (fnc-value #'(lambda (e &optional p) e)))
  (let* ((fnc-p (catfun predicate key))
		 p
		 )
	(with-collector (c)
	  (dolist (e lis)
		(unless (setf p (funcall fnc-p e)) (return))
		(collector-push (funcall fnc-value e p) c)
		) ;dolist
	  ) ;with-collector
	) ;let*
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; 
;;;; 
;;;; 

;;; macroexpand-1 �̌��ʂ���
;;; macroexpand-1 �𒼐ڌĂяo���ƁA�߂�l�Ɋ܂܂�� eq �ȃI�u�W�F�N�g��
;;; �ς킵���]�v�ȕ\���������B�����h���ׂɗp����B
;;; @param expr �W�J���鎮
(defmacro pme-1 (expr)
  `(progn
	 (print (macroexpand-1 ',expr))
	 nil
	 )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; 
;;;; 
;;;; 


;;; �P���̕b��
(defconstant +sec-of-min+ 60)
;;; �P���Ԃ̕b��
(defconstant +sec-of-hour+ (* 60 +sec-of-min+))
;;; �P���̕b��
(defconstant +sec-of-day+ (* 24 +sec-of-hour+))


;;; ���s�������t�ɂ�����N�����Ȃǂ̏���������
;;; �L�[�F:year, :month, :day, :day-of-week, :hour, :min, :sec
;;; (�N�i����j �� �� �j���i0=���j�j �� �� �b)
;;; @param &optional add-days �����̃I�t�Z�b�g
;;; @return plist
(defun date-info (&optional (add-days 0))
  (multiple-value-bind (sec min hour day mon year day-of-week daylight tz)
	  (decode-universal-time (+ (get-universal-time) (* +sec-of-day+ add-days)))
	(list :year year :month mon :day day :day-of-week day-of-week
		  :hour hour :min min :sec sec
		  ) ;list
	) ;multiple-value-bind
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; let** 
;;;; xyzzy �̕s��ɂ��Alet* �ŃO���[�o���ϐ��̍đ������w�肵���ꍇ�ɁA
;;;; �ϐ��̎Q�Ƃ����܂������Ȃ��B�������������ׂ� let*
;;;; 

;;; �A�������v�f���O���[�v�l�ŃO���[�s���O
;;; �i��j
;;; (split-by-group #'evenp '(1 3 4 8 10 5))
;;; => ((nil 1 3) (t 4 8 10) (nil 5))
;;; @param fnc �O���[�v�l�i�n�b�V���l�̂悤�Ȃ��́j�����߂�֐�
;;; @param lis
;;; @param &key test �O���[�v�l�̈�v�𔻒肷��֐�
;;; @return ���X�g�B�e�v�f�� (<�O���[�v�l> <�v�f> ... <�v�f>)
(defun split-by-group (fnc lis &key (test #'eql))
  (when lis
	(do* ((lis lis (cdr lis))
		  (lis-cd (mapcar fnc lis) (cdr lis-cd))
		  (cd-last nil cd)
		  (cd (car lis-cd) (car lis-cd))
		  (rtest t (funcall test cd-last cd))
		  (grp nil)
		  (lis-grp nil)
		  )
		((null lis)
		 (push (cons cd-last (reverse grp)) lis-grp)
		 (reverse lis-grp)
		 )
	  (unless rtest
		(push (cons cd-last (reverse grp)) lis-grp)
		(setf grp nil)
		) ;unless

	  (push (car lis) grp)
	  ) ;do*
	) ;when
  )

;;; �O���[�o���ϐ��𕡐��� let* �ɕ����đ������� let*
;;; �i xyzzy �̕s��΍�j
;;; @param lis-var 
;;; @param &body body
(defmacro let** (lis-var &body body)
  (labels ((make-form (llis-var body)
			 (cond
			  ((null llis-var) body)
			  ((null (cdr llis-var)) `(let* ,(car llis-var)
										,@(make-form (cdr llis-var) body)
										) ;let*
			   )
			  (t `(let* ,(car llis-var)
					,(make-form (cdr llis-var) body)
					) ;let*
				 )
			  ) ;cond
			 )
		   )
	(make-form
	 ;; mapcan �ŃO���[�o���ϐ��̑����͌ʂɕ���
	 (mapcan #'(lambda (grp)
				 (if (car grp) (mapcar #'list (cdr grp)) (list (cdr grp)))
				 ) ;lambda
			 ;; �O���[�o���ϐ��ƃ��[�J���ϐ��ŃO���[�s���O
			 (split-by-group #'(lambda (e) (boundp (if (symbolp e) e (car e)))) lis-var)
			 ) ;mapcan
	 body
	 ) ;make-form
	) ;labels
  )

