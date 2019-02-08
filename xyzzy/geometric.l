;;;; require - provide - defpackage - in-package - export

;;(require )

(provide :geometric)

(defpackage :geometric
  (:nicknames :geo)
  (:use :lisp :editor)
  )

(in-package :geometric)

(export '(create-size size-w size-h
		  size-p
		  copy-size*
		  enlarge-size* enlarge-size

		  size-rect max-size

		  create-rect rect-x rect-y rect-w rect-h
		  rect-l rect-t ;�}�N��
		  rect-r rect-b
		  rect-p
		  copy-rect*
		  rect-size or-rect and-rect cut-rect
		  )
		) ;export


;;;; 
;;;; ���T�C�Y
;;;; 

;;; �T�C�Y�������\���́i���E�����j
(defstruct (size
			(:constructor make-size (&key w h))
			(:constructor create-size (w h))
			)
  (w 0)
  (h 0)
  )

;;; �T�C�Y�̕���
;;; nil �������� copy-size
;;; @param siz �������̃T�C�Y
;;; @return ���������T�C�Y
(defun copy-size* (siz)
  (if (size-p siz) (copy-size siz) siz)
  )

;;; �T�C�Y���w��ʑ傫������
;;; ���j��I
;;; @param siz �ύX�Ώۂ̃T�C�Y�i���X���b�g�l���ύX�����j
;;; @param incw ���ɉ��Z����l
;;; @param inch �����ɉ��Z����l
;;; @return ���� siz
(defun enlarge-size* (siz incw inch)
  (cond
   ((null siz) (setf siz (make-size :w incw :h inch)))
   ((size-p siz)
	(incf (size-w siz) incw)
	(incf (size-h siz) inch)
	)
   ) ;cond
	siz
  )

;;; �w��ʑ傫�������T�C�Y���擾
;;; ����j��I
;;; @param siz ��Ƃ���T�C�Y�i�����������j
;;; @param incw ���ɉ��Z����l
;;; @param inch �����ɉ��Z����l
;;; @return ���������T�C�Y
(defun enlarge-size (siz incw inch)
  (enlarge-size* (copy-size* siz) incw inch)
  )

;;; �T�C�Y�����`���쐬
;;; @param siz ��`�̕��y�э����Ƃ���T�C�Y
;;; @param &optional x ��`�� X ���W
;;; @param &optional y ��`�� Y ���W
;;; @return ��`
(defun size-rect (siz &optional (x 0) (y 0))
  (make-rect :x x :y y :w (+ x (size-w siz)) :h (+ y (size-h siz)))
  )

;;; �w�肵���T�C�Y����A�ł��傫�����ƍł��傫��������p�����T�C�Y
;;; @param siz �T�C�Y
;;; @param &rest sizes �T�C�Y�i�����j
;;; @return �ł��傫�����ƍł��傫��������p�����T�C�Y
(defun max-size (siz &rest sizes)
  (cond
   ;; p->e :�T�C�Y�P��
   ((null sizes) (copy-size* siz))
   ;; p->e :����������
   (t (reduce #'(lambda (siz0 siz1)
				  (when siz1
					(setf (size-w siz0) (max (size-w siz0) (size-w siz1))
						  (size-h siz0) (max (size-h siz0) (size-h siz1))
						  ) ;setf
					) ;when
				  siz0
				  ) ;lambda
			  sizes
			  :initial-value (or siz (create-size 0 0))
			  ) ;reduce
	  )
   ) ;cond
  )

;;;; 
;;;; ����`
;;;; 

;;; ��`�������\���́i�ʒu�{���E�����j
(defstruct (rect
			(:constructor make-rect (&key x y w h))
			(:constructor create-rect (x y w h))
			)
  (x 0)
  (y 0)
  (w 0)
  (h 0)
  )

;;; ��`�̍��[�irect-x �̕ʖ��j
;;; @param rct ��`
;;; @return ���[
(defmacro rect-l (rct) `(rect-x ,rct))

;;; ��`�̏�[�irect-y �̕ʖ��j
;;; @param rct ��`
;;; @return ��[
(defmacro rect-t (rct) `(rect-y ,rct))

;;; ��`�̉E�[
;;; @param rct ��`
;;; @return �E�[
(defun rect-r (rct)
  (and rct (+ (rect-x rct) (rect-w rct)))
  )

;;; ��`�̉��[
;;; @param rct ��`
;;; @return ���[
(defun rect-b (rct)
  (and rct (+ (rect-y rct) (rect-h rct)))
  )

;;; ��`�̕��E����
;;; @param rct ��`
;;; @return �񎟌��T�C�Y�i�\���� size�j
(defun rect-size (rct)
  (if (rect-p rct) (make-size :w (rect-w rct) :h (rect-h rct)) rct)
  )

;;; ��`�̕���
;;; nil �������� copy-rect
;;; @param rct �������̋�`
(defun copy-rect* (rct)
  (if (rect-p rct) (copy-rect rct) rct)
  )

;;; �w�肵���S�Ă̋�`���܂ދ�`
;;; @param rct ��`
;;; @param &rest rcts ��`�i�����j
;;; @return �w�肵���S�Ă̋�`���܂ޑ傫���̋�`
(defun or-rect (rct &rest rcts)
  (cond
   ;; p->e :��`�P��
   ((null rcts) (copy-rect* rct))
   ;; p->e :��`������
   (t (reduce #'union-rect rcts :initial-value rct))
  ) ;cond
)

;;; �Q��`���܂ދ�`
;;; ��nil �͑傫�� 0 �̋�`�ƌ��Ȃ�
;;; @param rct0 ��`
;;; @param rct1 ��`
;;; @return �����̋�`���܂ޑ傫���̋�`
(defun union-rect (rct0 rct1)
  (cond
   ;; p->e
   ((null rct0) (copy-rect* rct1))
   ;; p->e
   ((null rct1) (copy-rect* rct0))
   ;; p->e
   (t (let* ((x (min (rect-x rct0) (rect-x rct1)))
			 (y (min (rect-y rct0) (rect-y rct1)))
			 (r (max (rect-r rct0) (rect-r rct1)))
			 (b (max (rect-b rct0) (rect-b rct1)))
			 (w (- r x))
			 (h (- b y))
			)
		(make-rect :x x :y y :w w :h h) ;make-rect
	  ) ;let*
   )
  ) ;cond
)

;;; �w�肵���S�Ă̋�`���d�Ȃ�����`
;;; @param rct ��`
;;; @param &rest rcts ��`�i�����j
;;; @return �w�肵���S�Ă̋�`���d�Ȃ��������̋�`
(defun and-rect (rct &rest rcts)
  (cond
   ;; p->e :��`�P��
   ((null rcts) (copy-rect* rct))
   ;; p->e :��`������
   (t (reduce #'intersection-rect rcts :initial-value rct))
  ) ;cond
)

;;; �Q��`�̏d�Ȃ��������ƂȂ��`
;;; ��nil �͑傫�� 0 �̋�`�ƌ��Ȃ�
;;; @param rct0 ��`
;;; @param rct1 ��`
;;; @return �����̋�`���d�Ȃ��������̋�`
(defun intersection-rect (rct0 rct1)
  (cond
   ;; p->e
   ((or (null rct0) (null rct1)) nil)
   ;; p->e
   (t (let* ((x (max (rect-x rct0) (rect-x rct1)))
			 (y (max (rect-y rct0) (rect-y rct1)))
			 (r (min (rect-r rct0) (rect-r rct1)))
			 (b (min (rect-b rct0) (rect-b rct1)))
			 (w (- r x))
			 (h (- b y))
			)
		(if (and (< 0 w) (< 0 h))
			(make-rect :x x :y y :w w :h h) ;make-rect
			;; else
			nil
		)
	  ) ;let*
   )
  ) ;cond
)

;;; ��`�̒[���w��̕��ŕ�������
;;; ��P�����́A:left, :top, :right, :bottom �̂����ꂩ
;;; �i��j
;;; (cut-rect :bottom 10 (make-rect :x 10 :y 20 :w 30 :h 40))
;;; => #S(rect x 10 y 20 w 30 h 30)
;;;    #S(rect x 10 y 50 w 30 h 10)
;;; @param edge :left / :top / :right / :bottom
;;; @param len ���� edge �Ŏ������ӂ��番�����钷��
;;; @param rct ���������`
;;; @return ���l
;;; 	[0] :<���f���ꂽ��`>
;;; 	[1] :<���f������`>
(defun cut-rect (edge len rct)
  (cond
   ;; p->e
   ((null rct) (list nil nil))
   ;; p->e
   (t
	(let* ((x (rect-x rct))
		   (y (rect-y rct))
		   (w (rect-w rct))
		   (h (rect-h rct))
		  )
	  (case edge
		;; left
		(:left (values
				(and (< len w)
					(make-rect :x (+ x len) :y y :w (- w len) :h h)
				) ;and
				(make-rect :x x :y y :w len :h h)
			   ) ;values
		)
		;; top
		(:top (values
			   (and (< len h)
				   (make-rect :x x :y (+ y len) :w w :h (- h len))
			   ) ;and
			   (make-rect :x x :y y :w w :h len)
			  ) ;values
		)
		;; right
		(:right (values
				 (and (< len w)
					  (make-rect :x x :y y :w (- w len) :h h)
				 ) ;and
				 (make-rect :x (- (+ x w) len) :y y :w len :h h)
				) ;values
		)
		;; bottom
		(:bottom (values
				  (and (< len h)
					   (make-rect :x x :y y :w w :h (- h len))
				  ) ;and
				  (make-rect :x x :y (- (+ y h) len) :w w :h len)
				 ) ;values
		)
		;; otherwise
		(otherwise (values
					nil
					rct
					) ;values
				   )
		) ;case
	  ) ;let*
	)
   ) ;cond
  )

