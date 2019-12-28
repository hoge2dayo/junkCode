;;;; 「 Common Lisp 第２版」（共立出版） p245 より
;;;; 1. provide
;;;; (1.5. defpackage)
;;;; 2. in-package
;;;; 3. shadow
;;;; 4. export
;;;; 5. require
;;;; 6. use-package
;;;; 7. import

;;;; 1. provide
(provide :collector)

;;;; (1.5. defpackage)
(defpackage :collector (:use :common-lisp))

;;;; 2. in-package
(in-package :collector)

;;;; 3. shadow

;;;; 4. export
(export '(collector-push
          collector-append
          get-collector-list
          with-collector  ;マクロ
          )
        ) ;export

;;;; 5. require

;;;; 6. use-package

;;;; 7. import


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; 
;;;; ■collector
;;;; 
;;;; リストを構築する際、一般にループ内で cons を繰り返し、
;;;; 最後に要素の蓄積の reverse が行われる。
;;;; それとは異なり、リストの末尾に要素を蓄積していく仕組み。
;;;; 
;;;; ●例
;;;; (with-collector (c)
;;;;   (dotimes (i 5)
;;;;     (collector-push i c)
;;;;    )
;;;;   (collector-append (list :a :b) c)
;;;;  )
;;;;  => (0 1 2 3 4 :a :b)
;;;; 


;;; リスト末尾に要素追加する為の構造体
;;; ※スロットの宣言は、(head (list nil)) (tail head)と書きたいが、
;;; 　initformで別スロットを参照できるのか不明なので、ラムダリストの&auxで指定
(defstruct (collector
            (:constructor make-collector (&aux (head (list nil)) (tail head)))
            )
  head
  tail
  )

;;; 構造体 collector に要素を追加（蓄積）
;;; @param e 追加する要素
;;; @param c 構造体 collector
;;; @return 引数e
(defun collector-push (e c)
  (let* ((new-node (list e))
         )
    (setf (cdr (collector-tail c)) new-node
          (collector-tail c) new-node
          ) ;setf
    e
    ) ;let*
  )

;;; 構造体 collector に、リストの要素を追加（蓄積）
;;; @param lis 追加する要素を含むリスト
;;; @param c 構造体 collector
;;; @return 引数lis
(defun collector-append (lis c)
  (when lis
    (let* ((lis-new-node (copy-list lis))
           )
      (setf (cdr (collector-tail c)) lis-new-node
            (collector-tail c) (last lis-new-node)
            ) ;setf
      lis
      ) ;let*
    ) ;when
  )

;;; 構造体 collector に蓄積した結果（リスト）を取得
;;; 蓄積は消去される
;;; @param c 構造体 collector
;;; @return 蓄積結果
(defun get-collector-list (c)
  (prog1
      (cdr (collector-head c))
    
    ;; 蓄積を消去
    (setf (cdr (collector-head c)) nil
          (collector-tail c) (collector-head c)
          ) ;setf
    ) ;prog1
  )

;;; 構造体 controller を作成し、フォーム終了時に蓄積結果（リスト）を返す
;;; （例）
;;; (with-collector (c)
;;;   (collector-push 'a c)
;;;   (collector-push 'b c)
;;;  )
;;; => (a b)
;;; ※フォーム内で蓄積結果を得たい場合は、get-collector-list を使えばよい
;;; @param var collectorをセットする変数（シンボル）
;;; @param &body body 実行フォーム
;;; @return controller に蓄積された要素（リスト）
(defmacro with-collector ((var) &body body)
  `(let* ((,var (make-collector))
          )
     ,@body
     (get-collector-list ,var)
     ) ;let*
  )


