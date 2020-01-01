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
(provide :infix)

;;;; (1.5. defpackage)
(defpackage :infix (:use :common-lisp))

;;;; 2. in-package
(in-package :infix)

;;;; 3. shadow

;;;; 4. export
(export '(inn
          )
        ) ;export

;;;; 5. require

;;;; 6. use-package

;;;; 7. import


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; 
;;;; ■inn マクロ
;;;; 
;;;; 中置記法（生活上一般に使用されいる数式の記法）の数式を記述可能にする。
;;;; 中置記法の中に Lisp 式を含めたい場合、関数クォート（#'）を使用する。
;;;;
;;;; ●使用できる単項演算子
;;;;    ~ （ビットごとの否定） + -
;;;;
;;;; ●使用できる二項演算子
;;;;    ** （べき乗） * / % （剰余） + -
;;;;    << （左シフト） >> （右シフト）
;;;;    & （ビットごとの論理積） \| （ビットごとの論理和）
;;;;    ^ （ビットごとの排他的論理和） 
;;;; 
;;;; ●例
;;;; (let* ((a 3) (b 10))
;;;;   (inn a + #'(expt 2 10) * b)  ;マクロinn の引数が中置記法の数式
;;;;   )
;;;; => 10243
;;;; 


;;; 構造体。
(defstruct pof
  priority       ;優先順位
  operator-type  ;演算子の種別（単項演算子／二項演算子）
  operator       ;演算子（中置記法に於ける）
  function       ;関数
  )

;;; 構造体のリストを作成
;;; （※グローバル変数への値設定用）
;;; @param ope-type 演算子の種別（単項演算子／二項演算子）
;;; @param pri-pairs car が<優先順位>、cdr が(<演算子> <関数>) のリスト
;;; @return 
(defun make-pof-list (ope-type pri-pairs)
  (let* ((pri (car pri-pairs))
         (lis-pair (cdr pri-pairs))
         (result nil)
         )
    (dolist (pair lis-pair (reverse result))
      (push (make-pof :priority pri
                      :operator-type ope-type
                      :operator (car pair)
                      :function (cadr pair)
                      ) ;make-pof
            result
            ) ;push
      )
    ) ;let*
  )

;;; 単項演算子
;;; （次のものは対象外：論理演算子）
(defparameter *unary-operator-table*
  (mapcan #'(lambda (l) (make-pof-list :ope1 l))
          '((16 (:~ lognot) (:+ +) (:- -)))
          ) ;mapcar
  )

;;; >> 用関数
(defun rash (integer count)
  (ash integer (- count))
  )

;;; ２項演算子
;;; （次のものは対象外：比較演算子、論理演算子、変数値の更新（+=など））
(defparameter *binary-operator-table*
  (mapcan #'(lambda (l) (make-pof-list :ope2 l))
          '((15 (:** expt))
            (14 (:* *) (:/ /) (:% rem))
            (13 (:+ +) (:- -))
            (12 (:<< ash) (:>> rash))
            ( 9 (:& logand))
            ( 8 (:^ logxor))
            ( 7 (:\| logior))
            )
          ) ;mapcar
  )

;;; 演算子を検索
;;; @param ope 中置記法に於ける演算子（シンボル）
;;; @param lis-pof 構造体 pof のリスト
;;; @return 
(defun find-operator (ope lis-pof)
  (flet (;; シンボルを、パッケージを無視して綴りで比較
         (symbol-name= (sym1 sym2) (string= (symbol-name sym1) (symbol-name sym2))
           ) ;symbol-name=
         )
    (when (symbolp ope)
      (find ope lis-pof :test #'(lambda (o pof) (symbol-name= o (pof-operator pof))))
      ) ;when
    ) ;flet
  )

;;; 演算子の優先順位を比較
;;; 第２引数の演算子の方が優先順位が高ければ真
;;; @param pof1 演算子（構造体）
;;; @param pof2 演算子（構造体）
;;; @return 
(defun priority< (pof1 pof2)
  (< (pof-priority pof1) (pof-priority pof2))
  )


;;; 先頭が第２引数とeqであるリストか？
;;; @param lis リスト
;;; @param e
(defun head-eq (lis e)
  (and (listp lis) (eq (car lis) e))
  )

;;; 指定数 pop した値を要素とするリスト。ただし要素は逆順。
;;; @param n 要素数
;;; @param place 汎変数
(defmacro pop-n (n place)
  (let ((var-result (gensym))
        )
    `(let ((,var-result nil)
           )
       (dotimes (,(gensym) ,n ,var-result)
         (push (pop ,place) ,var-result)
         ) ;dotimes
       )
    ) ;let
  )


;;; 中置記法を前置記法に変換
;;; @param infix 中置記法の式。（例）(1 + 2)
;;; 	値の部分にLisp式を埋め込む場合、#'を用いる
;;; 	（例）(a * #'(expt 2 10))
;;; @return 前置記法の式
(defun infix-to-sexp (infix)
  (let* ((ope-stack nil)    ;演算子スタック
         (prefix nil)       ;前置記法の式の出力先
         ;; フェーズ  :value    ： 値読取フェーズ
         ;;           :operator ： ２項演算子読取フェーズ
         (phase :value)
         pof
         )
    (labels (;; 単項演算子を検索・取得
             ;; @param ope 中置記法に於ける演算子（シンボル）
             ;; @return 単項演算子。該当する演算子がない場合nil
             (unary-operator (ope) (find-operator ope *unary-operator-table*)
               ) ;unary-operator
             ;; 二項演算子を検索・取得
             ;; @param ope 中置記法に於ける演算子（シンボル）
             ;; @return 二項演算子。該当する演算子がない場合nil
             (binary-operator (ope) (find-operator ope *binary-operator-table*)
               ) ;binary-operator
             
             ;; 演算子を出力。値スタックの値を一つあるいは二つ消費。
             ;; @param pof 演算子（構造体）
             ;; @return 
             (output-operator (pof) (case (pof-operator-type pof)
                                      ;; 単項演算子
                                      (:ope1 (push (list* (pof-function pof) (pop-n 1 prefix)) prefix)
                                       )
                                      ;; 二項演算子
                                      (otherwise (push (list* (pof-function pof) (pop-n 2 prefix)) prefix)
                                                 )
                                      ) ;case
               ) ;output-operator
             ;; 演算子スタック上部の、優先順位の高い演算子をポップして出力し、
             ;; その後に引数の演算子を演算子スタックへプッシュ。
             ;; @param ope-new 演算子スタックへプッシュする演算子
             ;; @return
             (pop-priority-and-push (ope-new)
               (do* ()
                    ((or (null ope-stack)
                         (priority< (car ope-stack) ope-new)
                         ) ;or
                     )
                 (output-operator (pop ope-stack))  ;出力
                 ) ;do*
               
               (push ope-new ope-stack)
               )
             )
      (dolist (e infix)
        (case phase
          ;; 値読取フェーズ
          (:value (cond
                   ;; ---- function で始まるリストの場合、そのまま評価する Lisp 式と見なす
                   ((head-eq e 'function)
                    (push (cadr e) prefix)  ;出力
                    ;; 二項演算子読取フェーズへ
                    (setf phase :operator)
                    )
                   
                   ;; ---- リストの場合、副式とみなし再帰呼出。その結果を出力
                   ((listp e)
                    (push (infix-to-sexp e) prefix)
                    ;; 二項演算子読取フェーズへ
                    (setf phase :operator)
                    )
                   
                   ;; ---- 単項演算子の場合
                   ((setf pof (unary-operator e))
                    (pop-priority-and-push pof)
                    ;; フェーズは変更せず、再度値読取
                    )
                   
                   ;; ---- その他の場合、値として出力
                   (t
                    (push e prefix)
                    ;; 二項演算子読取フェーズへ
                    (setf phase :operator)
                    )
                   ) ;cond
           ) ;:value
          
          ;; 二項演算子読取フェーズ
          (:operator (cond
                      ;; ---- 二項演算子の場合
                      ((setf pof (binary-operator e))
                       (pop-priority-and-push pof)
                       ;; 値読取フェーズへ
                       (setf phase :value)
                       )
                      
                      ;; ---- その他の場合、エラー
                      (t
                       (error "不適切な二項演算子です。")
                       )
                      ) ;cond
           ) ;:operator
          ) ;case
        ) ;dolist
      
      ;; 値読取フェーズ完了でなければエラー
      ;; （値読取が完了すれば、演算子読取フェーズになっている）
      (when (eq phase :value)
        (error "式が不完全です。")
        ) ;when
      
      ;; 演算子スタックの残りを全て出力
      (do ()
          ((null ope-stack) prefix)
        (output-operator (pop ope-stack))
        ) ;do
      
      ;; 要素数は一つだけのはず
      (car prefix)
      ) ;labels
    ) ;let*
  )


;;; リストの要素に、指定演算子を先頭に持つリストがある場合、
;;; その入れ子のリストの引数を自リストの引数とする
;;; （例）(pullup-args '+ '(+ 1 (+ 2 3)))
;;;       => (+ 1 2 3)
;;; @param ope 演算子
;;; @param lis 先頭がopeであるリストであること
(defun pullup-nest-args (ope args)
  (mapcan #'(lambda (e) (if (head-eq e ope)
                            (pullup-nest-args ope (cdr e))
                          (list e)
                          ) ;if
              ) ;lambda
          args
          ) ;mapcan
  )

;;; リストの中から、先頭が指定の演算子であるリストを探し出し、
;;; 見つかったリストを、関数 pullup-nest-args 呼出結果に置き換える
;;; @param ope 演算子
;;; @param lis
(defun simplify-nest-operator (ope lis)
  (cond
   ;; ----
   ((null lis)
    nil
    )
   ;; ----
   ((not (listp lis))
    lis
    )
   ;; ----
   ((head-eq lis ope)
    ;; 関数 pullup-nest-args を適用した後のリストをさらに探索
    (cons (car lis) (mapcar #'(lambda (e) (simplify-nest-operator ope e))
                            (pullup-nest-args ope (cdr lis))
                            ) ;mapcar
          ) ;cons
    )
   ;; ----
   (t
    (cons (car lis) (mapcar #'(lambda (e) (simplify-nest-operator ope e))
                            (cdr lis)
                            ) ;mapcar
          ) ;cons
    )
   ) ;cond
  )

;;; 中置記法をＳ式にする。その際展開可能な入れ子を展開する
;;; @param infix 中置記法の式
;;; @return Ｓ式
(defun infix-to-sexp-simplify (infix)
  (let* ((sexp (infix-to-sexp infix))
         )
    ;; 展開可能な入れ子を展開
    ;; （例）(+ 1 (+ 2 3)) => (+ 1 2 3)
    (dolist (ope '(* + logand logxor logior))
      (setf sexp (simplify-nest-operator ope sexp))
      ) ;dolist
    
    sexp
    ) ;let*
  )

;;; 中置記法の数式を評価
;;; @param infix 中置記法の式
(defmacro inn (&body infix)
  (infix-to-sexp-simplify infix)
  )
