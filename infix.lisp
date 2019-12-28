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


;;; p-list を a-list に変換
;;; @param plis p-list
;;; @return a-list
(defun p-to-alist (plis)
  (cond
   ((null plis) nil)
   (t (cons (cons (car plis) (cadr plis))
            (p-to-alist (cddr plis))
            ) ;cons
      )
   ) ;cond
  )

;;; 単項演算子
;;; 次のものは対象外：論理演算子
(defparameter *mono-ope*
  ;; 各リスト要素の優先順位より後をa-listにする
  ;; （getf は test関数を指定できないため、指定可能なassocを使用する為）
  (mapcar #'(lambda (e) (cons (car e) (p-to-alist (cdr e))))
          '((16 :~ lognot
                :+ +
                :- -)
            )
          ) ;mapcar
  )

;;; ２項演算子
;;; 次のものは対象外：比較演算子、論理演算子、変数値の更新（+=など）
(defparameter *bi-ope*
  ;; 各リスト要素の優先順位より後をa-listにする
  ;; （getf は test関数を指定できないため、指定可能なassocを使用する為）
  (mapcar #'(lambda (e) (cons (car e) (p-to-alist (cdr e))))
          '((15 :** expt)
            (14 :* *
                :/ /
                :% rem)
            (13 :+ +
                :- -)
            (12 :<< ash
                :>> rash) ; >>> は未対応
            (9 :& logand)
            (8 :^ logxor)
            (7 :\| logior)
            )
          ) ;mapcar
  )

;;; >> 用関数
(defun rash (integer count)
  (ash integer (- count))
  )

;;; 演算子を検索
;;; @param ope 検索する演算子
;;; @param tree 検索先
;;; @return 演算子が見つかった場合、リスト(<優先順位> <演算子> <Ｓ式での演算子>)
(defun find-operator (ope tree
                          &aux pair)
  (when (symbolp ope)
    (dolist (e tree)
      ;; 文字列の綴りで比較することで、演算子（シンボル）のパッケージの違いを無視
      (when (setf pair (assoc ope (cdr e)
                              :test #'(lambda (a b) (string= (string a) (string b)))
                              ) ;assoc
                  ) ;setf
        (return (list (car e) (car pair) (cdr pair)))
        ) ;when
      ) ;dolist
    ) ;when
  )

;;; 中置記法に単項演算子として記載された値から、単項演算子を取得
;;; @param infix-ope 中置記法に演算子として記載された値
;;; @return 単項演算子。該当する演算子がない場合nil
(defun mono-ope (infix-ope &aux ope)
  (when (setf ope (find-operator infix-ope *mono-ope*))
    (list* :ope1 ope)
    ) ;when
  )

;;; 中置記法に２項演算子として記載された値から、２項演算子を取得
;;; @param infix-ope 中置記法に演算子として記載された値
;;; @return ２項演算子。該当する演算子がない場合nil
(defun bi-ope (infix-ope &aux ope)
  (when (setf ope (find-operator infix-ope *bi-ope*))
    (list* :ope2 ope)
    ) ;when
  )

;;; 演算子の優先順位を比較
;;; 第２引数の演算子の方が優先順位が高ければ真
;;; @param ope1 演算子
;;; @param ope2 演算子
;;; @return 
(defun ope-priority< (ope1 ope2)
  (< (cadr ope1) (cadr ope2))
  )

;;; 演算子用スタック
;;; （関数 pop-priority-and-push からも使用する為にグローバル変数としている）
(defparameter *ope-stack* nil)

;;; 前置記法の式の出力先
;;; （関数 pop-priority-and-push からも使用する為にグローバル変数としている）
(defparameter *prefix* nil)


;;; 先頭が第２引数とeqであるリストか？
;;; @param lis リスト
;;; @param e
(defun head-eq (lis e)
  (and (listp lis) (eq (car lis) e))
  )


;;; 中置記法を前置記法に変換
;;; @param infix 中置記法の式。（例）(1 + 2)
;;; 	値の部分にLisp式を埋め込む場合、#'を用いる
;;; 	（例）(a * #'(expt 2 10))
;;; @return 前置記法の式
(defun infix-to-prefix (infix)
  (let* ((*ope-stack* nil)
         (*value-stack* nil)
         (*prefix* nil)
         ;; :value    = 値を読み取るフェーズ
         ;; :operator = ２項演算子を読み取るフェーズ
         (phase :value)
         ope
         )
    (dolist (e infix)
      (case phase
        ;; 値読取フェーズ
        (:value
         (cond
          ;; ---- functionで始まるリストの場合、そのまま評価するLisp式と見なす
          ((head-eq e 'function)
           (push (list :function (cadr e)) *prefix*)
           ;; ２項演算子読取フェーズへ
           (setf phase :operator)
           )
          
          ;; ---- リストの場合、副式とみなし再帰呼出。その結果を出力
          ((listp e)
           (setf *prefix* (append (infix-to-prefix e) *prefix*))
           ;; ２項演算子読取フェーズへ
           (setf phase :operator)
           )
          
          ;; ---- 単項演算子の場合
          ((setf ope (mono-ope e))
           (pop-priority-and-push ope)
           ;; フェーズは変更せず、再度値読取
           )
          
          ;; ---- その他の場合、値としてプッシュ
          (t
           (push e *prefix*)
           ;; ２項演算子読取フェーズへ
           (setf phase :operator)
           )
          ) ;cond
         ) ;:value
        
        ;; ２項演算子読取フェーズ
        (:operator
         (cond
          ;; ---- ２項演算子の場合
          ((setf ope (bi-ope e))
           (pop-priority-and-push ope)
           ;; 値読取フェーズへ
           (setf phase :value)
           )
          
          ;; ---- その他の場合、エラー
          (t
           (error "不適切な２項演算子です。")
           )
          ) ;cond
         ) ;:operator
        ) ;case
      ) ;dolist
    
    ;; 値読取フェーズ完了でなければエラー
    ;; （値読取が完了すれば、演算子読取フェーズになっている）
    (unless (eq phase :operator)
      (error "式が不完全です。")
      ) ;unless
    
    ;;
    (dolist (ope *ope-stack*)
      (push ope *prefix*)
      ) ;dolist
    
    *prefix*
    ) ;let*
  )

;;; 演算子スタック上部の、優先順位の高い演算子をポップして結果として出力し、
;;; その後引数の演算子を演算子スタックへプッシュ。
;;; @param ope 演算子スタックへプッシュする演算子
(defun pop-priority-and-push (ope)
  (do* ()
       ((or (null *ope-stack*)
            (ope-priority< (car *ope-stack*) ope)
            ) ;or
        )
    (push (pop *ope-stack*) *prefix*)
    ) ;do*
  
  (push ope *ope-stack*)
  )

;;; 中置記法を後置記法に変換
;;; @param infix 中置記法の式。（例）(1 + 2)
;;; 	値の部分にLisp式を埋め込む場合、#'を用いる
;;; 	（例）(a * #'(expt 2 10))
;;; @return 後置記法の式
(defun infix-to-postfix (infix)
  (reverse (infix-to-prefix infix))
  )

;;; 後置記法をＳ式に変換
;;; @param postfix 後置記法
;;; @return Ｓ式で示した数式
(defun postfix-to-sexp (postfix)
  (let* ((stack nil)
         arg1
         arg2
         )
    (dolist (e postfix)
      (cond
       ;; ---- 単項演算子
       ((head-eq e :ope1)
        (setf arg1 (pop stack))
        (push (list (cadddr e) arg1) stack)
        )
       
       ;; ---- ２項演算子
       ((head-eq e :ope2)
        (setf arg2 (pop stack)
              arg1 (pop stack)
              ) ;setf
        (push (list (cadddr e) arg1 arg2) stack)
        )
       
       ;; ---- Lisp式
       ((head-eq e :function)
        (push (cadr e) stack)
        )
       
       ;; ---- 値
       (t
        (push e stack)
        )
       ) ;cond
      ) ;dolist
    
    (unless (eql (length stack) 1)
      (error "スタックに複数の値が残っています。：~s" stack)
      ) ;unless
    
    (car stack)
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
(defun simplify-nest-operator-tree (ope lis)
;(format t "ope : ~s~%lis : ~s~%" ope lis)
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
    (cons (car lis) (mapcar #'(lambda (e) (simplify-nest-operator-tree ope e))
                            (pullup-nest-args ope (cdr lis))
                            ) ;mapcar
          ) ;cons
    )
   ;; ----
   (t
    (cons (car lis) (mapcar #'(lambda (e) (simplify-nest-operator-tree ope e))
                            (cdr lis)
                            ) ;mapcar
          ) ;cons
    )
   ) ;cond
  )

;;; 中置記法をＳ式にする。その際展開可能な入れ子を展開する
;;; @param infix 中置記法の式
;;; @return Ｓ式
(defun infix-to-sexp (infix)
  (let* ((postfix (infix-to-postfix infix))
         (sexp (postfix-to-sexp postfix))
         )
    ;; 展開可能な入れ子を展開
    ;; （例）(+ 1 (+ 2 3)) => (+ 1 2 3)
    (dolist (ope '(* + logand logxor logior))
      (setf sexp (simplify-nest-operator-tree ope sexp))
      ) ;dolist
    
    sexp
    ) ;let*
  )

;;; 中置記法の数式を評価
;;; @param infix 中置記法の式
(defmacro inn (&body infix)
  (infix-to-sexp infix)
  )
