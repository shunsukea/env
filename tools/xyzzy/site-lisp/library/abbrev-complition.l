;; -*- Mode : lisp -*-

;; abbrev complition
;;
;; TKI
;; http://www2.ocn.ne.jp/~cheerful/script/
;; ---------- [ Settings ] ----------
#|
refer lisp/abbrev.l

�⊮�@�\���g�����܂��B

���@�\���e
abbrev �ÓI��^��W�J �ɂ�������o�^����� �v���o���̂���ς�
�ׂ����X�y���~�X�ł��A�ł��܂���B

�Ȃ̂ł���� ���� abbrev table �� �ǂ�ŕ⊮���邽�߂̂��̂ł��B

���@�\�t���[
�ÓI��嗪�̕⊮
��  ��(�⊮�Ώۂ���)�⊮��ɂ���ɐÓI���W�J�⊮
��(���Ȃ��A�⊮NG�̂΂���) 
          �ilisp) lisp��⊮  lisp-complete-symbol
           html �����⊮  
                ���⊮
           �����C���N�������g
           ���t�}��
       �� ���I�⊮

;(global-set-key '(#\C-x #\") 'completion-abbreve-table)
;(global-set-key '(#\C-x #\') 'completion-abbreve-table)
(set-extended-key-translate-table exkey-C-space #\F14)
(global-set-key #\F14 'completion-abbreve-table)

|#

;; ---------- [ Settings ] ----------

;KEY

(unless (fboundp 'expand-abbrev)
  (load-library "abbrev")
  )

(provide "abbrev")
(in-package "editor")

(export '(*do-expand-after-complition* *do-expand-all-in-abbrev-complition*
		  completion-abbreve-table))

;after complition,  do expand?
;�⊮�̌� �W�J���邩�H
(defvar *do-expand-after-complition* t)

;search abbrev table type
;abbrev �⊮��table �̎��
(defvar *do-expand-all-in-abbrev-complition*
  (if *only-global-abbrevs* :global :local))

;example...
; :all  :local :global-only :local-only
;(setq *do-expand-all-in-abbrev-complition* :all)
;(setq *do-expand-all-in-abbrev-complition* :local) ;local & global
;(setq *do-expand-all-in-abbrev-complition* :local-only) ;local only
;(setq *do-expand-all-in-abbrev-complition* :global) ; global only
;(setq *do-expand-all-in-abbrev-complition* ed::*java-mode-abbrev-table*);java only


(setq *abbrev-complition-mode-list*
 (list (cons "Lisp"  'lisp-complete-symbol)
       (cons "Lisp Interaction"  'lisp-complete-symbol)
       (cons "HTML+"  'html+-do-completion)))

;; ---------- [ Settings ] ----------

(defun completion-abbreve-table ()
  (interactive)
  (let ((fp)(nc (point)) (kl nil) (tables))

	;init
    (save-excursion
      (backward-char)
      (skip-syntax-spec-backward "w_")
      (setq fp (point))
      (goto-char nc)
      )
 
    ;check & set table
    (setq tables (get-abbrev-complition-table))
	;gather word (thought-less-ly)
    (setq kl (get-abbreviate-list-from-hash tables))

    ;abbrev-flow
    (let ((res-val)(res-lis)(res-prefix))
      (multiple-value-setq (res-val res-lis res-prefix) (do-completion fp nc :list kl))
      (cond ((or (equal res-val :no-completions) 
                 (equal res-val :no-match)
                 )
             ;no hit
             (do-left-complitions))

             ;cancel
            ((not res-val)
             nil)

            (t ; furthermore expand abbrev word by 'expand-abbrev?
             ;(equal res-val :solo-match)
             ;(equal res-val :not-unique)
             (do-abbrev-re-complition tables))))))

;sub
;abbrev function
(defun do-abbrev-re-complition (tables)
  ;*do-expand-after-complition*
  (let ((bt *local-abbrev-table*))
    (dolist (abbrev-table tables)
      (if (symbolp abbrev-table) 
          (setq abbrev-table (symbol-value abbrev-table)))

      (when (hash-table-p abbrev-table)
        (setq *local-abbrev-table* abbrev-table)
        (if (equal t (expand-abbrev)) (return))))

    (setq *local-abbrev-table* bt)))

(defun do-left-complitions()
 (let ((func)(ret))
  (setq func (assoc mode-name *abbrev-complition-mode-list* :test 'equal))
   (if func (setq ret (funcall (cdr func))))
   (if (not ret)(dabbrev-expand))))

;
;sub function

(defun get-abbrev-complition-table (&optional type)
  (unless type (setq type *do-expand-all-in-abbrev-complition*))
  (cond ((equal :all type) (list ed::*abbrev-table-name-list*))
        ((equal :local-only type) (list *local-abbrev-table*))
        ((equal :global  type) (list *global-abbrev-table*))
        ((equal :local  type) (list *global-abbrev-table* *local-abbrev-table*))
        (t (list *do-expand-all-in-abbrev-complition*))
        ))

(defun get-abbreviate-list-from-hash (hashtable)
  (let ((i)(key)(val)(kl))
    (dolist (abbrev-table hashtable)
      (if (symbolp abbrev-table) (setq abbrev-table (symbol-value abbrev-table)))
	  (setq i 0)
	  (when (hash-table-p abbrev-table)
		(handler-case
			(loop
			  (multiple-value-setq (i key val) (si:*enum-hash-table abbrev-table i))
			  (unless i (return))
			  (push (format nil "~A" key) kl)
			  )
		  (type-error (c) (progn(message "error ~A" abbrev-table) (sit-for .3)))
		  )))
   kl))
