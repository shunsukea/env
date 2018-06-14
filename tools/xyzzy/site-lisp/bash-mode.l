;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

;;; Author:

;;; ・Cスタイルのコメントは無視することにした。
;;; ・正規表現は一切認識しない。正規表現で "'# 等を使用する場合は
;;;   エスケープするなりして逃げてちょーだい。

(provide "bash")

(in-package "editor")

(export '(bash-mode bash-indent-line *bash-keyword-file* *bash-mode-hook*
	  *bash-mode-syntax-table* *bash-mode-map*
	  *bash-mode-abbrev-table* *bash-comment-column*
          c-indent-level
          c-continued-statement-offset
          ))

(defvar *bash-mode-hook* nil)
(defvar *bash-comment-column* nil)

(defvar *bash-keyword-hash-table* nil)
(defvar *bash-keyword-file* "BASH")

(defvar *bash-mode-syntax-table* nil)
(unless *bash-mode-syntax-table*
  (setq *bash-mode-syntax-table* (make-syntax-table))
  (do ((x #x21 (1+ x)))
      ((>= x #x7f))
    (let ((c (code-char x)))
      (unless (alphanumericp c)
	(set-syntax-punctuation *bash-mode-syntax-table* c))))
  (set-syntax-string *bash-mode-syntax-table* #\")
  (set-syntax-string *bash-mode-syntax-table* #\')
  ;(set-syntax-string *bash-mode-syntax-table* #\`)
  (set-syntax-escape *bash-mode-syntax-table* #\\)
  (set-syntax-symbol *bash-mode-syntax-table* #\_)
  (set-syntax-symbol-prefix *bash-mode-syntax-table* #\$)
  (set-syntax-symbol-prefix *bash-mode-syntax-table* #\@)
  (set-syntax-symbol-prefix *bash-mode-syntax-table* #\%)
  (set-syntax-match *bash-mode-syntax-table* #\( #\))
  (set-syntax-match *bash-mode-syntax-table* #\{ #\})
  (set-syntax-match *bash-mode-syntax-table* #\[ #\])
  ;(set-syntax-start-multi-comment *bash-mode-syntax-table* "/*")
  ;(set-syntax-end-multi-comment *bash-mode-syntax-table* "*/")
  (set-syntax-start-comment *bash-mode-syntax-table* #\#)
  (set-syntax-end-comment *bash-mode-syntax-table* #\LFD nil t))

(defvar *bash-mode-map* nil)
(unless *bash-mode-map*
  (setq *bash-mode-map* (make-sparse-keymap))
  (define-key *bash-mode-map* #\{ 'c-electric-insert)
  (define-key *bash-mode-map* #\: 'c-electric-insert)
  (define-key *bash-mode-map* #\} 'c-electric-close)
  (define-key *bash-mode-map* #\C-h 'backward-delete-char-untabify-or-selection)
  (define-key *bash-mode-map* #\TAB 'bash-indent-line)
  (define-key *bash-mode-map* #\C-M-q 'indent-sexp)
  (define-key *bash-mode-map* #\RET 'c-newline-and-indent))

(setq c-continued-statement-offset 4)

(defvar *bash-mode-abbrev-table* nil)
(unless *bash-mode-abbrev-table*
  (define-abbrev-table '*bash-mode-abbrev-table*))

(defun bash-comment-start-skip ()
  (let ((eol (save-excursion (goto-eol) (point))))
    (while (scan-buffer "\\(^\\|[ \t]\\)\\(#+\\)[ \t]*"
;    (while (scan-buffer "\\(^\\|[ \t]\\);?\\(#+\\)[ \t]*"
			:regexp t :limit eol :tail t)
      (unless (eq (parse-point-syntax (match-end 2)) :string)
	(goto-char (match-beginning 2))
	(return-from bash-comment-start-skip (match-end 0)))))
  (goto-eol)
  nil)

(defun bash-comment-indent ()
  (generic-comment-indent "#" #'calc-c-indent))

(defun tags-find-bash-point (class name functionp point)
  (goto-char (- point *jump-tag-limit*))
  (and (scan-buffer (concat "^[ \t]*\\(function\\)?[ \t]+"
			    (regexp-quote name)
			    "\\($\\|[ \t\r\n\f#]\\)")
		    :regexp t :limit (+ point *jump-tag-limit*))
       (point)))

(defun bash-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq mode-name "bash")
  (setq buffer-mode 'bash-mode)
  (use-syntax-table *bash-mode-syntax-table*)
  (use-keymap *bash-mode-map*)
  (make-local-variable 'mode-specific-indent-command)
  (setq mode-specific-indent-command 'c-indent-line)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start "^$\\|\f")
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'indent-tabs-mode)
  (setq indent-tabs-mode *c-indent-tabs-mode*)

  (make-local-variable 'c-continued-statement-offset)
  (setq c-continued-statement-offset 0)
  (make-local-variable 'c-indent-level)
  (setq c-indent-level 4)

  (make-local-variable 'tags-find-target)
  (setq tags-find-target #'tags-find-target-simple)
  (make-local-variable 'tags-find-point)
  (setq tags-find-point #'tags-find-bash-point)
  (and *bash-keyword-file*
       (null *bash-keyword-hash-table*)
       (setq *bash-keyword-hash-table*
	     (load-keyword-file *bash-keyword-file*)))
  (when *bash-keyword-hash-table*
    (make-local-variable 'keyword-hash-table)
    (setq keyword-hash-table *bash-keyword-hash-table*))
  (setq *local-abbrev-table* *bash-mode-abbrev-table*)
  (setq comment-start "#")
  (setq comment-end "")
  (setq comment-start-skip-function 'bash-comment-start-skip)
  (setq comment-indent-function 'bash-comment-indent)
  (when *bash-comment-column*
    (setq comment-column *bash-comment-column*))
  (run-hooks '*bash-mode-hook*))

;まだちっとよくわかってない
(pushnew '(bash-maketags "*.sh") *maketags-list* :key #'car)


;まだちっとよくわかってない
(defvar c-indent-level 4)
(defvar c-continued-statement-offset 0)


(defun bash-maketags (file count)
  (goto-char (point-min))
  (while (scan-buffer "^[ \t]*\\(function\\)[ \t]+" :regexp t :tail t)
    (do-events)
    (let ((opoint (point)))
      (skip-chars-forward "A-Za-z0-9_")
      (unless (= opoint (point))
	(format t "~A~A~D" (buffer-substring opoint (point)) file opoint)
	(incf count))))
  count)





(defun calc-bash-indent ()
  ;4
  (calc-bash-indent2)
 )
(defun calc-bash-indent2 ()

 (let ((np)(upper)(upline)(downline)(templine)(current-line-endp)(ret)(cl))
   (setq np (point)
         upper (point-min)
         upline -1
         downline -1
         templine -1
         current-comment-line -1
         cl (current-line-number)
         )

  (goto-eol)
  (setq current-line-endp (point))

   (goto-bol)

#|   ;コメント
   (when  (scan-buffer "^[ \t]*#" :regexp t :reverse nil :right-bound current-line-endp)
     (setq current-comment-line (current-line-number))
    )

  (goto-char np)
  (goto-bol)
|#
   ;indentする行
   (when  (scan-buffer "^[ \t]*\\(if\\|{\\|do\\|else\\|elif\\|esac\\\)[ \t\n]" :regexp t :reverse t)
     (setq upline (current-line-number)
           upper (point))
    (if (= upline cl)(setq tl (1- cl) upline -1))
   )

  (goto-char np)
  (goto-bol)

   ;indent戻す行
   (when  (scan-buffer "^[ \t]*\\(done\\|}\\|esac\\|fi\\)" :regexp t :reverse t :left-bound upper)
     (setq downline (current-line-number))
     (if (>= downline cl)(setq downline -1))
    )

  (goto-char np)
  (goto-bol)


   ;ヒットした行が現在行なら 一時的にインデントが無くなる行
  (when  (scan-buffer "^[ \t]*\\(else\\|elif\\|esac\\|done\\|}\\|esac\\|fi\\)[ \t\n]" :regexp t :reverse nil :right-bound current-line-endp)
     (setq templine (current-line-number))
    (if (not (= templine cl)) (setq templine -1))
;    (msgbox "templine ~A" templine)
    )

;   (msgbox "bash indent:(cl:~A ~A) up~A down~A temp~A / + ~A (temp+~A)"
;           cl tl upline downline templine ret adp)
;  (msgbox "bash indent a")

   ;カーソルに近いポイントはどれか
  (if (> upline downline)
      (progn
        (setq downline nil)
        (if (>= templine upline)
            (setq upline nil)
          (setq templine nil)
          )
        )
    (progn
      (setq upline nil)
      (if (>= templine downline)
          (setq downline nil)
        (setq templine nil)
        )
      )
    )

  (setq downline nil)


;   (msgbox "bash indent:(cl:~A ~A) up~A down~A temp~A / + ~A (temp+~A)"
;           cl tl upline downline templine ret adp)

   ;  (msgbox "bash indent c")
   ;カーソルに近いポイントに合わせた操作
   (cond ((and  upline(not (= -1 upline)))
          (setq tl upline adp c-indent-level)
          (if (= upline cl)(setq tl upline))
          )
         ;((and downline(not (= -1 downline))(not downline)) (setq tl downline adp (- 0 c-indent-level)))
         ((and templine (not (= -1 templine)))
          (setq tl templine adp  (- 0 c-indent-level))
          (if (> templine cl)
              (setq tl nil)
            (setq tl (- cl 1))
;            (if (not (= templine cl))(setq tl (- cl 1) templine -1))
            )
          )
         (t (setq tl nil))
         )

;   (msgbox "bash indent:(cl:~A ~A) up~A down~A temp~A / + ~A (temp+~A)"
;           cl tl upline downline templine ret adp)
  (if (not tl) (setq tl (1- cl) adp 0))

   ;一つ前の行を参考にインデント
  (if (and (not upline)(= tl (1- cl)))
   (progn
     (goto-line tl)
     (goto-eol)
     (when (< (current-column) 2)
       (skip-chars-backward "\n")
       ;(decf tl)
       (setq tl (current-line-number))
       (goto-line tl)
       (goto-eol)
       )
    (setq tl (current-line-number))
    )
    )

;  (msgbox "bash indent d")
   ;対象の行(=tl)のインデントとその差分(=adp)から 現在のインデント量(=ret)を計算
   (setq ret
         (if tl
          (progn
            (goto-line tl)
            (setq temp 0)
            (goto-bol)
             (skip-syntax-spec-forward " ")
             (setq temp
                   (if (= tl (current-line-number))(current-column) 0))
             (backward-line)
            (+ adp temp)
            );progn
           0
           );if
         );setq

;   (msgbox "bash indent:(cl:~A ~A) up~A down~A temp~A / + ~A (temp+~A)"
;           cl tl upline downline templine ret adp)
   (goto-char np)
   ret
  )
  )

(defun bash-indent-line ()
  (interactive "*")
  (if (or (not (interactive-p))
	  *c-tab-always-indent*
	  (save-excursion
	    (skip-chars-backward " \t")
	    (bolp)))
      (case (save-excursion
	      (goto-bol)
	      (parse-point-syntax))
	(:string)
	(:comment
	 (let ((column (calc-c-comment-indent)))
	   (when (integerp column)
	     (smart-indentation column))))
	(t
	 (let ((column (calc-bash-indent)))
	   (when (integerp column)
	     (smart-indentation column)))))
    (insert "\t"))
  t)

;(msgbox "did read bash-mode")

