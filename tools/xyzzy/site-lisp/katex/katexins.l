;;; -*- Lisp; syntax: Common-Lisp -*-
;;; installation of KaTeX functions
;;; katexins.l, v 1.67.1.0
;;; fixed by Maeda Manabu
;;; Last fixed  "Mon Aug 18 2003 20:48:24 +0900"
;;;
;;; �� ���̃t�@�C���̎g����
;;;
;;;     �܂Ƃ߂ăo�C�g�R���p�C�����邽�߂̊֐�
;;;        katex-byte-compile             �R���p�C������
;;;        katex-byte-compile-with-clean  .cl �������Ă���R���p�C��
;;;
;;;     �Ԓ����Ŏg�p���Ă���q�X�g���ϐ��������ŕۑ��ł���悤�ɂ���B
;;;        katex-register-history
;;;
;;;     0. ����
;;; 	  �W�J�����t�@�C���� site-lisp/katex/ �ȉ��ɒu�����Ƃ��܂��B
;;;       �قȂ�ꍇ�́A*makefile-path* ��K�X�ύX���Ă��������B
;;;
;;;     1. �o�C�g�R���p�C������B
;;;
;;; 	  1.1. M-x load-library[Enter]
;;; 	  1.2. Load library: katex/katexins[Enter]
;;;
;;; 	  1.3. .lc �t�@�C�����Ȃ��ꍇ
;;;             M-x elisp::katex-byte-compile[Enter]
;;;
;;; 	  1.4. �o�[�W�����A�b�v����ꍇ
;;;		M-x elisp::katex-byte-compile-with-clean[Enter]
;;;
;;;     2. xyzzy���I���B
;;;
;;;     3. xyzzy���ċN���B
;;;

(provide "katexins")

(in-package "elisp")
(require "emacs")
(load-library "makefile")

(setq *makefile-path* (merge-pathnames "site-lisp/katex" (si:system-root)))

(setq *makefile-file-list* '("elisp.l"
			     "emacs.l"
			     "comment.l"
			     "katexlib.l"
			     "katex.l"
			     "katexadd.l"
			     "katexbar.l"
			     "katexenv.l"
			     "katexgen.l"
			     "katexhie.l"
			     "katexhlp.l"
			     "katexmen.l"
			     "katexmth.l"
			     "katexprc.l"
			     "katexsec.l"))

(defun katex-byte-compile ()
  (interactive)
  (makefile))

(defun katex-byte-compile-with-clean ()
  (interactive)
  (makefile-with-clean))
