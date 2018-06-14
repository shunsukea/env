;;; -*- Mode: Lisp; Package: OUTLINE-TREE2 -*-
;;; toolbar.l --- outline-tree

;; Copyright (C) 2001-2007 OHKUBO Hiroshi <ohkubo@s53.xrea.com>

;; Author: OHKUBO Hiroshi <ohkubo@s53.xrea.com>
;; Time-stamp: <2007/07/04 17:14:53 +0900>

;;; Code:

(provide "outline-tree/toolbar")

(require "outline-tree/defs")
(require "treeview/virtual-keys")

(in-package "outline-tree2")

(defun outline-tree-tool-bar-toggle-open-close ()
  (interactive)
  "�A�E�g���C���c���[���J����B
���N����  �F�N�����A�I�[�v��
�N���[�Y���F�I�[�v��
�I�[�v�����F�N���[�Y�BCtrl �������Ă���ꍇ�I��"
  (let ((ctrl (or (minusp (winapi:GetKeyState winapi:VK_CONTROL))
                  (minusp (winapi:GetKeyState winapi:VK_LCONTROL))
                  (minusp (winapi:GetKeyState winapi:VK_RCONTROL)))))
    (cond
     ((not (outline-tree-exist-p))
      (outline-tree-create)
      (outline-tree-open))
     ((outline-tree-open-p)
      (if ctrl
          (outline-tree-delete)
        (outline-tree-close)))
     (t
      (outline-tree-open)))))

(defun outline-tree-tool-bar-toggle-exist ()
  (interactive)
  "�A�E�g���C���c���[���N��/�I������B
�N����    �F�I��
���N����  �F�N��"
  (if (outline-tree-exist-p)
      (outline-tree-delete)
    (progn
      (outline-tree-create)
      (outline-tree-open))))

(defun outline-tree-tool-bar ()
  (create-tool-bar
   'outline-tree-tool-bar
   (merge-pathnames "etc/toolbar-outline-tree.bmp" (si:system-root))
   `(
;     ("�A�E�g���C���c���[" 0 outline-tree-tool-bar-toggle-open-close
;      ,#'(lambda () (and (outline-tree-open-p) :check)))
     ("�A�E�g���C���c���[" 0 outline-tree-tool-bar-toggle-exist
      ,#'(lambda () (and (outline-tree-exist-p) :check)))
     )))

(define-command-bar 'outline-tree-tool-bar "�A�E�g���C���c���[(&O)")

(add-hook '*outline-tree-uninstall-hook*
          #'(lambda () (delete-command-bar 'outline-tree-tool-bar)))

;;; toolbar.l ends here