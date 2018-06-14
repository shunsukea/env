;;; -*- Mode: Lisp; Package: OUTLINE-TREE2 -*-
;;; defs.l --- outline-tree

;; Copyright (C) 2001-2012 OHKUBO Hiroshi <ohkubo@s53.xrea.com>

;; Author: OHKUBO Hiroshi <ohkubo@s53.xrea.com>
;; Time-stamp: <2012/04/03 00:31:00 +0900>

;;; Code:

(provide "outline-tree/defs")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "outline-tree2")
    (defpackage "outline-tree2"
      (:use "lisp" "editor"))))

(in-package "outline-tree2")

;;----------------------------------------------------------------------------
;; ��{
;;----------------------------------------------------------------------------
(defconstant *outline-tree-version* "2012.04.03")
(defvar *outline-tree-debug* nil)

;;----------------------------------------------------------------------------
;; �p�X
;;----------------------------------------------------------------------------
;; config �t�@�C�� / autoload �f�B���N�g��
(defvar *outline-tree-base-directory* "~/.outline-tree")
(defvar *outline-tree-config-file*
  (merge-pathnames "config.l" *outline-tree-base-directory*))
(defvar *outline-tree-autoload-directory*
  (merge-pathnames "autoload" *outline-tree-base-directory*))

;;----------------------------------------------------------------------------
;; HOOK
;;----------------------------------------------------------------------------
;; TREEVIEW �쐬/�폜/�A���C���X�g�[��
(defvar *outline-tree-create-hook* nil
  "outline-tree �J�n���ɕ]������B
outline-tree-create �̍Ō�� run-hooks �ŏ����B")
(defvar *outline-tree-delete-hook* nil
  "outline-tree �I�����ɕ]������B
outline-tree-delete �̍Ō�� run-hooks �ŏ����B")

(defvar *outline-tree-startup-hook* nil
  "outline-tree �����N�����ɕ]������B
outline-tree-startup �̍Ō�� run-hooks �ŏ����B")
(defvar *outline-tree-uninstall-hook* nil
  "outline-tree �A���C���X�g�[�����ɕ]������B
outline-tree-uninstall �̃q�X�g���ϐ��폜�O�� run-hooks �ŏ����B")

;; �o�b�t�@�m�[�h�̍폜
(defvar *outline-tree-delete-buffer-node-hook* nil
  "buffer-node �폜���ɕ]������B
outline-tree-delete-buffer-node-by-buffer �̍Ō�� buffer �������Ƃ���
run-hook-with-args �ŏ����B")

;; �A�E�g���C���̍쐬
(defvar *outline-tree-create-outline-hook* nil
  "�o�b�t�@�̃A�E�g���C���쐬���ɕ]������B
outline-tree-create-outline-by-buffer �̍Ō�� run-hooks �ŏ����B
outline-tree-create-outline-by-buffer �֐��ɂ̓o�b�t�@�̃��X�g���n�����Ƃ�
�\�����A�o�b�t�@�̃��X�g��n�����ꍇ���A�o�b�t�@���ł͂Ȃ���x�̂ݕ]����
���B")

;;----------------------------------------------------------------------------
;; 
;;----------------------------------------------------------------------------
;; �w�b�_�m�[�h��
(defvar *outline-tree-header-node-name* "<HEADER>"
  "�w�b�_�m�[�h���B
outline-tree-create-outline-regexp-internal,
outline-tree-create-outline-topic-internal �֐��ɂĕ]�������B")

;; �J�e�S���m�[�h��
(defvar *outline-tree-visible-buffer-category-node-name* "=== �\���o�b�t�@ ==="
  "�\���o�b�t�@�p�J�e�S���m�[�h��")
(defvar *outline-tree-invisible-buffer-category-node-name* "=== ��\���o�b�t�@ ==="
  "��\���o�b�t�@�p�J�e�S���m�[�h��")

;; �T�u�J�e�S�������o
(defvar *outline-tree-buffer-category-regexp-list*
  '(("^ *\\(\\*Minibuf\\)"      . " Minibuf")
    ("^ *\\(\\*howm\\)"         . " howm")
    ("^ *\\(\\*Help\\*\\)"      . " Help")
    ("^ *\\(\\*NetInstaller\\)" . " NetInstaller")
    ("^ *\\(\\*RSS\\)"          . " RSS")
    "^ *\\*?\\(.+?\\):"
    ("^\\*untitled\\*"          . " *untitled*"))
  "�o�b�t�@������J�e�S���������肷��ۂɗp����B
outline-tree-detect-category-name �֐��Ŏg�p����B")

;; �m�[�h�Ή��e�L�X�g�n�C���C�g
(defvar *outline-tree-text-highlight-attribute*
  '(:background 3 :prefix #\T :extend t)
  "�m�[�h�Ή��e�L�X�g���n�C���C�g����Ƃ��̕\�������B
outline-tree-set-text-highlight �ŏ��������B")

;;
(defvar *outline-tree-delete-category-buffers-except-regexp-list*
  '("^\\*scratch\\*$")
  "�u�J�e�S�������v�ۂɑΏۊO����o�b�t�@���̐��K�\�������X�g�Ɏw�肷��B
outline-tree-delete-category-buffers �֐��Ŏg�p����B")

;;----------------------------------------------------------------------------
;; app-menu
;;----------------------------------------------------------------------------
(defvar *outline-tree-app-menu* nil
  "outline-tree ���j���[")
(defvar *outline-tree-app-menu-position* 'ed::help
  "outline-tree ���j���[�ʒu")
(define-history-variable *outline-tree-app-menu-name* "&Outline-Tree"
  "outline-tree ���j���[������")
(define-history-variable *outline-tree-set-app-menu* t
  "outline-tree ���j���[�N����Ԃ��L�^���邽�߂̃t���O")

;;; defs.l ends here