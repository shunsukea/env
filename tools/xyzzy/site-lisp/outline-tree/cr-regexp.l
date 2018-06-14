;;; -*- Mode: Lisp; Package: OUTLINE-TREE2 -*-
;;; cr-regexp.l --- outline-tree

;; Copyright (C) 2001-2009 OHKUBO Hiroshi <ohkubo@s53.xrea.com>

;; Author: OHKUBO Hiroshi <ohkubo@s53.xrea.com>
;; Time-stamp: <2009/04/03 14:24:48 +0900>

;;; Code:

(provide "outline-tree/cr-regexp")

(require "outline-tree/outline-tree")
(require "outline-tree/cr-regexp-internal")
(require "outline-tree/utils")

(in-package "outline-tree2")

(defvar-local outline-tree-create-outline-regexp-layer-regexp-list nil)
(defvar-local outline-tree-create-outline-regexp-layer-regexp-list-case-fold nil)
(defvar-local outline-tree-create-outline-regexp-layer-regexp-list-order nil)
(let ((variables
       '(outline-tree-create-outline-regexp-layer-regexp-list
         outline-tree-create-outline-regexp-layer-regexp-list-case-fold
         outline-tree-create-outline-regexp-layer-regexp-list-order)))
  ;; �q�X�g���ϐ��Ƃ��Đݒ�
  (register-history-variable variables)
  ;; �o�b�t�@���[�J���L��/�����̓���
  (push variables *outline-tree-sync-local-variables-list*))

;; ���K�\���A�E�g���C��
(defun outline-tree-create-outline-regexp (root-node)
  (outline-tree-create-outline-regexp-internal
   :root-node root-node
   :layer-regexp-list-order outline-tree-create-outline-regexp-layer-regexp-list-order
   :layer-regexp-list outline-tree-create-outline-regexp-layer-regexp-list
   :layer-regexp-list-case-fold outline-tree-create-outline-regexp-layer-regexp-list-case-fold
   :sub-type :heading-node
   :line-oriented t
   :title-converter nil))
(outline-tree-add-create-outline-function
 'outline-tree-create-outline-regexp "**���K�\�� (�_�C�A���O�ݒ�L)" '(:type-general :line-oriented :sequential)
 "���K�\���ŊK�w����͂��A���o�����A�E�g���C���\�����܂��B

[�A�E�g���C���c���[�ݒ�]-[���K�\��] �ŉ�͕��@�̃J�X�^�}�C�Y���\�ł��B

[�A�E�g���C���쐬�֐��̐���]-[�u���K�\���v�ݒ肩��] ���j���[����A
���݂̐ݒ�����ɁA���[�U��`�A�E�g���C���쐬�֐��𐶐��ł��܂��B")

;; property-sheet
(outline-tree-add-option-prop-page
 'outline-tree-create-outline-regexp
 (let (dialog-base-side dialog-base-height dialog-side dialog-height dialog-size controls)
   ;; ����/���[�J��
   (setq dialog-base-side 4)
   (setq dialog-base-height 4)
   (setq dialog-side dialog-base-side)
   (setq dialog-height dialog-base-height)
   (push `(:button nil "" #x50020007
           ,dialog-side ,dialog-height 238 22) controls)
   (incf dialog-height 6)
   (push `(:button setting-default          "���ʐݒ�(&1)"           #x50000009
           ,(incf dialog-side 4) ,dialog-height 50 14) controls)
   (push `(:button setting-local            "���[�J���ݒ�(&2)"       #x50000009
           ,(incf dialog-side 50) ,dialog-height 62 14) controls)
   (push `(:button setting-local-to-default "���[�J���ݒ聨���ʐݒ�(&3)" #x50000009
           ,(incf dialog-side 62) ,dialog-height 95 14) controls)
   (push `(:button dummy-trigger ""                                  #x50000009
           10000 ,dialog-height 0 0) controls)

   ;; ���K�\��
   (setq dialog-base-height (+ dialog-base-height 22 4))
   (setq dialog-side dialog-base-side)
   (setq dialog-height dialog-base-height)
   (push `(:button nil "���K�\�����X�g(&R)"                                   #x50020007
           ,dialog-side ,dialog-height 238 166) controls)
   (incf dialog-side 4)
   (push `(:edit layer-regexp-list nil                                        #x50a110c4
           ,dialog-side ,(incf dialog-height 10) 160 110) controls)
   (incf dialog-side 164)
   (push `(:button layer-order-non-miss "�ȗ��s��(&N)"                        #x50000009
           ,dialog-side ,dialog-height 64 12) controls)
   (push `(:button layer-order-top-miss "�擪�ȗ��\(&M)"                    #x50000009
           ,dialog-side ,(incf dialog-height 12) 64 12) controls)
   (push `(:button layer-order-any-miss "�C�ӏȗ��\(&Y)"                    #x50000009
           ,dialog-side ,(incf dialog-height 12) 64 12) controls)
   (push `(:button layer-order-disorder "���o��(���s��)(&D)"                  #x50000009
           ,dialog-side ,(incf dialog-height 12) 64 12) controls)
   (push `(:button layer-regexp-list-case-fold "�啶����������\n��ʂ���(&C)" #x50012006
           ,dialog-side ,(incf dialog-height 16) 60 18) controls)

   (setq dialog-height dialog-base-height)
   (incf dialog-height 120)
   (setq dialog-side dialog-base-side)
   (incf dialog-side 4)
   (push `(:static nil "���K�\���̃O���[�s���O���s�����ꍇ�A�Ō�̃O���[�v���^�C�g���ɂȂ�܂��B" #x50000000
           ,dialog-side ,(incf dialog-height 10) 226 16) controls)

   (setq controls (nreverse controls))
   `(dialog 0 0 260 220
     (:caption "���K�\��")
     (:control
      ,@controls)))
 #'(lambda ()
     (let ((local-p (outline-tree-local-variables-local-p
                     'outline-tree-create-outline-regexp-layer-regexp-list)))
       (list
        ;; ����/���[�J��
        (cons 'setting-default (not local-p))
        (cons 'setting-local local-p)
        (cons 'dummy-trigger (not local-p))

        ;; ���K�\�����X�g
        (cons 'layer-regexp-list
              (format nil "~{~A\r\n~}"
                      (mapcar 'outline-tree-encode-escape-sequence
                              (cond
                               ((symbolp outline-tree-create-outline-regexp-layer-regexp-list)
                                (symbol-value outline-tree-create-outline-regexp-layer-regexp-list))
                               ((listp outline-tree-create-outline-regexp-layer-regexp-list)
                                outline-tree-create-outline-regexp-layer-regexp-list)
                               (t nil)))))
        (case outline-tree-create-outline-regexp-layer-regexp-list-order
          (:top-miss
           (cons 'layer-order-top-miss t))
          (:any-miss
           (cons 'layer-order-any-miss t))
          (:disorder
           (cons 'layer-order-disorder t))
          (t
           (cons 'layer-order-non-miss t)))
        (cons 'layer-regexp-list-case-fold
              (ed::cfs2dialog outline-tree-create-outline-regexp-layer-regexp-list-case-fold)))))
 '((dummy-trigger :disable (setting-local-to-default)))
 #'(lambda (data)
     (let (local)
       ;; ����/���[�J��
       (cond ((cdr (assoc 'setting-local data))
              (setq local t)
              (outline-tree-sync-local-variables
               'outline-tree-create-outline-regexp-layer-regexp-list local))
             ((cdr (assoc 'setting-local-to-default data))
              (outline-tree-sync-local-variables
               'outline-tree-create-outline-regexp-layer-regexp-list local)))
       (cond (local
              (setq outline-tree-create-outline-regexp-layer-regexp-list
                    (mapcar #'(lambda (str) (decode-escape-sequence str t))
                            (split-string
                             (substitute-string
                              (cdr (assoc 'layer-regexp-list data)) "\r\n" "\n") #\LFD)))
              (setq outline-tree-create-outline-regexp-layer-regexp-list-case-fold
                    (ed::dialog2cfs (cdr (assoc 'layer-regexp-list-case-fold data))))
              (setq outline-tree-create-outline-regexp-layer-regexp-list-order
                    (cond ((cdr (assoc 'layer-order-non-miss data)) :non-miss)
                          ((cdr (assoc 'layer-order-top-miss data)) :top-miss)
                          ((cdr (assoc 'layer-order-any-miss data)) :any-miss)
                          ((cdr (assoc 'layer-order-disorder data)) :disorder))))
             (t
              (setq-default outline-tree-create-outline-regexp-layer-regexp-list
                            (mapcar #'(lambda (str) (decode-escape-sequence str t))
                                    (split-string
                                     (substitute-string
                                      (cdr (assoc 'layer-regexp-list data)) "\r\n" "\n") #\LFD)))
              (setq-default outline-tree-create-outline-regexp-layer-regexp-list-case-fold
                            (ed::dialog2cfs (cdr (assoc 'layer-regexp-list-case-fold data))))
              (setq-default outline-tree-create-outline-regexp-layer-regexp-list-order
                            (cond ((cdr (assoc 'layer-order-non-miss data)) :non-miss)
                                  ((cdr (assoc 'layer-order-top-miss data)) :top-miss)
                                  ((cdr (assoc 'layer-order-any-miss data)) :any-miss)
                                  ((cdr (assoc 'layer-order-disorder data)) :disorder))))))
     ;; �A�E�g���C�����ŐV�łȂ��悤�ɐݒ�
     (save-excursion
       (dolist (buffer (buffer-list))
         (set-buffer buffer)
         (when (eq (outline-tree-get-create-outline-function-by-buffer buffer)
                   'outline-tree-create-outline-regexp)
           (setq outline-tree-outline-modified nil))))))

;;; cr-regexp.l ends here