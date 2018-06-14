;;; -*- Mode: Lisp; Package: OUTLINE-TREE2 -*-
;;; cr-regexp-internal.l --- outline-tree

;; Copyright (C) 2001-2005 OHKUBO Hiroshi <ohkubo@s53.xrea.com>

;; Author: OHKUBO Hiroshi <ohkubo@s53.xrea.com>
;; Time-stamp: <2005/11/12 14:21:05 +0900>

;;; Code:

(provide "outline-tree/cr-regexp-internal")

(require "search"); dialog2cfs, cfs2dialog
(require "outline-tree/outline-tree")

(in-package "outline-tree2")

;;; �A�E�g���C���쐬�֐�
(defun outline-tree-create-outline-regexp-internal
  (&key root-node
        layer-regexp-list layer-regexp-list-case-fold (layer-regexp-list-order :non-miss)
        create-sub-outline-func title-converter sub-type line-oriented)
  "���K�\���A�E�g���C���쐬���� function
ROOT-NODE
LAYER-REGEXP-LIST
LAYER-REGEXP-LIST-CASE-FOLD
LAYER-REGEXP-LIST-ORDER: :top-miss, :any-miss, :non-miss, :disorder"
  (labels (;; ************************************************************
           ;; �K�w���� 
           ;; ************************************************************
           (outline-tree-create-outline-regexp-order (&key root-node
                                                           layer-regexp-list
                                                           layer-regexp-list-order
                                                           create-sub-outline-func
                                                           line-oriented)
             (let ((current-regexp (car layer-regexp-list))
                   (layer-regexp-list (cdr layer-regexp-list))
                   header-node range-node value-list from next-from title-from title-to
                   whole-range title-range contents-from regnum title)
               (save-excursion
                 (goto-char (point-min))
                 ;; ****************************************
                 ;; �T�u�A�E�g���C���쐬
                 ;; ****************************************
                 (unless current-regexp
                   (when create-sub-outline-func
                     (funcall create-sub-outline-func root-node))
                   (return-from outline-tree-create-outline-regexp-order))
                 ;; ****************************************
                 ;; ���݂̐��K�\���ł̃}�b�`�����擾
                 ;; ****************************************
                 (when current-regexp
                   (while (scan-buffer current-regexp :regexp t)
                     (setq from next-from
                           next-from (point))
                     (setq whole-range (cons from next-from)
                           title-range (cons title-from title-to))
                     ;; from �� nil �̂��̂��Ƃ肠���� push
                     (push (list whole-range title-range contents-from) value-list)
                     (setq contents-from (match-end 0))
                     (when line-oriented
                       (goto-char contents-from)
                       (unless (bolp)
                         (goto-eol)
                         (forward-char)
                         (setq contents-from (point))))
                     (if (setq regnum (dotimes (i 9) (if (match-string (- 9 i)) (return (- 9 i)))))
                         ;; ���K�\������^�C�g���擾
                         (setq title-from (match-beginning regnum)
                               title-to (match-end regnum))
                       ;; �擪�s����^�C�g���擾
                       (save-excursion
                         (goto-char (match-beginning 0))
                         (setq title-from (point)
                               title-to (progn (goto-eol) (point)))))
                     (goto-char contents-from))
                   ;; �Ō�̈�񕪂� push
                   (when next-from
                     (setq whole-range (cons next-from (if (= (point-max) (buffer-size)) nil (point-max)))
                           title-range (cons title-from title-to))
                     (push (list whole-range title-range contents-from) value-list))
                   ;; ���Ԃ��t�ɂ�����ŁAfrom �� nil �̂��̂���菜��
                   (setq value-list (cdr (nreverse value-list))))
                 ;; ******************************
                 ;; ���K�\���}�b�`���ɏ]���������{
                 ;; ******************************
                 (cond
                  ;; ���݂̐��K�\���Ƀ}�b�`���镔�������������ꍇ
                  (value-list
                   ;; layer-regexp-list-order ���X�V
                   (when (eq layer-regexp-list-order :top-miss)
                     (setq layer-regexp-list-order :non-miss))
                   ;; ���݂̐��K�\���Ƀ}�b�`�J�n����܂ł͈̔͂�����
                   (if line-oriented
                       (setq whole-range
                             (cons (save-excursion
                                     (goto-char (point-min))
                                     (if (bolp) (point) (progn (goto-eol) (forward-char) (point))))
                                   (car (first (car value-list)))))
                     (setq whole-range (cons (point-min) (car (first (car value-list))))))
                   (when (< (outline-tree-get-range-from whole-range)
                            (outline-tree-get-range-to whole-range))
                     (save-excursion
                       (save-restriction
                         ;; �T�u�A�E�g���C���쐬
                         (narrow-to-region (outline-tree-get-range-from whole-range)
                                           (outline-tree-get-range-to whole-range))
                         (setq header-node
                               (outline-tree-insert-range-node *outline-tree-header-node-name*
                                                               root-node
                                                               :whole-range whole-range
                                                               :sub-type :header-node))
                         (outline-tree-create-outline-regexp-order
                          :root-node header-node
                          :create-sub-outline-func create-sub-outline-func)
                         (when (and (not (outline-tree-node-has-children-p header-node))
                                    (not (and (eq sub-type :heading-node)
                                              (outline-tree-buffer-node-p root-node))))
                           (outline-tree-remove-range-node header-node)))))
                   ;; ���݂̐��K�\���Ƀ}�b�`�����ӏ�����J�n���邻�ꂼ��͈̔͂�����
                   ;; (�K�w��`���K�\�����X�g)
                   (dolist (value value-list)
                     (setq whole-range (first value)
                           title-range (second value)
                           contents-from (third value))
                     (setq title (buffer-substring (outline-tree-get-range-from title-range)
                                                   (outline-tree-get-range-to title-range)))
                     (when title-converter
                       (setq title (funcall title-converter title)))
                     (setq range-node (outline-tree-insert-range-node
                                       title root-node :title-range title-range :whole-range whole-range
                                       :sub-type sub-type))
                     (when (< contents-from (outline-tree-get-range-to whole-range))
                       (save-restriction
                         (narrow-to-region contents-from
                                           (outline-tree-get-range-to whole-range))
                         (outline-tree-create-outline-regexp-order
                          :root-node range-node
                          :layer-regexp-list layer-regexp-list
                          :layer-regexp-list-order layer-regexp-list-order
                          :create-sub-outline-func create-sub-outline-func
                          :line-oriented line-oriented))))
                   (caaar value-list))
                  ;; ���K�\���Ƀ}�b�`���镔���������炸�A���ꂪ���e�����ꍇ
                  ((member layer-regexp-list-order '(:top-miss :any-miss))
                   (outline-tree-create-outline-regexp-order
                    :root-node root-node
                    :layer-regexp-list layer-regexp-list
                    :layer-regexp-list-order layer-regexp-list-order
                    :create-sub-outline-func create-sub-outline-func
                    :line-oriented line-oriented))
                  (t
                   (outline-tree-create-outline-regexp-order
                    :root-node root-node
                    :create-sub-outline-func create-sub-outline-func))))))
           ;; ************************************************************
           ;; �K�w���
           ;; ************************************************************
           (outline-tree-create-outline-regexp-disorder (&key root-node
                                                              layer-regexp-list
                                                              create-sub-outline-func
                                                              line-oriented)
             (let (current-regexp match-alist regexp-position regnum header-node title)
               (save-excursion
                 ;; ****************************************
                 ;; ����̊K�w���ǂ̐��K�\���ō쐬���邩���
                 ;; ****************************************
                 ;; �e���K�\�����}�b�`����ŏ��̃|�C���g���擾
                 (dolist (regexp layer-regexp-list)
                   (goto-char (point-min))
                   (when (and regexp
                              (scan-buffer regexp :regexp t))
                     (push (cons regexp (point)) match-alist)))
                 ;; �ǂ̐��K�\���Ɏn�߂Ƀ}�b�`�������m�F
                 (when match-alist
                   (setq current-regexp (caar (stable-sort match-alist #'< :key #'cdr)))
                   (setq regexp-position
                         (position current-regexp layer-regexp-list :test 'equal))
                   (setq layer-regexp-list
                         (append (subseq layer-regexp-list 0 regexp-position)
                                 (subseq layer-regexp-list (1+ regexp-position)))))
                 (goto-char (point-min))
                 ;; ****************************************
                 ;; �T�u�A�E�g���C���쐬
                 ;; ****************************************
                 (unless current-regexp
                   (when create-sub-outline-func
                     (funcall create-sub-outline-func root-node))
                   (return-from outline-tree-create-outline-regexp-disorder))
                 ;; ****************************************
                 ;; ���݂̐��K�\���ł̃}�b�`�����擾
                 ;; ****************************************
                 (when current-regexp
                   (let (range-node value-list
                         from next-from title-from title-to
                         whole-range title-range contents-from)
                     (while (scan-buffer current-regexp :regexp t)
                       (setq from next-from
                             next-from (point))
                       (setq whole-range (cons from next-from)
                             title-range (cons title-from title-to))
                       ;; from �� nil �̂��̂��Ƃ肠���� push
                       (push (list whole-range title-range contents-from) value-list)
                       (setq contents-from (match-end 0))
                       (when line-oriented
                         (goto-char contents-from)
                         (unless (bolp)
                           (goto-eol)
                           (forward-char)
                           (setq contents-from (point))))
                       (if (setq regnum (dotimes (i 9) (if (match-string (- 9 i)) (return (- 9 i)))))
                           ;; ���K�\������^�C�g���擾
                           (setq title-from (match-beginning regnum)
                                 title-to (match-end regnum))
                         ;; �擪�s����^�C�g���擾
                         (save-excursion
                           (goto-char (match-beginning 0))
                           (setq title-from (point)
                                 title-to (progn (goto-eol) (point)))))
                       (goto-char contents-from))
                     ;; �Ō�̈�񕪂� push
                     (when next-from
                       (setq whole-range (cons next-from (if (= (point-max) (buffer-size)) nil (point-max)))
                             title-range (cons title-from title-to))
                       (push (list whole-range title-range contents-from) value-list))
                     ;; ���Ԃ��t�ɂ�����ŁAfrom �� nil �̂��̂���菜��
                     (setq value-list (cdr (nreverse value-list)))
                     (cond
                      (value-list
                       ;; ���݂̐��K�\���Ƀ}�b�`�J�n����܂ł͈̔͂�����
                       (if line-oriented
                           (setq whole-range
                                 (cons (save-excursion
                                         (goto-char (point-min))
                                         (if (bolp) (point) (progn (goto-eol) (forward-char) (point))))
                                       (car (first (car value-list)))))
                         (setq whole-range (cons (point-min) (car (first (car value-list))))))
                       (when (< (outline-tree-get-range-from whole-range)
                                (outline-tree-get-range-to whole-range))
                         (save-excursion
                           (save-restriction
                             ;; �T�u�A�E�g���C���쐬
                             (narrow-to-region (outline-tree-get-range-from whole-range)
                                               (outline-tree-get-range-to whole-range))
                             (setq header-node
                                   (outline-tree-insert-range-node *outline-tree-header-node-name*
                                                                   root-node
                                                                   :whole-range whole-range
                                                                   :sub-type :header-node))
                             (outline-tree-create-outline-regexp-disorder
                              :root-node header-node
                              :create-sub-outline-func create-sub-outline-func)
                             (when (and (not (outline-tree-node-has-children-p header-node))
                                        (not (and (eq sub-type :heading-node)
                                                  (outline-tree-buffer-node-p root-node))))
                               (outline-tree-remove-range-node header-node)))))
                       ;; ���݂̐��K�\���Ƀ}�b�`�����ӏ�����J�n���邻�ꂼ��͈̔͂�����
                       ;; (�K�w��`���K�\�����X�g)
                       (dolist (value value-list)
                         (setq whole-range (first value)
                               title-range (second value)
                               contents-from (third value))
                         (setq title (buffer-substring (outline-tree-get-range-from title-range)
                                                       (outline-tree-get-range-to title-range)))
                         (when title-converter
                           (setq title (funcall title-converter title)))
                         (setq range-node (outline-tree-insert-range-node
                                           title root-node :title-range title-range :whole-range whole-range
                                           :sub-type sub-type))
                         (when (< contents-from (outline-tree-get-range-to whole-range))
                           (save-restriction
                             (narrow-to-region contents-from
                                               (outline-tree-get-range-to whole-range))
                             (outline-tree-create-outline-regexp-disorder
                              :root-node range-node
                              :layer-regexp-list layer-regexp-list
                              :create-sub-outline-func create-sub-outline-func
                              :line-oriented line-oriented))))
                       (caaar value-list))
                      ;; ���K�\���Ƀ}�b�`���镔���������炸�A�����I���̏ꍇ
                      (t
                       (outline-tree-create-outline-regexp-disorder
                        :root-node root-node
                        :create-sub-outline-func create-sub-outline-func)))))))))
    ;; �����J�n��
    ;; layer-regexp-list �̊e�v�f�� compiled-regexp �`����
    (when (symbolp layer-regexp-list)
      (setq layer-regexp-list (symbol-value layer-regexp-list)))
    (setq layer-regexp-list
          (mapcar #'(lambda (regexp)
                      (if (stringp regexp)
                          (compile-regexp regexp layer-regexp-list-case-fold)
                        regexp))
                  layer-regexp-list))
    (cond
     ;; �K�w���
     ((member layer-regexp-list-order '(:disorder))
      (outline-tree-create-outline-regexp-disorder
       :root-node root-node
       :layer-regexp-list layer-regexp-list
       :create-sub-outline-func create-sub-outline-func
       :line-oriented line-oriented))
     ;; �K�w����
     (t
      (unless (member layer-regexp-list-order '(:top-miss :any-miss :non-miss))
        (setq layer-regexp-list-order :non-miss))
      (outline-tree-create-outline-regexp-order
       :root-node root-node
       :layer-regexp-list layer-regexp-list
       :layer-regexp-list-order layer-regexp-list-order
       :create-sub-outline-func create-sub-outline-func
       :line-oriented line-oriented)))
    (outline-tree-node-has-children-p root-node)))

;;; cr-regexp-internal.l ends here