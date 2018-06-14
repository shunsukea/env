;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is part of xyzzy.
;;;

#|

�o�b�N�A�b�v�t�@�C���̍쐬����D�݂̏ꏊ�ɕύX���܂��B
(require "backup") ��������ŁA�ȉ��̕ϐ����D�݂̒l�ɐݒ肵�Ă��������B


*backup-directory*
�o�b�N�A�b�v�t�@�C�����쐬����f�B���N�g�����w�肷��B

*hierarchic-backup-directory*
�o�b�N�A�b�v�f�B���N�g���̊K�w���@�邩?

*auto-save-to-backup-directory*
nil�łȂ���΁A�����Z�[�u�t�@�C���������Ŏw�肷��o�b�N�A�b�v�f�B���N�g
���ɕۑ�����(�f�t�H���g��nil)


*backup-directory*�ɐݒ�ł���l�͈ȉ��̒ʂ�B

 1 nil (�f�t�H���g)
 2 ������
 3 ���X�g
 4 �֐�

�����Ƃ��������͖ʓ|�Ȃ̂ŏȗ��B������ė������āB

1 (setq *backup-directory* nil)

  �ۑ�����t�@�C���Ɠ����f�B���N�g���Ƀo�b�N�A�b�v�t�@�C�����쐬�����B
  �܂�A�]���ʂ�B

2.1 (setq *backup-directory* "c:/backup-files/")
    (setq *hierarchic-backup-directory* nil)

  ���̏ꍇ�A���ׂẴo�b�N�A�b�v�t�@�C���� c:/backup-files/ �̉��ɍ쐬�����B

2.2 (setq *backup-directory* "c:/backup-files/")
    (setq *hierarchic-backup-directory* t)

  c:/foo/bar.txt �̃o�b�N�A�b�v�t�@�C���� c:/backup-files/c/foo/
  c:/foo/bar/baz.txt �̃o�b�N�A�b�v�t�@�C���� c:/backup-files/c/foo/bar/
  d:/foo/bar.txt �̃o�b�N�A�b�v�t�@�C���� c:/backup-files/d/foo/
  //foo/bar/baz/qux.txt �̃o�b�N�A�b�v�t�@�C���� c:/backup-files/foo/bar/baz/
  �ɂ��ꂼ��쐬�����B

2.3 (setq *backup-directory* "BACKUP")

  *backup-directory* �� '/' ���܂܂Ȃ��ꍇ�A
  c:/foo/bar.txt �̃o�b�N�A�b�v�t�@�C���� c:/foo/BACKUP/
  c:/foo/bar/baz.txt �̃o�b�N�A�b�v�t�@�C���� c:/foo/bar/BACKUP/
  �ɂ��ꂼ��쐬�����B

  *hierarchic-backup-directory* �̒l�͊֌W�Ȃ��B

  (setq *backup-directory* "../BACKUP") �̂悤�ȏꍇ�́A'/' ���܂�ł���
  �̂� 2.1 �܂��� 2.2 �ƂȂ�B

3.1 (setq *backup-directory*
	  '(("c:/foo/" . "c:/backup-files-c/")
	    ("d:/bar/" . "c:/backup-files-d/")))
    (setq *hierarchic-backup-directory* nil)

  c:/foo/bar.txt, c:/foo/bar/baz.txt ���Ac:/foo/ �Ŏn�܂�t�@�C���́A���ׂ�
  c:/backup-files-c/ �̉��ɍ쐬����A
  d:/bar/baz.txt, d:/bar/baz/qux.txt ���Ad:/bar/ �Ŏn�܂�t�@�C���́A���ׂ�
  c:/backup-files-d/ �̉��ɍ쐬�����B
  ����ȊO�̃t�@�C���́A�ۑ�����t�@�C���Ɠ����f�B���N�g���ɍ쐬�����B

3.2 (setq *backup-directory*
	  '(("c:/foo/" . "c:/backup-files-c/")
	    ("d:/bar/" . "c:/backup-files-d/")))
    (setq *hierarchic-backup-directory* t)

  c:/foo/bar.txt �̃o�b�N�A�b�v�t�@�C���� c:/backup-files-c/
, c:/foo/bar/baz.txt �̃o�b�N�A�b�v�t�@�C���� c:/backup-files-c/bar/
  d:/bar/baz.txt �̃o�b�N�A�b�v�t�@�C���� c:/backup-files-d/
  d:/bar/baz/qux.txt �̃o�b�N�A�b�v�t�@�C���� c:/backup-files-d/baz/
  �ɂ��ꂼ��쐬�����B
  ����ȊO�̃t�@�C���́A�ۑ�����t�@�C���Ɠ����f�B���N�g���ɍ쐬�����B

3.3 3.1, 3.2 ����

3.3.1 ���X�g�Ɉ�v���Ȃ��t�@�C���̃o�b�N�A�b�v����w�肷��ꍇ�́A

    (setq *backup-directory*
	  '(("c:/foo/" . "c:/backup-files-c/")
	    ("d:/bar/" . "c:/backup-files-d/")
	    ("" . "c:/common-backup/")))

  �Ƃ���� OK�B

3.3.2 �f�B���N�g����\�킷�ꍇ�́A������ '/' ��t����B

  ("c:/foo" . "c:/backup-files/")

  �Ǝw�肵���ꍇ�Ac:/foo/bar.txt �����łȂ��Ac:/food/drink.txt �ɂ��A
  c:/foot/ball.txt �ɂ��}�b�`���邱�Ƃɒ��ӁB
  �������Ac:/fool.txt �ɂ̓}�b�`���Ȃ��B

3.3.3 ���X�g�͍�����]�������B

    (setq *backup-directory*
	  '(("c:/foo/" . "c:/backup-files-foo/")
	    ("c:/foo/bar/" . "c:/backup-files-fubar/")))

  �̏ꍇ�Ac:/foo/bar/baz.txt �� c:/foo/ �Ƀ}�b�`����B

    (setq *backup-directory*
	  '(("c:/foo/bar/" . "c:/backup-files-fubar/")
	    ("c:/foo/" . "c:/backup-files-foo/")))

  �Ƃ���̂��g�B

  ���l�ɁA

    (setq *backup-directory*
	  '(("" . "c:/common-backup/")
	    ("c:/foo/" . "c:/backup-files-c/")
	    ("d:/bar/" . "c:/backup-files-d/")))

  �́A���ׂ� c:/common-backup/ �Ƀo�b�N�A�b�v����Ă܂��B

3.3.4 �o�b�N�A�b�v�悪 nil �Ȃ�A�ۑ�����t�@�C���Ɠ����f�B���N�g���ɍ�
      �������B

    (setq *backup-directory*
	  '(("c:/foo/")
	    ("d:/bar/" . "c:/backup-files-d/")
	    ("" . "c:/common-backup/")))

���̏ꍇ�Ac:/foo/ �Ƀ}�b�`����t�@�C���̃o�b�N�A�b�v�t�@�C���́A�����f�B
���N�g���ɍ쐬�����B

4 �֐�
  �t�@�C�����������Ƃ��� funcall �\�Ȋ֐��łȂ���΂Ȃ�Ȃ��B
  �߂�l�� nil �̏ꍇ�́A�ۑ�����t�@�C���Ɠ����f�B���N�g���ɍ쐬�����B
  �߂�l�� nil �łȂ��ꍇ�A������łȂ���΂Ȃ炸�A2 �Ɠ��l�ɏ��������B

��1) �p�X�̋�؂�� '\' �͎󂯕t���Ȃ��B�K�� '/' ���g�p���邱�ƁB
��2) �Ԉ�����ݒ������ƁA�t�@�C���̃Z�[�u���ł��Ȃ��Ȃ�̂ŋC�����ĂˁB
     �����A�ǂ����悤���Ȃ��Ȃ�����A

     (setq *backup-directory* nil)

     ��]�����邩�Awrite-file ���œ�����B

|#

(provide "backup")

(in-package "editor")

(export '(*backup-directory* *hierarchic-backup-directory*))

(or (boundp '*backup-directory*)
    (setq *backup-directory* nil))

(or (boundp '*hierarchic-backup-directory*)
    (setq *hierarchic-backup-directory* nil))

(or (boundp '*auto-save-to-backup-directory*)
    (setq *auto-save-to-backup-directory* nil))

(defun make-backup-directory-hook (filename)
  (let* ((dir (directory-namestring filename))
	 (templ nil)
	 (backup (cond ((null *backup-directory*)
			(return-from make-backup-directory-hook nil))
		       ((stringp *backup-directory*)
			*backup-directory*)
		       ((consp *backup-directory*)
			(dolist (x *backup-directory*
				   (return-from make-backup-directory-hook nil))
			  (setq templ (car x))
			  (when (and (>= (length dir) (length templ))
				     (string-equal templ dir :end2 (length templ)))
			    (when (null (cdr x))
			      (return-from make-backup-directory-hook nil))
			    (return (cdr x)))))
		       (t
			(or (funcall *backup-directory* filename)
			    (return-from make-backup-directory-hook nil))))))
    (when (prog1
	      (and *hierarchic-backup-directory*
		   (find #\/ backup))
	    (setq backup (append-trail-slash (merge-pathnames backup dir))))
      (setq backup (format nil "~A~{~A/~}" backup
			   (split-string (subseq dir (length templ)) "/:"))))
    (create-directory backup :if-exists :skip)
    backup))

(setq *make-backup-filename-hook* #'make-backup-directory-hook)