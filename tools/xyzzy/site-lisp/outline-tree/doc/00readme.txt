-*- outline-tree: topic -*-
======================================================================
                            outline-tree
                            ------------

    Copyright (C) 2001-2012 OHKUBO Hiroshi <ohkubo@s53.xrea.com>

    Author: OHKUBO Hiroshi <ohkubo@s53.xrea.com>
    Time-stamp: <2012/03/30 21:39:50 +0900>
======================================================================

���T�v

  TreeView.dll ��p���āA�o�b�t�@���X�g�̃c���[�\���A�o�b�t�@���̃A
  �E�g���C���\�����s���܂��B

  outline-tree �y�сA�g�p���Ă��� treeview ���C�u�������񋟂���@�\
  �̊T�v���ȉ��ɗ񋓂��܂��B

  ����{�@�\
   - �o�b�t�@���X�g�̃c���[�\��
      - �o�b�t�@�̑I��
      - �t�H���_�����
   - �o�b�t�@���A�E�g���C���\��
      - �o�b�t�@���m�[�h�ʒu�ւ̈ړ�
  ��Treeview�ݒ�E����
   - TreeView �̊e��ݒ�
   - TreeView �E�B���h�E�ʒu�̕ύX
   - TreeView �E�B���h�E�̊J��
   - TreeView �E�B���h�E��L�[�o�C���h�̐ݒ� (Alt �L�[�g�p�s��)
   - ��̂̑��삪 TreeView ��E�N���b�N����\
  ���ҏW
   - �u�͈̓m�[�h�v�̏㉺���ւ�
  ���o��
   - �e�L�X�g, HTML �o��
  �����̑�
   - �A�E�g���C���쐬�֐��̔���������
   - �t�@�C���擪�����ւ̋L�q�ɂ�� outline ��ގ����I��


  �i���[�C���O�̓��[�U���C���^���N�e�B�u�ɐݒ肷����̂ƍl���A
  outline-tree ���m�[�h�ɑΉ����郊�[�W�����������I�Ƀi���[�C���O��
  ��@�\�͗L���܂���B

���C���X�g�[��

  0. �K�v�ȃ��C�u�����𓱓����܂��B

          - treeview (treeview ���C�u����)
             | 2005/05/17 ���_�ł́Atreeview �͈ȉ���K�v�Ƃ��܂��B
             | - TreeView.dll (ver. 1.03 �ȍ~)
             | - color
             | - win-window
          - buf2html

  1. �A�[�J�C�u��W�J���� outline-tree/ �ȉ��� $XYZZY/site-lisp ��
     �R�s�[���܂��B
     toolbar-outline-tree.bmp �� $XYZZY/etc �ɃR�s�[���܂��B

  2. �K�v�Ȃ�΃o�C�g�R���p�C�����܂��B

          M-x load-library
          Load library: outline-tree/makefile

          M-x outline-tree-make-clean
          M-x outline-tree-make-all

  3. ~/.xyzzy �܂��� $XYZZY/site-lisp/siteinit.l �Ɉȉ��̃R�[�h��
     �ǉ����܂��B

          (require "outline-tree/outline-tree")

  4. ��L�̐ݒ�𔽉f�����邽�߂ɁAxyzzy ���ċN�����܂��B
     siteinit.l �ɋL�q�����ꍇ�� Ctrl �L�[�� Shift �L�[�������Ȃ���
     xyzzy ���ċN�����A�_���v�t�@�C�����č\�z���܂��B

���A���C���X�g�[��

  1. ESC ESC (outline-tree2::outline-tree-uninstall) �ƃ^�C�v���A
     outline-tree �֘A�̏��� xyzzy ����폜���܂��B

  2. outline-tree �Ɋւ���L�q���폜���܂��B

  3. siteinit.l �ɋL�q���Ă����ꍇ�� Ctrl �L�[�� Shift �L�[������
     �Ȃ��� xyzzy ���ċN�����A�_���v�t�@�C�����č\�z���܂��B

�����C�Z���X

  outline-tree �͏C��BSD���C�Z���X�Ɋ�Â��ė��p�\�ł��B
  <http://www.opensource.org/licenses/bsd-license.php>


  Copyright (C) 2001-2012, OHKUBO Hiroshi.  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions
  are met:

  1. Redistributions of source code must retain the above copyright
     notice, this list of conditions and the following disclaimer.

  2. Redistributions in binary form must reproduce the above copyright
     notice, this list of conditions and the following disclaimer in
     the documentation and/or other materials provided with the
     distribution.

  3. Neither the name of the University nor the names of its
     contributors may be used to endorse or promote products derived
     from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

��outline-tree �L�[�o�C���h

  ������

    ���K�Ȏg�p�̂��߂ɂ́A�ȉ��̐ݒ肪�K�v���ƍl���Ă��܂��B

      �ETreeView <-> Editor �̃t�H�[�J�X��؂�ւ���L�[�o�C���h
      �Eoutline ���X�V����L�[�o�C���h

    ��҂͂��ꂼ�� exkey-C-tab (Ctrl+TAB), exkey-C-return
    (Ctrl+Return) ���g�p���Ă��܂����A�����̃L�[�� xyzzy �W���ł�
    �g�p�ł���悤�ɂȂ��Ă��܂���B���̂��� outline-tree �ł͐ݒ肵
    �Ă��܂���B
    �ȉ��ł́Aexkey-C-tab �� F23�Aexkey-C-return �� F20 �Ƀ}�b�s���O
    ������ŃL�[�ݒ肷���������܂��B

    F23, F20 �Ƀ}�b�s���O���Ă���̂́A�P���ɁA��҂̂���܂ł� xyzzy
    �ݒ�Ƃ̌��ˍ����ɂ���Ă��܂��B���L���Q�l�ɂ��Đݒ���s���ꍇ�́A
    �e�l�̊��ɍ��킹�� F13�`F24 �ӂ���g�p���Ă��������B

      ;; extended-key-translate-table �ݒ�
      (set-extended-key-translate-table exkey-C-tab #\F23)
      (set-extended-key-translate-table exkey-C-return #\F20)

      ;; Editor <-> TreeView
      ; Editor -> TreeView
      (require "treeview/setup")
      (global-set-key #\F23 'treeview::treeview-focus-treeview)
      ; TreeView (outline-tree) -> Editor
      (require "outline-tree/outline-tree")
      (define-key outline-tree2::*outline-tree-map*
                  #\F23 'treeview::treeview-focus-editor)

      ;; outline �X�V
      (global-set-key #\F20 'outline-tree2::outline-tree-create-outline-and-select-node)
      (define-key outline-tree2::*outline-tree-map*
                  #\F20 'outline-tree2::outline-tree-create-outline-and-select-node)

  ���G�f�B�^��

    ��{�I�� TreeView ��ł̑���݂̂ɂ��Ă��܂��̂ŁA�L�[�ݒ�͂���
    �܂���B��L�u���Ӂv�̐ݒ�������ꍇ�A�ȉ�2�_���ݒ肳��܂��B
      �EEditor -> TreeView �ƃt�H�[�J�X��؂�ւ���L�[�o�C���h
      �Eoutline ���X�V����L�[�o�C���h

  ��TreeView ��

    [�E�N���b�N]-[�w���v]-[�L�[���蓖�Ĉꗗ] ���Q�Ƃ��������B

    ���m�[�h�Ԉړ�
        Up,        C-p
        Down,      C-n
        Left,      C-b
        Right,     C-f

        PageUp,    S-PageUp
        PageDown,  S-PageDown
        Home,      S-Home
        End,       S-End

        C-a
        C-e
        C-z
        C-v

    ���X�N���[��
        C-Up,        S-C-Up
        C-Down,      S-C-Down
        C-Left,      S-C-Left
        C-Right,     S-C-Right
        C-PageUp,    S-C-PageUp
        C-PageDown,  S-C-PageDown
        C-Home,      S-C-Home
        C-End,       S-C-End

        <              ���[�ɃX�N���[��
        >              �E�[�ɃX�N���[��

        C-l            ���Z���^�[

    ���m�[�h����
        C-s            �C���N�������^���T�[�`
        C-r            �C���N�������^���T�[�`(�t��)

    ���m�[�h���s
        Return
        ���_�u���N���b�N

    ���A�E�g���C���̍폜
        Delete

    ���G�f�B�^�����t�H�[�J�X
        (exkey-C-tab : F23)

    ���͈̓m�[�h���ւ�
        P
        N

    ���͈̓m�[�h�폜
        D

    �����̑�
        Apps           �|�b�v�A�b�v���j���[�\��
        �E�N���b�N     �|�b�v�A�b�v���j���[�\��

        C-c C-f        �t�@�C���N��

        C-x 0          TreeView �����
        C-x 1          TreeView ���J��
        C-:            TreeView ���J����

        C-c w t        �E�B���h�E�����
        C-c w b        �E�B���h�E������
        C-c w l        �E�B���h�E������
        C-c w r        �E�B���h�E���E��
        C-c w w        �E�B���h�E�����̈ʒu��

        C-x ?          �L�[����

���v���p�e�B�V�[�g

  ��Ȑݒ�́A���L����ŕ\�������v���p�e�B�V�[�g/�_�C�A���O�����
  ��\�ł��B

  �E[�E�N���b�N���j���[]-[�A�E�g���C���c���[�ݒ�]
  �E[�E�N���b�N���j���[]-[TreeView �ݒ�]

���c�[���o�[

  �A�E�g���C���c���[�̋N���E�I�����s���܂��B
  �A�E�g���C���c���[���N�����Ă���ꍇ�A�A�C�R���͉����ꂽ
  ��Ԃŕ\������܂��B

    ���N�����F�i�ʏ��Ԃ́j�A�C�R�����N���b�N�ˋN��
    �N����  �F�i������Ԃ́j�A�C�R�����N���b�N�ˏI��

���ݒ�t�@�C��

  outline-tree �J�n����
      ~/.outline-tree/autoload/ �ȉ��� *.l(c)
      ~/.outline-tree/config.l(c)
  �t�@�C����ǂݍ��݂܂��B

���Q�l�t�H�[�}�b�g

  ���\�����G�f�B�^ (STED)
  �E���� �� HP
    http://www008.upp.so-net.ne.jp/momotan/

  ��eMemoPad
  �EeMemoPad Home
    http://www.ememopad.net/

  ��ChangeLog
  �E�����̃z�[���y�[�W
    http://nais.to/~yto/

  �ELet's try ChangeLog MEMO
    http://pop-club.hp.infoseek.co.jp/emacs/changelog.html

  �E�����v���O���~���O: Unix�̃����Z�p
    http://namazu.org/~satoru/unimag/1/index.html

  ��RD
  �ERD working draft ���{���
    http://www.rubyist.net/~rubikitch/RDP.cgi?cmd=view&name=RD

  �ERD���n��
    http://www.rubyist.net/~rubikitch/computer/rd-intro/

  �EWhat is RD? What is RDtool?
    http://www2.pos.to/~tosh/ruby/rdtool/ja/whats.html

  ��Markdown
  �EDaring Fireball: Markdown Syntax Documentation
    http://daringfireball.net/projects/markdown/syntax

  ��Hiki
  �EHiki�g�b�v�y�[�W
    http://www.namaraii.com/hiki/

  �E���`���[��
    http://www.namaraii.com/hiki/?TextFormattingRules

  ��RFC
  �E�T���v��: Internet Small Computer Systems Interface (iSCSI)
    http://www.ietf.org/rfc/rfc3720.txt

