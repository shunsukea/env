# -*- mode: python; coding: utf-8-with-signature-dos -*-

##                          nickname: fakeymacs config
##
## Windows �̑���� emacs �̃L�[�o�C���h�ōs�����߂̐ݒ�iKeyhac�Łjver.20161120_01
##

# ���̃X�N���v�g�́AKeyhac for Windows ver 1.75 �ȍ~�œ��삵�܂��B
#   https://sites.google.com/site/craftware/keyhac-ja
# �X�N���v�g�ł��̂ŁA�g���₷���悤�ɃJ�X�^�}�C�Y���Ă����p���������B
#
# ���̓��e�́Autf-8-with-signature-dos �� coding-system �� config.py �̖��O�ŃZ�[�u����
# ���p���Ă��������B
#
# �{�ݒ�𗘗p���邽�߂̎d�l�́A�ȉ����Q�Ƃ��Ă��������B
#
# �����ʂ̎d�l��
# �Enot_emacs_target �ϐ��� ime_target �ϐ��ŁAemacs�L�[�o�C���h�� IME �̐؂�ւ��L�[�o�C���h
#   �̑ΏۂƂ���A�v���P�[�V�����\�t�g���w��ł���B
# �E���{��Ɖp��̂ǂ���̃L�[�{�[�h�𗘗p���邩�� is_japanese_keyboard �ϐ��Ŏw��ł���B
# �E���E�ǂ���� Ctrl�L�[���g������ side_of_ctrl_key �ϐ��Ŏw��ł���B
# �E���E�ǂ���� Alt�L�[���g������ side_of_alt_key �ϐ��Ŏw��ł���B
# �E�L�[�o�C���h�̒�`�ł͈ȉ��̕\�L�����p�ł���B
#   �ES-    : Shift�L�[
#   �EC-    : Ctrl�L�[
#   �EA-    : Alt�L�[
#   �EM-    : Alt�L�[ �� Esc�AC-[ �̃v���t�B�b�N�X�L�[�𗘗p����R�p�^�[�����`
#             �iemacs�L�[�o�C���h�ݒ�ŗ��p�Bemacs �� Meta �Ɠ��l�̈Ӗ��B�j
#   �ECtl-x : ctl_x_prefix_key �ϐ��Œ�`����Ă���v���t�B�b�N�X�L�[�ɒu����
#             �iemacs�L�[�o�C���h�ݒ�ŗ��p�B�ϐ��̈Ӗ��͈ȉ����Q�Ƃ̂��ƁB�j
#   �E(999) : ���z�L�[�R�[�h�w��
#
# ��emacs�L�[�o�C���h�ݒ�� IME �̐؂�ւ��ݒ��L���ɂ����A�v���P�[�V�����\�t�g�ł̓�����
# �Etoggle_input_method_key �ϐ��̐ݒ�ɂ��AIME ��؂�ւ���L�[���w��ł���B
# �Euse_emacs_ime_mode �ϐ��̐ݒ�ɂ��Aemacs���{����̓��[�h���g�����ǂ������w��
#   �ł���Bemacs���{����̓��[�h�́AIME �� ON �̎��ɕ����i�p�������X�y�[�X������
#   ���ꕶ���j����͂���ƋN������B
#   emacs���{����̓��[�h�ł́A�ȉ��̃L�[�݂̂� emacs�L�[�o�C���h�Ƃ��ė��p�ł��A
#   ���̑��̃L�[�� Windows �ɂ��̂܂ܓn�����悤�ɂȂ邽�߁AIME �̃V���[�g�J�b�g�L�[
#   �Ƃ��ė��p���邱�Ƃ��ł���B
#   �Eemacs���{����̓��[�h�Ŏg���� emacs�L�[�o�C���h�L�[
#     �EC-[
#     �EC-b�AC-f
#     �EC-p�AC-n
#     �EC-a�AC-e
#     �EC-h
#     �EC-d
#     �EC-m
#     �EC-g
#     �Escroll_key �ϐ��Ŏw�肵���X�N���[���L�[
#     �Etoggle_emacs_ime_mode_key �ϐ��Ŏw�肵���L�[
#      �iemacs�L�[�o�C���h�p�̃L�[�ł͂Ȃ����Aemacs���{����̓��[�h��؂�ւ���L�[�j
#   emacs���{����̓��[�h�́A�ȉ��̑���ŏI������B
#   �EEnter�AC-m �܂��� C-g �������ꂽ�ꍇ
#   �E[���p�^�S�p] �L�[�AA-` �L�[�������ꂽ�ꍇ
#   �EBS�AC-h ��������� toggle_input_method_key �ϐ��Ŏw�肵���L�[�������ꂽ�ꍇ
#     �i�Ԉ���ē��{����͂����Ă��܂������̃L�[�����z�肵�Ă̑΍�j
#   �Etoggle_emacs_ime_mode_key �ϐ��Ŏw�肵���L�[�������ꂽ�ꍇ
# �Eemacs���{����̓��[�h�̎g�p��L���ɂ����ہAemacs_ime_mode_balloon_message �ϐ���
#   �ݒ�Ńo���[�����b�Z�[�W�Ƃ��ĕ\�����镶������w��ł���B
# �Euse_emacs_shift_mode �ϐ��̐ݒ�ɂ��Aemacs�V�t�g���[�h���g�����ǂ������w��ł���B
#   emacs�V�t�g���[�h���g���ꍇ�͈ȉ��̓����ƂȂ�B
#   �EC-[a-z]�L�[�� Shift�L�[�ƈꏏ�ɉ��������́AShift�L�[���Ƃ����L�[�iC-[a-z]�j��
#     Windows �ɓ��͂����B
#   �EA-[a-z]�L�[�� Shift�L�[�ƈꏏ�ɉ��������́AShift�L�[���Ƃ����L�[�iA-[a-z]�j��
#     Windows �ɓ��͂����B
#
# ��emacs�L�[�o�C���h�ݒ��L���ɂ����A�v���P�[�V�����\�t�g�ł̓�����
# �Euse_ctrl_i_as_tab �ϐ��̐ݒ�ɂ��AC-i�L�[�� Tab�L�[�Ƃ��Ďg�����ǂ������w��ł���B
# �Euse_esc_as_meta �ϐ��̐ݒ���AEsc�L�[�� Meta�L�[�Ƃ��Ďg�����ǂ������w��ł���B
#   use_esc_as_meta �ϐ��� True�iMeta�L�[�Ƃ��Ďg���j�ɐݒ肳��Ă���ꍇ�AESC ��
#   ��񉟉��� ESC �����͂����B
# �Ectl_x_prefix_key �ϐ��̐ݒ�ɂ��ACtl-x�v���t�B�b�N�X�L�[�Ɏg���L�[���w��ł���B
# �Escroll_key �ϐ��̐ݒ�ɂ��A�X�N���[���Ɏg���L�[���w��ł���B
# �EC-c�AC-z �́AWindows �́u�R�s�[�v�A�u�������v���@�\����悤�ɂ��Ă���B
#   ctl_x_prefix_key �ϐ��� C-x �ȊO�ɐݒ肳��Ă���ꍇ�ɂ́AC-x �� Windows ��
#   �u�J�b�g�v�Ƃ��ċ@�\����悤�ɂ��Ă���B
# �EC-k ��A�����Ď��s���Ă��A�N���b�v�{�[�h�ւ̍폜������̒~�ς͍s���Ȃ��B
#   �����s���ꊇ���ăN���b�v�{�[�h�ɓ��ꂽ���ꍇ�́A�폜�͈̔͂��}�[�N���č폜���邩
#   �O�u�������w�肵�č폜����B
# �EC-y ��O�u�������w�肵�Ď��s����ƁA�����N�i�y�[�X�g�j�̌J��Ԃ����s����B
# �EC-l �́A�A�v���P�[�V�����\�t�g�ʑΉ��Ƃ���Brecenter �֐��ŌʂɎw�肷�邱�ƁB
#   ���̐ݒ�ł́ASakura Editor �̂ݑΉ����Ă���B
# �E�L�[�{�[�h�}�N���� emacs �̋����ƈقȂ�AIME �̕ϊ��L�[���܂߂����͂����L�[���̂��̂�
#   �L�^����B���̂��߁A�L�[�{�[�h�}�N���L�^����Đ����AIME �̏�Ԃɗ��ӂ������p���K�v�B
#
# ���S�ẴA�v���P�[�V�����\�t�g�ŋ��ʂ̓�����
# �Eother_window_key �ϐ��ɐݒ肵���L�[�ɂ��A�\�����Ă���E�C���h�E�̒��ŁA��ԍŋ�
#   �܂Ńt�H�[�J�X���������E�C���h�E�Ɉړ�����BNTEmacs �̋@�\�⃉���`���[�̋@�\����
#   Windows �A�v���P�[�V�����\�t�g���N�������ۂɁA�N�����̃A�v���P�[�V�����\�t�g�ɖ߂�
#   �̂ɕ֗��B���̋@�\�� Ctl-x o �ɂ����蓖�ĂĂ��邪�A������� emacs �̃L�[�o�C���h��
#   �K�p�����A�v���P�[�V�����\�t�g�݂̂ŗL���ƂȂ�B
# �EclipboardList_key �ϐ��ɐݒ肵���L�[�ɂ��A�N���b�v�{�[�h���X�g���N������B
#   �iC-f�AC-b �Ń��X�g�̕ύX�AC-n�AC-p �Ń��X�g�����ړ����AEnter �Ŋm�肷��B
#     C-s�AC-r �Ō������\�Bmigemo ������o�^���Ă���΁A����������啶���Ŏn�߂�
#     ���Ƃ� migemo �������\�Bemacs �L�[�o�C���h��K�p���Ȃ��A�v���P�[�V�����\�t�g
#     �ł��N���b�v�{�[�h���X�g�͋N�����A�I���������ڂ� Enter �Ŋm�肷�邱�ƂŁA
#     �N���b�v�{�[�h�ւ̊i�[�i�e�L�X�g�̓\��t���ł͂Ȃ��j���s����B�j
# �ElancherList_key �ϐ��ɐݒ肵���L�[�ɂ��A�����`���[���X�g���N������B
#   �i�S�ẴA�v���P�[�V�����\�t�g�ŗ��p�\�B������@�́A�N���b�v�{�[�h���X�g�Ɠ����B�j
# �E�N���b�v�{�[�h���X�g�⃉���`���[���X�g�̃��X�g�{�b�N�X���ł́A��{�AAlt�L�[��
#   Ctrl�L�[�Ɠ����L�[�Ƃ��Ĉ����Ă���B�iC-v �� A-v �̒u�������̂ݍs���Ă��Ȃ��B�j
# �Ewindow_switching_key �ϐ��ɐݒ肵���L�[�ɂ��A�A�N�e�B�u�E�B���h�E�̐؂�ւ����s���
#   ��B�A�N�e�B�u�E�B���h�E��؂�ւ���ہA�؂�ւ�����E�B���h�E���ŏ�������Ă���ꍇ�́A
#   �E�B���h�E�̃��X�g�A�������čs����B
# �E�}���`�f�B�X�v���C�𗘗p���Ă���ۂɁAwindow_movement_key �ϐ��ɐݒ肵���L�[�ɂ��A
#   �A�N�e�B�u�E�B���h�E�̃f�B�X�v���C�Ԃ̈ړ����s����B
# �Ewindow_minimize_key �ϐ��ɐݒ肵���L�[�ɂ��A�E�B���h�E�̍ŏ����A���X�g�A���s����B
# �Edesktop_switching_key �ϐ��ɐݒ肵���L�[�ɂ��A���z�f�X�N�g�b�v�̐؂�ւ����s����B
#   �i���z�f�X�N�g�b�v�̗��p�ɂ��ẮA�ȉ����Q�Ƃ��������B
#     �Ehttp://pc-karuma.net/windows-10-virtual-desktops/
#     �Ehttp://pc-karuma.net/windows-10-virtual-desktop-show-all-window-app/
#     ���z�f�X�N�g�b�v�ؑ֎��̃A�j���[�V�������~�߂���@�͈ȉ����Q�Ƃ��������B
#     �Ehttp://www.jw7.org/2015/11/03/windows10_virtualdesktop_animation_off/ �j

import time
import sys
import os.path
import re

import keyhac_keymap
import keyhac_hook
from keyhac import *

def configure(keymap):
    keymap_global = keymap.defineWindowKeymap()
    keymap.replaceKey( "(29)", "LAlt" ) # (29) �́u���ϊ��v�L�[

    ####################################################################################################
    ## �J�X�^�}�C�Y�̐ݒ�
    ####################################################################################################

    # emacs �̃L�[�o�C���h��"�������Ȃ�"�A�v���P�[�V�����\�t�g���w�肷��
    # �iKeyhac �̃��j���[����u�������O�v�� ON �ɂ���� processname �� classname ���m�F���邱�Ƃ��ł��܂��B�j
    not_emacs_target = ["cmd.exe",            # cmd
                        "mintty.exe",         # mintty
                        "emacs.exe",          # Emacs
                        "emacs-w32.exe",      # Emacs
                        "gvim.exe",           # GVim
                        # "eclipse.exe",        # Eclipse
                        # "firefox.exe",        # firefox
                        "xyzzy.exe",          # xyzzy
                        "Code.exe",           # VSCode
                        "atom.exe",           # atom
                        "VirtualBox.exe",     # VirtualBox
                        "XWin.exe",           # Cygwin/X
                        "Xming.exe",          # Xming
                        "putty.exe",          # PuTTY
                        "ttermpro.exe",       # TeraTerm
                        "Poderosa.exe",       # Poderosa
                        "MobaXterm.exe",      # MobaXterm
                        "TurboVNC.exe",       # TurboVNC
                        "vncviewer.exe"]      # UltraVNC

    # IME �̐؂�ւ��݂̂��������A�v���P�[�V�����\�t�g���w�肷��
    # �i�w��ł���A�v���P�[�V�����\�t�g�́Anot_emacs_target �Łi���O�j�w�肵�����̂���݂̂ƂȂ�܂��B�j
    ime_target       = ["cmd.exe",            # cmd
                        "mintty.exe",         # mintty
                        "gvim.exe",           # GVim
                        # "eclipse.exe",        # Eclipse
                        # "firefox.exe",        # firefox
                        "atom.exe",           # atom
                        #"xyzzy.exe",          # xyzzy
                        "putty.exe",          # PuTTY
                        "ttermpro.exe",       # TeraTerm
                        "MobaXterm.exe"]      # MobaXterm

    # ���{��L�[�{�[�h���ǂ������w�肷��iTrue: ���{��L�[�{�[�h�AFalse: �p��L�[�{�[�h�j
    is_japanese_keyboard = True

    # ���E�ǂ���� Ctrl�L�[���g�������w�肷��i"L": ���A"R": �E�j
    side_of_ctrl_key = "L"

    # ���E�ǂ���� Alt�L�[���g�������w�肷��i"L": ���A"R": �E�j
    side_of_alt_key = "L"

    # emacs���{����̓��[�h���g�����ǂ������w�肷��iTrue: �g���AFalse: �g��Ȃ��j
    use_emacs_ime_mode = True

    # emacs���{����̓��[�h��؂�ւ���i�g�O������j�L�[���w�肷��
    toggle_emacs_ime_mode_key = "C-t"

    # emacs���{����̓��[�h���L���ȂƂ��ɕ\������o���[�����b�Z�[�W���w�肷��
    emacs_ime_mode_balloon_message = None
    #emacs_ime_mode_balloon_message = "��"

    # emacs�V�t�g���[�h���g�����ǂ������w�肷��iTrue: �g���AFalse: �g��Ȃ��j
    use_emacs_shift_mode = False

    # IME ��؂�ւ���L�[���w�肷��i�����w��j
    # toggle_input_method_key = ["C-Yen"]
    toggle_input_method_key = ["C-Yen", "C-o"]

    # C-i�L�[�� Tab�L�[�Ƃ��Ďg�����ǂ������w�肷��iTrue: �g���AFalse: �g��Ȃ��j
    use_ctrl_i_as_tab = True

    # Esc�L�[�� Meta�L�[�Ƃ��Ďg�����ǂ������w�肷��iTrue: �g���AFalse: �g��Ȃ��j
    use_esc_as_meta = False

    # Ctl-x�v���t�B�b�N�X�L�[�Ɏg���L�[���w�肷��
    # �iCtl-x�v���t�B�b�N�X�L�[�̃��f�B�t�@�C�A�L�[�́ACtrl �܂��� Alt �̂����ꂩ����w�肵�Ă��������j
    ctl_x_prefix_key = "C-x"

    # �X�N���[���Ɏg���L�[�̑g�ݍ��킹�iUp�ADown �̏��j���w�肷��
    scroll_key = None # PageUp�APageDown�L�[�݂̂𗘗p����
    #scroll_key = ["M-v", "C-v"]

    # �\�����Ă���E�C���h�E�̒��ŁA��ԍŋ߂܂Ńt�H�[�J�X���������E�C���h�E�Ɉړ�����L�[���w�肷��
    other_window_key = "A-o"

    # �N���b�v�{�[�h���X�g���N������L�[���w�肷��
    clipboardList_key = "A-y"

    # �����`���[���X�g���N������L�[���w�肷��
    lancherList_key = "A-l"

    # �A�N�e�B�u�E�B���h�E��؂�ւ���L�[�̑g�ݍ��킹�i�O�A�� �̏��j���w�肷��i�����w��j
    #window_switching_key = [["A-p", "A-n"], ["A-Up", "A-Down"]]
    window_switching_key = [["A-p", "A-n"]]

    # �A�N�e�B�u�E�B���h�E���f�B�X�v���C�Ԃňړ�����L�[�̑g�ݍ��킹�i�O�A�� �̏��j���w�肷��i�����w��j
    # �iother_window_key �Ɋ��蓖�ĂĂ��� A-o �Ƃ̘A�W�������p��z�肵�AA-C-o �����蓖�ĂĂ��܂��B�j
    window_movement_key = None # Single display
    #window_movement_key = [[None, "A-C-o"], ["A-Left", "A-Right"]] # Multi-display

    # �E�B���h�E���ŏ����A���X�g�A����L�[�̑g�ݍ��킹�i���X�g�A�A�ŏ��� �̏��j���w�肷��i�����w��j
    window_minimize_key = None #����Ȃ�
    #window_minimize_key = [["A-r", "A-m"]]

    # ���z�f�X�N�g�b�v��؂�ւ���L�[�̑g�ݍ��킹�i�O�A�� �̏��j���w�肷��i�����w��j
    # desktop_switching_key = None # for Windows 7 or 8.1
    desktop_switching_key = [["A-C-p", "A-C-n"], ["A-C-Left", "A-C-Right"]] # for Windows 10

    # shell_command �֐��ŋN������A�v���P�[�V�����\�t�g���w�肷��
    # �i�p�X���ʂ��Ă��Ȃ��ꏊ�ɂ���R�}���h�́A��΃p�X�Ŏw�肵�Ă��������B�j
    command_name = r"cmd.exe"


    ####################################################################################################
    ## ��{�ݒ�
    ####################################################################################################

    # �ϐ����i�[����N���X���`����
    class Fakeymacs:
        pass

    fakeymacs = Fakeymacs()

    fakeymacs.last_window = None

    def is_emacs_target(window):
        if window != fakeymacs.last_window:
            if window.getProcessName() == "EXCEL.EXE": # Microsoft Excel
                # �N���b�v�{�[�h�̊Ď��p�̃t�b�N�𖳌��ɂ���
                keymap.clipboard_history.enableHook(False)
            else:
                # �N���b�v�{�[�h�̊Ď��p�̃t�b�N��L���ɂ���
                keymap.clipboard_history.enableHook(True)

            fakeymacs.last_window = window

        if is_list_window(window):
            return False

        if window.getProcessName() in not_emacs_target:
            fakeymacs.keybind = "not_emacs"
            return False

        fakeymacs.keybind = "emacs"
        return True

    def is_ime_target(window):
        if window.getProcessName() in ime_target:
            return True
        return False

    if use_emacs_ime_mode:
        keymap_emacs = keymap.defineWindowKeymap(check_func=lambda wnd: is_emacs_target(wnd) and not is_emacs_ime_mode(wnd))
        keymap_ime   = keymap.defineWindowKeymap(check_func=lambda wnd: is_ime_target(wnd)   and not is_emacs_ime_mode(wnd))
    else:
        keymap_emacs = keymap.defineWindowKeymap(check_func=is_emacs_target)
        keymap_ime   = keymap.defineWindowKeymap(check_func=is_ime_target)

    # mark ���Z�b�g������ True �ɂȂ�
    fakeymacs.is_marked = False

    # �������J�n������ True �ɂȂ�
    fakeymacs.is_searching = False

    # �L�[�{�[�h�}�N���� play �� �� True �ɂȂ�
    fakeymacs.is_playing_kmacro = False

    # universal-argument �R�}���h�����s������ True �ɂȂ�
    fakeymacs.is_universal_argument = False

    # digit-argument �R�}���h�����s������ True �ɂȂ�
    fakeymacs.is_digit_argument = False

    # �R�}���h�̃��s�[�g�񐔂�ݒ肷��
    fakeymacs.repeat_counter = 1

    #  �̃��[�h�̎� True �ɂȂ�iredo �̃��[�h�̎� False �ɂȂ�j
    fakeymacs.is_undo_mode = True

    # Ctl-x�v���t�B�b�N�X�L�[���\������L�[�̉��z�L�[�R�[�h��ݒ肷��
    if ctl_x_prefix_key:
        keyCondition = keyhac_keymap.KeyCondition.fromString(ctl_x_prefix_key)

        if keyCondition.mod == MODKEY_CTRL:
            if side_of_ctrl_key == "L":
                ctl_x_prefix_vkey = [VK_LCONTROL, keyCondition.vk]
            else:
                ctl_x_prefix_vkey = [VK_RCONTROL, keyCondition.vk]
        elif keyCondition.mod == MODKEY_ALT:
            if side_of_alt_key == "L":
                ctl_x_prefix_vkey = [VK_LMENU, keyCondition.vk]
            else:
                ctl_x_prefix_vkey = [VK_RMENU, keyCondition.vk]
        else:
            print("Ctl-x�v���t�B�b�N�X�L�[�̃��f�B�t�@�C�A�L�[�́ACtrl �܂��� Alt �̂����ꂩ����w�肵�Ă�������")

    ##################################################
    ## IME �̐؂�ւ�
    ##################################################

    def toggle_input_method():
        self_insert_command("A-(25)")()
        delay(0.05)

        # IME �̏�Ԃ��i�[����
        ime_status = keymap.getWindow().getImeStatus()
        if use_emacs_ime_mode:
            fakeymacs.ei_ime_status = ime_status

        if not fakeymacs.is_playing_kmacro:
            if ime_status:
                message = "[��]"
            else:
                message = "[A]"

            # IME �̏�Ԃ��o���[���w���v�ŕ\������
            #keymap.popBalloon("ime_status", message, 500)

    ##################################################
    ## �t�@�C������
    ##################################################

    def find_file():
        self_insert_command("C-o")()

    def save_buffer():
        self_insert_command("C-s")()

    def write_file():
        self_insert_command("A-f", "A-a")()

    def dired():
        keymap.ShellExecuteCommand(None, r"explorer.exe", "", "")()

    ##################################################
    ## �J�[�\���ړ�
    ##################################################

    def backward_char():
        self_insert_command("Left")()

    def forward_char():
        self_insert_command("Right")()

    def backward_word():
        self_insert_command("C-Left")()

    def forward_word():
        self_insert_command("C-Right")()

    def previous_line():
        self_insert_command("Up")()

    def next_line():
        self_insert_command("Down")()

    def move_beginning_of_line():
        self_insert_command("Home")()

    def move_end_of_line():
        self_insert_command("End")()
        if checkWindow("WINWORD.EXE$", "_WwG$"): # Microsoft Word
            if fakeymacs.is_marked:
                self_insert_command("Left")()

    def beginning_of_buffer():
        self_insert_command("C-Home")()

    def end_of_buffer():
        self_insert_command("C-End")()

    def scroll_up():
        self_insert_command("PageUp")()

    def scroll_down():
        self_insert_command("PageDown")()

    def recenter():
        if checkWindow("sakura.exe$", "EditorClient$|SakuraView166$"): # Sakura Editor
            self_insert_command("C-h")()

    ##################################################
    ## �J�b�g / �R�s�[ / �폜 / �A���h�D
    ##################################################

    def delete_backward_char():
        self_insert_command("Back")()

    def delete_char():
        self_insert_command("Delete")()

    def backward_kill_word(repeat=1):
        fakeymacs.is_marked = True
        def move_beginning_of_region():
            for i in range(repeat):
                backward_word()
        mark(move_beginning_of_region)()
        delay()
        kill_region()

    def kill_word(repeat=1):
        fakeymacs.is_marked = True
        def move_end_of_region():
            for i in range(repeat):
                forward_word()
        mark(move_end_of_region)()
        delay()
        kill_region()

    def kill_line(repeat=1):
        fakeymacs.is_marked = True
        if repeat == 1:
            mark(move_end_of_line)()
            delay()
            if checkWindow("Hidemaru.exe$", "HM32CLIENT$"): # Hidemaru Editor
                self_insert_command("C-x")()
                delay()
                if getClipboardText() == "":
                    self_insert_command("Delete")()
            else:
                self_insert_command("C-c", "Delete")() # ���s��������悤�ɂ��邽�� C-x �ɂ͂��Ă��Ȃ�
        else:
            def move_end_of_region():
                if checkWindow("WINWORD.EXE$", "_WwG$"): # Microsoft Word
                    for i in range(repeat):
                        next_line()
                    move_beginning_of_line()
                else:
                    for i in range(repeat - 1):
                        next_line()
                    move_end_of_line()
                    forward_char()
            mark(move_end_of_region)()
            delay()
            kill_region()

    def kill_region():
        self_insert_command("C-x")()

    def kill_ring_save():
        self_insert_command("C-c")()
        if (checkWindow("sakura.exe$", "EditorClient$|SakuraView166$") or # Sakura Editor
            checkWindow("Code.exe$", "Chrome_WidgetWin_1$")):             # Visual Studio Code
            # �I������Ă��郊�[�W�����̃n�C���C�g���������邽�߂� Esc �𔭍s����
            self_insert_command("Esc")()

    def yank():
        self_insert_command("C-v")()

    def undo():
        # redo�iC-y�j�̋@�\�������Ă��Ȃ��A�v���P�[�V�����\�t�g�͏�� undo �Ƃ���
        if checkWindow("notepad.exe$", "Edit$"): # NotePad
            self_insert_command("C-z")()
        else:
            if fakeymacs.is_undo_mode:
                self_insert_command("C-z")()
            else:
                self_insert_command("C-y")()

    def redo():
        self_insert_command("C-y")()

    def set_mark_command():
        if fakeymacs.is_marked:
            fakeymacs.is_marked = False
        else:
            fakeymacs.is_marked = True

    def mark_whole_buffer():
        if checkWindow("EXCEL.EXE$", "EXCEL"): # Microsoft Excel
            # Excel �̃Z���̒��ł��@�\����悤�ɂ���΍�
            self_insert_command("C-End", "C-S-Home")()
        else:
            self_insert_command("C-Home", "C-a")()

    def mark_page():
        mark_whole_buffer()

    def open_line():
        self_insert_command("Enter", "Up", "End")()

    ##################################################
    ## �o�b�t�@ / �E�C���h�E����
    ##################################################

    def kill_buffer():
        self_insert_command("C-F4")()

    def switch_to_buffer():
        self_insert_command("C-Tab")()

    def other_window():
        window_list = getWindowList()
        for wnd in window_list[1:]:
            if not wnd.isMinimized():
                wnd.getLastActivePopup().setForeground()
                break

    ##################################################
    ## �����񌟍� / �u��
    ##################################################

    def isearch(direction):
        if fakeymacs.is_searching:
            if checkWindow("EXCEL.EXE$", None): # Microsoft Excel
                if checkWindow(None, "EDTBX$"): # �����E�B���h�E
                    self_insert_command({"backward":"A-S-f", "forward":"A-f"}[direction])()
                else:
                    self_insert_command("C-f")()
            else:
                self_insert_command({"backward":"S-F3", "forward":"F3"}[direction])()
        else:
            self_insert_command("C-f")()
            fakeymacs.is_searching = True

    def isearch_backward():
        isearch("backward")

    def isearch_forward():
        isearch("forward")

    def query_replace():
        if (checkWindow("sakura.exe$", "EditorClient$|SakuraView166$") or # Sakura Editor
            checkWindow("Hidemaru.exe$", "HM32CLIENT$")):                 # Hidemaru Editor
            self_insert_command("C-r")()
        else:
            self_insert_command("C-h")()

    ##################################################
    ## �L�[�{�[�h�}�N��
    ##################################################

    def kmacro_start_macro():
        keymap.command_RecordStart()

    def kmacro_end_macro():
        keymap.command_RecordStop()
        # �L�[�{�[�h�}�N���̏I���L�[�uCtl-x�v���t�B�b�N�X�L�[ + ")"�v�� Ctl-x�v���t�B�b�N�X�L�[���}�N����
        # �L�^����Ă��܂��̂�΍􂷂�i�L�[�{�[�h�}�N���̏I���L�[�̑O����uCtl-x�v���t�B�b�N�X�L�[ + ")"�v
        # �Ƃ��Ă��邱�Ƃɂ��ẮA�Ƃ肦���������������B�j
        if ctl_x_prefix_key and len(keymap.record_seq) >= 4:
            if (((keymap.record_seq[len(keymap.record_seq) - 1] == (ctl_x_prefix_vkey[0], True) and
                  keymap.record_seq[len(keymap.record_seq) - 2] == (ctl_x_prefix_vkey[1], True)) or
                 (keymap.record_seq[len(keymap.record_seq) - 1] == (ctl_x_prefix_vkey[1], True) and
                  keymap.record_seq[len(keymap.record_seq) - 2] == (ctl_x_prefix_vkey[0], True))) and
                keymap.record_seq[len(keymap.record_seq) - 3] == (ctl_x_prefix_vkey[1], False)):
                   keymap.record_seq.pop()
                   keymap.record_seq.pop()
                   keymap.record_seq.pop()
                   if keymap.record_seq[len(keymap.record_seq) - 1] == (ctl_x_prefix_vkey[0], False):
                       for i in range(len(keymap.record_seq) - 1, -1, -1):
                           if keymap.record_seq[i] == (ctl_x_prefix_vkey[0], False):
                               keymap.record_seq.pop()
                           else:
                               break
                   else:
                       # �R���g���[���n�̓��͂��A�����čs����ꍇ�����邽�߂̑Ώ�
                       keymap.record_seq.append((ctl_x_prefix_vkey[0], True))

    def kmacro_end_and_call_macro():
        fakeymacs.is_playing_kmacro = True
        keymap.command_RecordPlay()
        fakeymacs.is_playing_kmacro = False

    ##################################################
    ## ���̑�
    ##################################################

    def newline():
        self_insert_command("Enter")()

    def newline_and_indent():
        self_insert_command("Enter", "Tab")()

    def indent_for_tab_command():
        self_insert_command("Tab")()

    def keyboard_quit():
        # Microsoft Excel �܂��� Evernote �ȊO�̏ꍇ�AEsc �𔭍s����
        if not (checkWindow("EXCEL.EXE$", "EXCEL") or checkWindow("Evernote.exe$", "WebViewHost$")):
            self_insert_command("Esc")()
        keymap.command_RecordStop()
        if fakeymacs.is_undo_mode:
            fakeymacs.is_undo_mode = False
        else:
            fakeymacs.is_undo_mode = True

    def kill_emacs():
        self_insert_command("A-F4")()

    def universal_argument():
        if fakeymacs.is_universal_argument:
            if fakeymacs.is_digit_argument:
                fakeymacs.is_universal_argument = False
            else:
                fakeymacs.repeat_counter *= 4
        else:
            fakeymacs.is_universal_argument = True
            fakeymacs.repeat_counter *= 4

    def digit_argument(number):
        if fakeymacs.is_digit_argument:
            fakeymacs.repeat_counter = fakeymacs.repeat_counter * 10 + number
        else:
            fakeymacs.repeat_counter = number
            fakeymacs.is_digit_argument = True

    def shell_command():
        def popCommandWindow(wnd, command):
            if wnd.isVisible() and not wnd.getOwner() and wnd.getProcessName() == command:
                popWindow(wnd)()
                fakeymacs.is_executing_command = True
                return False
            return True

        fakeymacs.is_executing_command = False
        Window.enum(popCommandWindow, os.path.basename(command_name))

        if not fakeymacs.is_executing_command:
            keymap.ShellExecuteCommand(None, command_name, "", "")()

    ##################################################
    ## ���ʊ֐�
    ##################################################

    def checkWindow(processName, className):
        return ((processName is None or re.match(processName, keymap.getWindow().getProcessName())) and
                (className is None or re.match(className, keymap.getWindow().getClassName())))

    def delay(sec=0.02):
        time.sleep(sec)

    def vkeys():
        vkeys = list(keyCondition.vk_str_table.keys())
        for vkey in [VK_MENU, VK_LMENU, VK_RMENU, VK_CONTROL, VK_LCONTROL, VK_RCONTROL, VK_SHIFT, VK_LSHIFT, VK_RSHIFT, VK_LWIN, VK_RWIN]:
            vkeys.remove(vkey)
        return vkeys

    def addSideModifier(key):
        key = key.replace("C-", side_of_ctrl_key + "C-")
        key = key.replace("A-", side_of_alt_key + "A-")
        return key

    def kbd(keys):
        if keys:
            keys_lists = [keys.split()]

            if keys_lists[0][0] == "Ctl-x":
                if ctl_x_prefix_key:
                    keys_lists[0][0] = ctl_x_prefix_key
                else:
                    keys_lists = []
            elif keys_lists[0][0].startswith("M-"):
                key = re.sub("^M-", "", keys_lists[0][0])
                keys_lists[0][0] = "A-" + key
                keys_lists.append(["C-OpenBracket", key])
                if use_esc_as_meta:
                    keys_lists.append(["Esc", key])

            for keys_list in keys_lists:
                keys_list[0] = addSideModifier(keys_list[0])
        else:
            keys_lists = []

        return keys_lists

    def define_key(keymap, keys, command):
        for keys_list in kbd(keys):
            if len(keys_list) == 1:
                keymap[keys_list[0]] = command
            else:
                keymap[keys_list[0]][keys_list[1]] = command

    def self_insert_command(*keys):
        return keymap.InputKeyCommand(*list(map(addSideModifier, keys)))

    if use_emacs_ime_mode:
        def self_insert_command2(*keys):
            func = self_insert_command(*keys)
            def _func():
                func()
                if fakeymacs.ei_ime_status:
                    enable_emacs_ime_mode()
            return _func
    else:
        def self_insert_command2(*keys):
            return self_insert_command(*keys)

    def digit(number):
        def _func():
            if fakeymacs.is_universal_argument:
                digit_argument(number)
            else:
                reset_undo(reset_counter(reset_mark(repeat(self_insert_command2(str(number))))))()
        return _func

    def digit2(number):
        def _func():
            fakeymacs.is_universal_argument = True
            digit_argument(number)
        return _func

    def mark(func):
        def _func():
            if fakeymacs.is_marked:
                # D-Shift ���ƁAM-< �� M-> �������ɁAD-Shift ����������Ă��܂��B���̑΍�B
                self_insert_command("D-LShift", "D-RShift")()
                delay()
                func()
                self_insert_command("U-LShift", "U-RShift")()
            else:
                func()
        return _func

    def reset_mark(func):
        def _func():
            func()
            fakeymacs.is_marked = False
        return _func

    def reset_counter(func):
        def _func():
            func()
            fakeymacs.is_universal_argument = False
            fakeymacs.is_digit_argument = False
            fakeymacs.repeat_counter = 1
        return _func

    def reset_undo(func):
        def _func():
            func()
            fakeymacs.is_undo_mode = True
        return _func

    def reset_search(func):
        def _func():
            func()
            fakeymacs.is_searching = False
        return _func

    def repeat(func):
        def _func():
            # �ȉ��̂Q�s�́A�L�[�{�[�h�}�N���̌J��Ԃ����s�̍ۂɕK�v�Ȑݒ�
            repeat_counter = fakeymacs.repeat_counter
            fakeymacs.repeat_counter = 1
            for i in range(repeat_counter):
                func()
        return _func

    def repeat2(func):
        def _func():
            if fakeymacs.is_marked:
                fakeymacs.repeat_counter = 1
            repeat(func)()
        return _func

    def repeat3(func):
        def _func():
            func(fakeymacs.repeat_counter)
        return _func

    ##################################################
    ## �L�[�o�C���h
    ##################################################

    # �L�[�o�C���h�̒�`�ɗ��p���Ă���\�L�̈Ӗ��͈ȉ��̂Ƃ���ł��B
    # �ES-    : Shift�L�[
    # �EC-    : Ctrl�L�[
    # �EA-    : Alt�L�[
    # �EM-    : Alt�L�[ �� Esc�AC-[ �̃v���t�B�b�N�X�L�[�𗘗p����R�p�^�[�����`�iemacs �� Meta �Ɠ��l�j
    # �ECtl-x : ctl_x_prefix_key �ϐ��Œ�`����Ă���v���t�B�b�N�X�L�[�ɒu����
    # �E(999) : ���z�L�[�R�[�h�w��

    # https://github.com/crftwr/keyhac/blob/master/keyhac_keymap.py
    # https://github.com/crftwr/pyauto/blob/master/pyauto_const.py
    # http://www.yoshidastyle.net/2007/10/windowswin32api.html
    # http://homepage3.nifty.com/ic/help/rmfunc/vkey.htm
    # http://www.azaelia.net/factory/vk.html

    ## �}���`�X�g���[�N�L�[�̐ݒ�
    define_key(keymap_emacs, "Ctl-x",         keymap.defineMultiStrokeKeymap(ctl_x_prefix_key))
    define_key(keymap_emacs, "C-q",           keymap.defineMultiStrokeKeymap("C-q"))
    define_key(keymap_emacs, "C-OpenBracket", keymap.defineMultiStrokeKeymap("C-OpenBracket"))
    if use_esc_as_meta:
        define_key(keymap_emacs, "Esc", keymap.defineMultiStrokeKeymap("Esc"))

    ## �����L�[�̐ݒ�
    for key in range(10):
        s_key = str(key)
        define_key(keymap_emacs,        s_key, digit(key))
        define_key(keymap_emacs, "C-" + s_key, digit2(key))
        define_key(keymap_emacs, "M-" + s_key, digit2(key))
        define_key(keymap_emacs, "S-" + s_key, reset_undo(reset_counter(reset_mark(repeat(self_insert_command2("S-" + s_key))))))
        define_key(keymap_ime,          s_key, self_insert_command2(       s_key))
        define_key(keymap_ime,   "S-" + s_key, self_insert_command2("S-" + s_key))

    ## �A���t�@�x�b�g�L�[�̐ݒ�
    for vkey in range(VK_A, VK_Z + 1):
        s_vkey = "(" + str(vkey) + ")"
        define_key(keymap_emacs,        s_vkey, reset_undo(reset_counter(reset_mark(repeat(self_insert_command2(       s_vkey))))))
        define_key(keymap_emacs, "S-" + s_vkey, reset_undo(reset_counter(reset_mark(repeat(self_insert_command2("S-" + s_vkey))))))
        define_key(keymap_ime,          s_vkey, self_insert_command2(       s_vkey))
        define_key(keymap_ime,   "S-" + s_vkey, self_insert_command2("S-" + s_vkey))

    ## ���ꕶ���L�[�̐ݒ�
    s_vkey = "(" + str(VK_SPACE) + ")"
    define_key(keymap_emacs,        s_vkey, reset_undo(reset_counter(reset_mark(repeat(self_insert_command(       s_vkey))))))
    define_key(keymap_emacs, "S-" + s_vkey, reset_undo(reset_counter(reset_mark(repeat(self_insert_command("S-" + s_vkey))))))

    for vkey in [VK_OEM_MINUS, VK_OEM_PLUS, VK_OEM_COMMA, VK_OEM_PERIOD, VK_OEM_1, VK_OEM_2, VK_OEM_3, VK_OEM_4, VK_OEM_5, VK_OEM_6, VK_OEM_7, VK_OEM_102]:
        s_vkey = "(" + str(vkey) + ")"
        define_key(keymap_emacs,        s_vkey, reset_undo(reset_counter(reset_mark(repeat(self_insert_command2(       s_vkey))))))
        define_key(keymap_emacs, "S-" + s_vkey, reset_undo(reset_counter(reset_mark(repeat(self_insert_command2("S-" + s_vkey))))))
        define_key(keymap_ime,          s_vkey, self_insert_command2(       s_vkey))
        define_key(keymap_ime,   "S-" + s_vkey, self_insert_command2("S-" + s_vkey))

    ## 10key �̓��ꕶ���L�[�̐ݒ�
    for vkey in [VK_MULTIPLY, VK_ADD, VK_SUBTRACT, VK_DECIMAL, VK_DIVIDE]:
        s_vkey = "(" + str(vkey) + ")"
        define_key(keymap_emacs, s_vkey, reset_undo(reset_counter(reset_mark(repeat(self_insert_command2(s_vkey))))))
        define_key(keymap_ime,   s_vkey, self_insert_command2(s_vkey))

    ## quoted-insert�L�[�̐ݒ�
    for vkey in vkeys():
        s_vkey = "(" + str(vkey) + ")"
        define_key(keymap_emacs, "C-q "     + s_vkey, reset_search(reset_undo(reset_counter(reset_mark(self_insert_command(         s_vkey))))))
        define_key(keymap_emacs, "C-q S-"   + s_vkey, reset_search(reset_undo(reset_counter(reset_mark(self_insert_command("S-"   + s_vkey))))))
        define_key(keymap_emacs, "C-q C-"   + s_vkey, reset_search(reset_undo(reset_counter(reset_mark(self_insert_command("C-"   + s_vkey))))))
        define_key(keymap_emacs, "C-q C-S-" + s_vkey, reset_search(reset_undo(reset_counter(reset_mark(self_insert_command("C-S-" + s_vkey))))))
        define_key(keymap_emacs, "C-q A-"   + s_vkey, reset_search(reset_undo(reset_counter(reset_mark(self_insert_command("A-"   + s_vkey))))))
        define_key(keymap_emacs, "C-q A-S-" + s_vkey, reset_search(reset_undo(reset_counter(reset_mark(self_insert_command("A-S-" + s_vkey))))))

    ## C-S-[a-z] -> C-[a-z]�AA-S-[a-z] -> A-[a-z] �̒u�������ݒ�iemacs�V�t�g���[�h�̐ݒ�j
    if use_emacs_shift_mode:
        for vkey in range(VK_A, VK_Z + 1):
            s_vkey = "(" + str(vkey) + ")"
            define_key(keymap_emacs, "C-S-" + s_vkey, reset_search(reset_undo(reset_counter(reset_mark(self_insert_command("C-" + s_vkey))))))
            define_key(keymap_emacs, "A-S-" + s_vkey, reset_search(reset_undo(reset_counter(reset_mark(self_insert_command("A-" + s_vkey))))))
            define_key(keymap_ime,   "C-S-" + s_vkey, self_insert_command("C-" + s_vkey))
            define_key(keymap_ime,   "A-S-" + s_vkey, self_insert_command("A-" + s_vkey))

    ## Esc�L�[�̐ݒ�
    define_key(keymap_emacs, "C-OpenBracket C-OpenBracket", reset_undo(reset_counter(self_insert_command("Esc"))))
    if use_esc_as_meta:
        define_key(keymap_emacs, "Esc Esc", reset_undo(reset_counter(self_insert_command("Esc"))))
    else:
        define_key(keymap_emacs, "Esc", reset_undo(reset_counter(self_insert_command("Esc"))))

    ## universal-argument�L�[�̐ݒ�
    define_key(keymap_emacs, "C-u", universal_argument)

    ## �uIME �̐؂�ւ��v�̃L�[�ݒ�
    define_key(keymap_emacs, "(243)",  toggle_input_method)
    define_key(keymap_emacs, "(244)",  toggle_input_method)
    define_key(keymap_emacs, "A-(25)", toggle_input_method)

    define_key(keymap_ime,   "(243)",  toggle_input_method)
    define_key(keymap_ime,   "(244)",  toggle_input_method)
    define_key(keymap_ime,   "A-(25)", toggle_input_method)

    ## �u�t�@�C������v�̃L�[�ݒ�
    define_key(keymap_emacs, "Ctl-x C-f", reset_search(reset_undo(reset_counter(reset_mark(find_file)))))
    define_key(keymap_emacs, "Ctl-x C-s", reset_search(reset_undo(reset_counter(reset_mark(save_buffer)))))
    define_key(keymap_emacs, "Ctl-x C-w", reset_search(reset_undo(reset_counter(reset_mark(write_file)))))
    define_key(keymap_emacs, "Ctl-x d",   reset_search(reset_undo(reset_counter(reset_mark(dired)))))

    ## �u�J�[�\���ړ��v�̃L�[�ݒ�
    define_key(keymap_emacs, "C-b",        reset_search(reset_undo(reset_counter(mark(repeat(backward_char))))))
    define_key(keymap_emacs, "C-f",        reset_search(reset_undo(reset_counter(mark(repeat(forward_char))))))
    define_key(keymap_emacs, "M-b",        reset_search(reset_undo(reset_counter(mark(repeat(backward_word))))))
    define_key(keymap_emacs, "M-f",        reset_search(reset_undo(reset_counter(mark(repeat(forward_word))))))
    define_key(keymap_emacs, "C-p",        reset_search(reset_undo(reset_counter(mark(repeat(previous_line))))))
    define_key(keymap_emacs, "C-n",        reset_search(reset_undo(reset_counter(mark(repeat(next_line))))))
    define_key(keymap_emacs, "C-a",        reset_search(reset_undo(reset_counter(mark(move_beginning_of_line)))))
    define_key(keymap_emacs, "C-e",        reset_search(reset_undo(reset_counter(mark(move_end_of_line)))))
    define_key(keymap_emacs, "M-S-Comma",  reset_search(reset_undo(reset_counter(mark(beginning_of_buffer)))))
    define_key(keymap_emacs, "M-S-Period", reset_search(reset_undo(reset_counter(mark(end_of_buffer)))))
    define_key(keymap_emacs, "C-l",        reset_search(reset_undo(reset_counter(recenter))))

    define_key(keymap_emacs, "Left",     reset_search(reset_undo(reset_counter(mark(repeat(backward_char))))))
    define_key(keymap_emacs, "Right",    reset_search(reset_undo(reset_counter(mark(repeat(forward_char))))))
    define_key(keymap_emacs, "Up",       reset_search(reset_undo(reset_counter(mark(repeat(previous_line))))))
    define_key(keymap_emacs, "Down",     reset_search(reset_undo(reset_counter(mark(repeat(next_line))))))
    define_key(keymap_emacs, "PageUP",   reset_search(reset_undo(reset_counter(mark(scroll_up)))))
    define_key(keymap_emacs, "PageDown", reset_search(reset_undo(reset_counter(mark(scroll_down)))))
    define_key(keymap_emacs, "Home",     reset_search(reset_undo(reset_counter(mark(move_beginning_of_line)))))
    define_key(keymap_emacs, "End",      reset_search(reset_undo(reset_counter(mark(move_end_of_line)))))

    ## �u�J�b�g / �R�s�[ / �폜 / �A���h�D�v�̃L�[�ݒ�
    define_key(keymap_emacs, "Back",     reset_search(reset_undo(reset_counter(reset_mark(repeat2(delete_backward_char))))))
    define_key(keymap_emacs, "C-h",      reset_search(reset_undo(reset_counter(reset_mark(repeat2(delete_backward_char))))))
    define_key(keymap_emacs, "Delete",   reset_search(reset_undo(reset_counter(reset_mark(repeat2(delete_char))))))
    define_key(keymap_emacs, "C-d",      reset_search(reset_undo(reset_counter(reset_mark(repeat2(delete_char))))))
    define_key(keymap_emacs, "C-Back",   reset_search(reset_undo(reset_counter(reset_mark(repeat3(backward_kill_word))))))
    define_key(keymap_emacs, "M-Delete", reset_search(reset_undo(reset_counter(reset_mark(repeat3(backward_kill_word))))))
    define_key(keymap_emacs, "C-Delete", reset_search(reset_undo(reset_counter(reset_mark(repeat3(kill_word))))))
    define_key(keymap_emacs, "M-d",      reset_search(reset_undo(reset_counter(reset_mark(repeat3(kill_word))))))
    define_key(keymap_emacs, "C-k",      reset_search(reset_undo(reset_counter(reset_mark(repeat3(kill_line))))))
    define_key(keymap_emacs, "C-w",      reset_search(reset_undo(reset_counter(reset_mark(kill_region)))))
    define_key(keymap_emacs, "M-w",      reset_search(reset_undo(reset_counter(reset_mark(kill_ring_save)))))
    define_key(keymap_emacs, "C-c",      reset_search(reset_undo(reset_counter(reset_mark(kill_ring_save)))))
    define_key(keymap_emacs, "C-y",      reset_search(reset_undo(reset_counter(reset_mark(repeat(yank))))))
    define_key(keymap_emacs, "C-v",      reset_search(reset_undo(reset_counter(reset_mark(repeat(yank)))))) # scroll_key �̐ݒ�ŏ㏑������Ȃ��ꍇ
    define_key(keymap_emacs, "C-z",      reset_search(reset_counter(reset_mark(undo))))
    define_key(keymap_emacs, "C-BackSlash", reset_search(reset_counter(reset_mark(redo))))
    define_key(keymap_emacs, "C-Slash",  reset_search(reset_counter(reset_mark(undo))))
    define_key(keymap_emacs, "Ctl-x u",  reset_search(reset_counter(reset_mark(undo))))

    # C-Underscore ���@�\�����邽�߂̐ݒ�
    if is_japanese_keyboard:
        define_key(keymap_emacs, "C-S-BackSlash", reset_search(reset_undo(reset_counter(reset_mark(undo)))))
    else:
        define_key(keymap_emacs, "C-S-Minus", reset_search(reset_undo(reset_counter(reset_mark(undo)))))

    if is_japanese_keyboard:
        # C-Atmark ���Ƃ��܂������Ȃ���������悤�Ȃ̂� C-(192) �Ƃ��Ă���
        # �ihttp://bhby39.blogspot.jp/2015/02/windows-emacs.html�j
        define_key(keymap_emacs, "C-(192)", reset_search(reset_undo(reset_counter(set_mark_command))))
    else:
        # C-S-2 �͗L���ƂȂ�Ȃ����A�ꉞ�ݒ�͍s���Ă����iC-S-3 �Ȃǂ͗L���ƂȂ�B�Ȃ����낤�H�j
        define_key(keymap_emacs, "C-S-2", reset_search(reset_undo(reset_counter(set_mark_command))))

    define_key(keymap_emacs, "C-Space",   reset_search(reset_undo(reset_counter(set_mark_command))))
    define_key(keymap_emacs, "Ctl-x h",   reset_search(reset_undo(reset_counter(reset_mark(mark_whole_buffer)))))
    define_key(keymap_emacs, "Ctl-x C-p", reset_search(reset_undo(reset_counter(reset_mark(mark_page)))))

    ## �u�o�b�t�@ / �E�C���h�E����v�̃L�[�ݒ�
    define_key(keymap_emacs, "Ctl-x k", reset_search(reset_undo(reset_counter(reset_mark(kill_buffer)))))
    define_key(keymap_emacs, "Ctl-x b", reset_search(reset_undo(reset_counter(reset_mark(switch_to_buffer)))))
    define_key(keymap_emacs, "Ctl-x o", reset_search(reset_undo(reset_counter(reset_mark(other_window)))))

    ## �u�����񌟍� / �u���v�̃L�[�ݒ�
    define_key(keymap_emacs, "C-r",   reset_undo(reset_counter(reset_mark(isearch_backward))))
    define_key(keymap_emacs, "C-s",   reset_undo(reset_counter(reset_mark(isearch_forward))))
    define_key(keymap_emacs, "M-S-5", reset_search(reset_undo(reset_counter(reset_mark(query_replace)))))

    ## �u�L�[�{�[�h�}�N���v�̃L�[�ݒ�
    if is_japanese_keyboard:
        define_key(keymap_emacs, "Ctl-x S-8", kmacro_start_macro)
        define_key(keymap_emacs, "Ctl-x S-9", kmacro_end_macro)
    else:
        define_key(keymap_emacs, "Ctl-x S-9", kmacro_start_macro)
        define_key(keymap_emacs, "Ctl-x S-0", kmacro_end_macro)

    define_key(keymap_emacs, "Ctl-x e", reset_search(reset_undo(reset_counter(repeat(kmacro_end_and_call_macro)))))

    ## �u���̑��v�̃L�[�ݒ�
    define_key(keymap_emacs, "Enter",     reset_undo(reset_counter(reset_mark(repeat(newline)))))
    define_key(keymap_emacs, "C-m",       reset_undo(reset_counter(reset_mark(repeat(newline)))))
    define_key(keymap_emacs, "C-j",       reset_undo(reset_counter(reset_mark(newline_and_indent))))
    define_key(keymap_emacs, "Tab",       reset_undo(reset_counter(reset_mark(repeat(indent_for_tab_command)))))
    define_key(keymap_emacs, "C-g",       reset_search(reset_counter(reset_mark(keyboard_quit))))
    #define_key(keymap_emacs, "Ctl-x C-c", reset_search(reset_undo(reset_counter(reset_mark(kill_emacs)))))
    define_key(keymap_emacs, "M-S-1",     reset_search(reset_undo(reset_counter(reset_mark(shell_command)))))

    if use_ctrl_i_as_tab:
        define_key(keymap_emacs, "C-i", reset_undo(reset_counter(reset_mark(repeat(indent_for_tab_command)))))

    ## �uIME �̐؂�ւ��v�̃L�[�ݒ�i�㏑������Ȃ��悤�ɍŌ�ɐݒ肷��j
    if toggle_input_method_key:
        for key in toggle_input_method_key:
            define_key(keymap_emacs, key, toggle_input_method)
            define_key(keymap_ime,   key, toggle_input_method)

    ## �u�X�N���[���v�̃L�[�ݒ�i�㏑������Ȃ��悤�ɍŌ�ɐݒ肷��j
    if scroll_key:
        define_key(keymap_emacs, scroll_key[0], reset_search(reset_undo(reset_counter(mark(scroll_up)))))
        define_key(keymap_emacs, scroll_key[1], reset_search(reset_undo(reset_counter(mark(scroll_down)))))

    ## �u�J�b�g�v�̃L�[�ݒ�i�㏑������Ȃ��悤�ɍŌ�ɐݒ肷��j
    if ctl_x_prefix_key != "C-x":
        define_key(keymap_emacs, "C-x", reset_search(reset_undo(reset_counter(reset_mark(kill_region)))))


    ####################################################################################################
    ## emacs���{����̓��[�h�̐ݒ�
    ####################################################################################################
    if use_emacs_ime_mode:

        def is_emacs_ime_mode(window):
            fakeymacs.ei_ime_status = window.getImeStatus()

            if fakeymacs.ei_last_window:
                if fakeymacs.ei_last_window == window:
                    return True
                else:
                    disable_emacs_ime_mode(False)
                    return False
            else:
                return False

        keymap_ei = keymap.defineWindowKeymap(check_func=is_emacs_ime_mode)

        # IME �̏�Ԃ��i�[����
        fakeymacs.ei_ime_status = False

        # emacs���{����̓��[�h���J�n���ꂽ�Ƃ��̃E�B���h�E�I�u�W�F�N�g���i�[����ϐ�������������
        fakeymacs.ei_last_window = None

        ##################################################
        ## emacs���{����̓��[�h �̐؂�ւ�
        ##################################################

        def enable_emacs_ime_mode(update=True, toggle=False):
            fakeymacs.ei_last_window = keymap.getWindow()
            fakeymacs.ei_last_func = None
            ei_popBalloon(toggle)
            if update:
                keymap.updateKeymap()

        def disable_emacs_ime_mode(update=True, toggle=False):
            fakeymacs.ei_last_window = None
            ei_popBalloon(toggle)
            if update:
                keymap.updateKeymap()

        def toggle_emacs_ime_mode():
            if fakeymacs.ei_last_window:
                disable_emacs_ime_mode(True, True)
            else:
                enable_emacs_ime_mode(True, True)

        ##################################################
        ## IME �̐؂�ւ��iemacs���{����̓��[�h�p�j
        ##################################################

        def ei_toggle_input_method():
            disable_emacs_ime_mode()
            toggle_input_method()

        def ei_toggle_input_method2(key):
            def _func():
                if fakeymacs.ei_last_func == delete_backward_char:
                    ei_toggle_input_method()
                else:
                    ei_record_func(self_insert_command(key)())
            return _func

        ##################################################
        ## ���̑��iemacs���{����̓��[�h�p�j
        ##################################################

        def ei_esc():
            self_insert_command("Esc")()

        def ei_newline():
            self_insert_command("Enter")()
            disable_emacs_ime_mode()

        def ei_keyboard_quit():
            self_insert_command("Esc")()
            disable_emacs_ime_mode()

        ##################################################
        ## ���ʊ֐��iemacs���{����̓��[�h�p�j
        ##################################################

        def ei_record_func(func):
            def _func():
                func()
                fakeymacs.ei_last_func = func
            return _func

        def ei_popBalloon(toggle):
            if not fakeymacs.is_playing_kmacro:
                if emacs_ime_mode_balloon_message:
                    if fakeymacs.ei_last_window:
                        keymap.popBalloon("emacs_ime_mode", emacs_ime_mode_balloon_message)
                    else:
                        keymap.closeBalloon("emacs_ime_mode")
                else:
                    if toggle:
                        if fakeymacs.ei_last_window:
                            message = "[IME]"
                        else:
                            message = "[main]"
                        keymap.popBalloon("emacs_ime_mode", message, 500)

        def ei_hook_mouseup(x, y, vk):
            if fakeymacs.ei_last_window:
                disable_emacs_ime_mode()
            else:
                keymap.updateKeymap()

            fakeymacs.ei_hook_mouseup(x, y, vk)

        # �}�E�X�̃{�^���������ꂽ�Ƃ��ɌĂяo�����t�b�N�֐���ݒ肷��
        keymap.enableHook(True) # �ݒ�̃����[�h���Ƀt�b�N�̐ݒ肪�l�X�g���Ȃ��悤�ɃR�[��
        fakeymacs.ei_hook_mouseup = keyhac_hook.hook.mouseup
        keyhac_hook.hook.mouseup = ei_hook_mouseup

        ##################################################
        ## �L�[�o�C���h�iemacs���{����̓��[�h�p�j
        ##################################################

        ## �S�ăL�[�p�^�[���̐ݒ�iei_record_func �֐���ʂ����߂̐ݒ�j
        for vkey in vkeys():
            s_vkey = "(" + str(vkey) + ")"
            define_key(keymap_ei,          s_vkey, ei_record_func(self_insert_command(         s_vkey)))
            define_key(keymap_ei, "S-"   + s_vkey, ei_record_func(self_insert_command("S-"   + s_vkey)))
            define_key(keymap_ei, "C-"   + s_vkey, ei_record_func(self_insert_command("C-"   + s_vkey)))
            define_key(keymap_ei, "C-S-" + s_vkey, ei_record_func(self_insert_command("C-S-" + s_vkey)))
            define_key(keymap_ei, "A-"   + s_vkey, ei_record_func(self_insert_command("A-"   + s_vkey)))
            define_key(keymap_ei, "A-S-" + s_vkey, ei_record_func(self_insert_command("A-S-" + s_vkey)))

        ## C-S-[a-z] -> C-[a-z]�AA-S-[a-z] -> A-[a-z] �̒u�������ݒ�iemacs�V�t�g���[�h�̐ݒ�j
        if use_emacs_shift_mode:
            for vkey in range(VK_A, VK_Z + 1):
                s_vkey = "(" + str(vkey) + ")"
                define_key(keymap_ei, "C-S-" + s_vkey, ei_record_func(self_insert_command("C-" + s_vkey)))
                define_key(keymap_ei, "A-S-" + s_vkey, ei_record_func(self_insert_command("A-" + s_vkey)))

        ## �uIME �̐؂�ւ��v�̃L�[�ݒ�
        define_key(keymap_ei, "(243)",  ei_toggle_input_method)
        define_key(keymap_ei, "(244)",  ei_toggle_input_method)
        define_key(keymap_ei, "A-(25)", ei_toggle_input_method)

        ## Esc�L�[�̐ݒ�
        define_key(keymap_ei, "Esc",           ei_record_func(ei_esc))
        define_key(keymap_ei, "C-OpenBracket", ei_record_func(ei_esc))

        ## �u�J�[�\���ړ��v�̃L�[�ݒ�
        define_key(keymap_ei, "C-b", ei_record_func(backward_char))
        define_key(keymap_ei, "C-f", ei_record_func(forward_char))
        define_key(keymap_ei, "C-p", ei_record_func(previous_line))
        define_key(keymap_ei, "C-n", ei_record_func(next_line))
        define_key(keymap_ei, "C-a", ei_record_func(move_beginning_of_line))
        define_key(keymap_ei, "C-e", ei_record_func(move_end_of_line))

        define_key(keymap_ei, "Left",     ei_record_func(backward_char))
        define_key(keymap_ei, "Right",    ei_record_func(forward_char))
        define_key(keymap_ei, "Up",       ei_record_func(previous_line))
        define_key(keymap_ei, "Down",     ei_record_func(next_line))
        define_key(keymap_ei, "PageUP",   ei_record_func(scroll_up))
        define_key(keymap_ei, "PageDown", ei_record_func(scroll_down))
        define_key(keymap_ei, "Home",     ei_record_func(move_beginning_of_line))
        define_key(keymap_ei, "End",      ei_record_func(move_end_of_line))

        ## �u�J�b�g / �R�s�[ / �폜 / �A���h�D�v�̃L�[�ݒ�
        define_key(keymap_ei, "Back",   ei_record_func(delete_backward_char))
        define_key(keymap_ei, "C-h",    ei_record_func(delete_backward_char))
        define_key(keymap_ei, "Delete", ei_record_func(delete_char))
        define_key(keymap_ei, "C-d",    ei_record_func(delete_char))

        ## �u���̑��v�̃L�[�ݒ�
        define_key(keymap_ei, "Enter", ei_newline)
        define_key(keymap_ei, "C-m",   ei_newline)
        define_key(keymap_ei, "Tab",   ei_record_func(indent_for_tab_command))
        define_key(keymap_ei, "C-g",   ei_keyboard_quit)

        ## �uIME �̐؂�ւ��v�̃L�[�ݒ�i�㏑������Ȃ��悤�ɍŌ�ɐݒ肷��j
        if toggle_input_method_key:
            for key in toggle_input_method_key:
                define_key(keymap_ei, key, ei_toggle_input_method2(key))

        ## �u�X�N���[���v�̃L�[�ݒ�i�㏑������Ȃ��悤�ɍŌ�ɐݒ肷��j
        if scroll_key:
            define_key(keymap_ei, scroll_key[0].replace("M-", "A-"), ei_record_func(scroll_up))
            define_key(keymap_ei, scroll_key[1].replace("M-", "A-"), ei_record_func(scroll_down))

        ## emacs���{����̓��[�h��؂�ւ���i�g�O������j
        define_key(keymap_emacs, toggle_emacs_ime_mode_key, toggle_emacs_ime_mode)
        define_key(keymap_ime,   toggle_emacs_ime_mode_key, toggle_emacs_ime_mode)
        define_key(keymap_ei,    toggle_emacs_ime_mode_key, toggle_emacs_ime_mode)


    ####################################################################################################
    ## �f�X�N�g�b�v�̐ݒ�
    ####################################################################################################

    keymap_global = keymap.defineWindowKeymap()

    ##################################################
    ## �E�C���h�E����i�f�X�N�g�b�v�p�j
    ##################################################

    def popWindow(wnd):
        def _func():
            try:
                if wnd.isMinimized():
                    wnd.restore()
                wnd.getLastActivePopup().setForeground()
            except:
                print("�I�������E�B���h�E�͑��݂��܂���ł���")
        return _func

    def getWindowList():
        def makeWindowList(wnd, arg):
            if wnd.isVisible() and not wnd.getOwner():

                class_name = wnd.getClassName()
                title = wnd.getText()

                if class_name == "Emacs" or title != "":

                    # ����̑ΏۂƂ������Ȃ��A�v���P�[�V�����\�t�g�̃N���X���̂��Are.match �֐�
                    # �i�擪����̃}�b�`�j�̐��K�\���Ɂu|�v���g���Čq���Ďw�肵�Ă��������B
                    # �i���S�}�b�`�Ƃ��邽�߂ɂ� $ �̎w�肪�K�v�ł��B�j
                    if not re.match(r"Progman$", class_name):

                        process_name = wnd.getProcessName()

                        # ����̑ΏۂƂ������Ȃ��A�v���P�[�V�����\�t�g�̃v���Z�X���̂��Are.match �֐�
                        # �i�擪����̃}�b�`�j�̐��K�\���Ɂu|�v���g���Čq���Ďw�肵�Ă��������B
                        # �i���S�}�b�`�Ƃ��邽�߂ɂ� $ �̎w�肪�K�v�ł��B�j
                        if not re.match(r"RocketDock.exe$", process_name): # �T���v���Ƃ��� RocketDock.exe ��o�^

                            # �\������Ă��Ȃ��X�g�A�A�v���i�u�ݒ�v���j�� window_list �ɓo�^�����̂�}������
                            if class_name == "Windows.UI.Core.CoreWindow":
                                if title in window_dict:
                                    window_list.remove(window_dict[title])
                                else:
                                    window_dict[title] = wnd
                            elif class_name == "ApplicationFrameWindow":
                                if title not in window_dict:
                                    window_dict[title] = wnd
                                    window_list.append(wnd)
                            else:
                                window_list.append(wnd)

                            # print(process_name + " : " + class_name + " : " + title + " : " + str(wnd.isMinimized()))
            return True

        window_dict = {}
        window_list = []
        # print("----------------------------------------------------------------------------------------------------")
        Window.enum(makeWindowList, None)

        return window_list

    def restoreWindow():
        wnd = keymap.getTopLevelWindow()
        if wnd and wnd.isMinimized():
            wnd.restore()

    def previous_desktop():
        self_insert_command("W-C-Left")()

    def next_desktop():
        self_insert_command("W-C-Right")()

    def previous_window():
        self_insert_command("A-S-Esc")()
        delay(0.2)
        keymap.delayedCall(restoreWindow, 0)

    def next_window():
        self_insert_command("A-Esc")()
        delay(0.2)
        keymap.delayedCall(restoreWindow, 0)

    def previous_display():
        self_insert_command("W-S-Left")()

    def next_display():
        self_insert_command("W-S-Right")()

    def minimize_window():
        wnd = keymap.getTopLevelWindow()
        if wnd and not wnd.isMinimized():
            wnd.minimize()

    def restore_window():
        window_list = getWindowList()
        if not (sys.getwindowsversion().major == 6 and
                sys.getwindowsversion().minor == 1):
            window_list.reverse()
        for wnd in window_list:
            if wnd.isMinimized():
                wnd.restore()
                break

    ##################################################
    ## �L�[�o�C���h�i�f�X�N�g�b�v�p�j
    ##################################################

    # �\�����Ă���E�C���h�E�̒��ŁA��ԍŋ߂܂Ńt�H�[�J�X���������E�C���h�E�Ɉړ�
    define_key(keymap_global, other_window_key, reset_search(reset_undo(reset_counter(reset_mark(other_window)))))

    # �A�N�e�B�u�E�B���h�E�̐؂�ւ�
    if window_switching_key:
        for previous_key, next_key in window_switching_key:
            define_key(keymap_global, previous_key, reset_search(reset_undo(reset_counter(reset_mark(previous_window)))))
            define_key(keymap_global, next_key,     reset_search(reset_undo(reset_counter(reset_mark(next_window)))))

    # �A�N�e�B�u�E�B���h�E�̃f�B�X�v���C�Ԉړ�
    if window_movement_key:
        for previous_key, next_key in window_movement_key:
            define_key(keymap_global, previous_key, reset_search(reset_undo(reset_counter(reset_mark(previous_display)))))
            define_key(keymap_global, next_key,     reset_search(reset_undo(reset_counter(reset_mark(next_display)))))

    # �E�B���h�E�̍ŏ����A���X�g�A
    if window_minimize_key:
        for restore_key, minimize_key in window_minimize_key:
            define_key(keymap_global, restore_key,  reset_search(reset_undo(reset_counter(reset_mark(restore_window)))))
            define_key(keymap_global, minimize_key, reset_search(reset_undo(reset_counter(reset_mark(minimize_window)))))

    # ���z�f�X�N�g�b�v�̐؂�ւ�
    if desktop_switching_key:
        for previous_key, next_key in desktop_switching_key:
            define_key(keymap_global, previous_key, reset_search(reset_undo(reset_counter(reset_mark(previous_desktop)))))
            define_key(keymap_global, next_key,     reset_search(reset_undo(reset_counter(reset_mark(next_desktop)))))


    ####################################################################################################
    ## ���X�g�E�B���h�E�̐ݒ�
    ####################################################################################################

    # ���X�g�E�B���h�E�̓N���b�v�{�[�h���X�g�ŗ��p���Ă��܂����A�N���b�v�{�[�h���X�g�̋@�\��
    # emacs�L�[�o�C���h��K�p���Ă��Ȃ��A�v���P�[�V�����\�t�g�ł����p�ł���悤�ɂ��邽�߁A
    # �N���b�v�{�[�h���X�g�� Enter �����������ۂ̋������A�ȉ��̂Ƃ���ɐ؂蕪���Ă��܂��B
    #
    # �P�jemacs�L�[�o�C���h��K�p���Ă���A�v���P�[�V�����\�t�g����N���b�v�{�[�h���X�g���N��
    # �@�@��   Enter�i�e�L�X�g�̓\��t���j
    # �Q�jemacs�L�[�o�C���h��K�p���Ă��Ȃ��A�v���P�[�V�����\�t�g����N���b�v�{�[�h���X�g���N��
    # �@�@�� S-Enter�i�e�L�X�g���N���b�v�{�[�h�Ɋi�[�j
    #
    # �iemacs�L�[�o�C���h��K�p���Ȃ��A�v���P�[�V�����\�t�g�ɂ́A�L�[�̓��o�͂̕����������
    # �@���̂������w�肳��Ă��邽�߁A�e�L�X�g�̓\��t�������܂��@�\���Ȃ����̂�����܂��B
    # �@���̂��߁A�A�v���P�[�V�����\�t�g�Ƀy�[�X�g����ꍇ�́A���̃A�v���P�[�V�����\�t�g��
    # �@�y�[�X�g����ōs�����Ƃ�O��Ƃ��A��L�̂Ƃ���̎d�l�Ƃ��Ă܂��B�����A�ǂ����Ă�
    # �@Enter�i�e�L�X�g�̓\��t���j�̓��͂��s�������ꍇ�ɂ́AC-m �̉����ɂ��Ή��ł��܂��B
    # �@�Ȃ��AC-Enter�i���p�L���t�œ\��t���j�̒u�������́A�Ή������G�ƂȂ邽�ߍs���Ă���܂���B�j

    keymap.setFont("�l�r �S�V�b�N", 12)

    def is_list_window(window):
        if window.getClassName() == "KeyhacWindowClass" and window.getText() != "Keyhac":
            return True
        return False

    keymap_lw = keymap.defineWindowKeymap(check_func=is_list_window)

    # ���X�g�E�B���h�E�Ō������J�n������ True �ɂȂ�
    fakeymacs.lw_is_searching = False

    ##################################################
    ## �����񌟍� / �u���i���X�g�E�B���h�E�p�j
    ##################################################

    def lw_isearch(direction):
        if fakeymacs.lw_is_searching:
            self_insert_command({"backward":"Up", "forward":"Down"}[direction])()
        else:
            self_insert_command("f")()
            fakeymacs.lw_is_searching = True

    def lw_isearch_backward():
        lw_isearch("backward")

    def lw_isearch_forward():
        lw_isearch("forward")

    ##################################################
    ## ���̑��i���X�g�E�B���h�E�p�j
    ##################################################

    def lw_keyboard_quit():
        self_insert_command("Esc")()

    ##################################################
    ## ���ʊ֐��i���X�g�E�B���h�E�p�j
    ##################################################

    def lw_newline():
        if fakeymacs.keybind == "emacs":
            self_insert_command("Enter")()
        else:
            self_insert_command("S-Enter")()

    def lw_exit_search(func):
        def _func():
            if fakeymacs.lw_is_searching:
                self_insert_command("Enter")()
            func()
        return _func

    def lw_reset_search(func):
        def _func():
            func()
            fakeymacs.lw_is_searching = False
        return _func

    ##################################################
    ## �L�[�o�C���h�i���X�g�E�B���h�E�p�j
    ##################################################

    ## Esc�L�[�̐ݒ�
    define_key(keymap_lw, "Esc",           lw_reset_search(self_insert_command("Esc")))
    define_key(keymap_lw, "C-OpenBracket", lw_reset_search(self_insert_command("Esc")))

    ## �u�J�[�\���ړ��v�̃L�[�ݒ�
    define_key(keymap_lw, "C-b", backward_char)
    define_key(keymap_lw, "A-b", backward_char)

    define_key(keymap_lw, "C-f", forward_char)
    define_key(keymap_lw, "A-f", forward_char)

    define_key(keymap_lw, "C-p", previous_line)
    define_key(keymap_lw, "A-p", previous_line)

    define_key(keymap_lw, "C-n", next_line)
    define_key(keymap_lw, "A-n", next_line)

    if scroll_key:
        define_key(keymap_lw, scroll_key[0].replace("M-", "A-"), scroll_up)
        define_key(keymap_lw, scroll_key[1].replace("M-", "A-"), scroll_down)

    ## �u�J�b�g / �R�s�[ / �폜 / �A���h�D�v�̃L�[�ݒ�
    define_key(keymap_lw, "C-h", delete_backward_char)
    define_key(keymap_lw, "A-h", delete_backward_char)

    define_key(keymap_lw, "C-d", delete_char)
    define_key(keymap_lw, "A-d", delete_char)

    ## �u�����񌟍� / �u���v�̃L�[�ݒ�
    define_key(keymap_lw, "C-r", lw_isearch_backward)
    define_key(keymap_lw, "A-r", lw_isearch_backward)

    define_key(keymap_lw, "C-s", lw_isearch_forward)
    define_key(keymap_lw, "A-s", lw_isearch_forward)

    ## �u���̑��v�̃L�[�ݒ�
    define_key(keymap_lw, "Enter", lw_exit_search(lw_newline))
    define_key(keymap_lw, "C-m",   lw_exit_search(lw_newline))
    define_key(keymap_lw, "A-m",   lw_exit_search(lw_newline))

    define_key(keymap_lw, "C-g", lw_reset_search(lw_keyboard_quit))
    define_key(keymap_lw, "A-g", lw_reset_search(lw_keyboard_quit))

    define_key(keymap_lw, "S-Enter", lw_exit_search(self_insert_command("S-Enter")))
    define_key(keymap_lw, "C-Enter", lw_exit_search(self_insert_command("C-Enter")))
    define_key(keymap_lw, "A-Enter", lw_exit_search(self_insert_command("C-Enter")))


    ####################################################################################################
    ## �N���b�v�{�[�h���X�g�̐ݒ�
    ####################################################################################################
    if 1:
        # �N���b�v�{�[�h���X�g�𗘗p���邽�߂̐ݒ�ł��B�N���b�v�{�[�h���X�g�� clipboardList_key �ϐ���
        # �ݒ肵���L�[�̉����ɂ��N�����܂��B�N���b�v�{�[�h���X�g���J������AC-f�i���j�� C-b�i���j
        # �L�[����͂��邱�Ƃŉ�ʂ�؂�ւ��邱�Ƃ��ł��܂��B
        # �i�Q�l�Fhttps://github.com/crftwr/keyhac/blob/master/_config.py�j

        # ���X�g�E�B���h�E�̃t�H�[�}�b�^���`����
        list_formatter = "{:30}"

        # ��^��
        fixed_items = [
            ["---------+ x 8", "---------+" * 8],
            ["���[���A�h���X", "user_name@domain_name"],
            ["�Z��",           "��999-9999 �m�m�m�m�m�m�m�m�m�m"],
            ["�d�b�ԍ�",       "99-999-9999"],
        ]
        fixed_items[0][0] = list_formatter.format(fixed_items[0][0])

        import datetime

        # �������y�[�X�g����@�\
        def dateAndTime(fmt):
            def _func():
                return datetime.datetime.now().strftime(fmt)
            return _func

        # ����
        datetime_items = [
            ["YYYY/MM/DD HH:MM:SS", dateAndTime("%Y/%m/%d %H:%M:%S")],
            ["YYYY/MM/DD",          dateAndTime("%Y/%m/%d")],
            ["HH:MM:SS",            dateAndTime("%H:%M:%S")],
            ["YYYYMMDD_HHMMSS",     dateAndTime("%Y%m%d_%H%M%S")],
            ["YYYYMMDD",            dateAndTime("%Y%m%d")],
            ["HHMMSS",              dateAndTime("%H%M%S")],
        ]
        datetime_items[0][0] = list_formatter.format(datetime_items[0][0])

        keymap.cblisters += [
            ["��^��",  cblister_FixedPhrase(fixed_items)],
            ["����",    cblister_FixedPhrase(datetime_items)],
        ]

        def lw_clipboardList():
            keymap.command_ClipboardList()

        # �N���b�v�{�[�h���X�g���N������
        define_key(keymap_global, clipboardList_key, lw_reset_search(reset_search(reset_undo(reset_counter(reset_mark(lw_clipboardList))))))


    ####################################################################################################
    ## �����`���[���X�g�̐ݒ�
    ####################################################################################################
    if 1:
        # �����`���[�p�̃��X�g�𗘗p���邽�߂̐ݒ�ł��B�����`���[���X�g�� lancherList_key �ϐ���
        # �ݒ肵���L�[�̉����ɂ��N�����܂��B�����`���[���X�g���J������AC-f�i���j�� C-b�i���j
        # �L�[����͂��邱�Ƃŉ�ʂ�؂�ւ��邱�Ƃ��ł��܂��B
        # �i�Q�l�Fhttps://github.com/crftwr/keyhac/blob/master/_config.py�j

        def lw_lancherList():
            def popLancherList():

                # ���X�g�E�B���h�E�̃t�H�[�}�b�^���`����
                list_formatter = "{:30}"

                # ���Ƀ��X�g���J���Ă�������邾��
                if keymap.isListWindowOpened():
                    keymap.cancelListWindow()
                    return

                # �E�B���h�E
                window_list = getWindowList()
                window_items = []
                if window_list:
                    processName_length = max(map(len, map(Window.getProcessName, window_list)))

                    formatter = "{0:" + str(processName_length) + "} | {1}"
                    for wnd in window_list:
                        window_items.append((formatter.format(wnd.getProcessName(), wnd.getText()), popWindow(wnd)))

                window_items.append((list_formatter.format("<Desktop>"), keymap.ShellExecuteCommand(None, r"shell:::{3080F90D-D7AD-11D9-BD98-0000947B0257}", "", "")))

                # �A�v���P�[�V�����\�t�g
                application_items = [
                    ["notepad",     keymap.ShellExecuteCommand(None, r"notepad.exe", "", "")],
                    ["sakura",      keymap.ShellExecuteCommand(None, r"C:\Program Files (x86)\sakura\sakura.exe", "", "")],
                    ["explorer",    keymap.ShellExecuteCommand(None, r"explorer.exe", "", "")],
                    ["cmd",         keymap.ShellExecuteCommand(None, r"cmd.exe", "", "")],
                    ["chrome",      keymap.ShellExecuteCommand(None, r"C:\Program Files (x86)\Google\Chrome\Application\chrome.exe", "", "")],
                    ["firefox",     keymap.ShellExecuteCommand(None, r"C:\Program Files (x86)\Mozilla Firefox\firefox.exe", "", "")],
                    ["thunderbird", keymap.ShellExecuteCommand(None, r"C:\Program Files (x86)\Mozilla Thunderbird\thunderbird.exe", "", "")],
                ]
                application_items[0][0] = list_formatter.format(application_items[0][0])

                # �E�F�u�T�C�g
                website_items = [
                    ["Google",          keymap.ShellExecuteCommand(None, r"https://www.google.co.jp/", "", "")],
                    ["Facebook",        keymap.ShellExecuteCommand(None, r"https://www.facebook.com/", "", "")],
                    ["Twitter",         keymap.ShellExecuteCommand(None, r"https://twitter.com/", "", "")],
                    ["Keyhac",          keymap.ShellExecuteCommand(None, r"https://sites.google.com/site/craftware/keyhac-ja", "", "")],
                    ["NTEmacs���E�B�L", keymap.ShellExecuteCommand(None, r"http://www49.atwiki.jp/ntemacs/", "", "")],
                ]
                website_items[0][0] = list_formatter.format(website_items[0][0])

                # ���̑�
                other_items = [
                    ["Edit   config.py", keymap.command_EditConfig],
                    ["Reload config.py", keymap.command_ReloadConfig],
                ]
                other_items[0][0] = list_formatter.format(other_items[0][0])

                listers = [
                    ["Window",  cblister_FixedPhrase(window_items)],
                    ["App",     cblister_FixedPhrase(application_items)],
                    ["Website", cblister_FixedPhrase(website_items)],
                    ["Other",   cblister_FixedPhrase(other_items)],
                ]

                try:
                    select_item = keymap.popListWindow(listers)

                    if not select_item:
                        Window.find("Progman", None).setForeground()
                        select_item = keymap.popListWindow(listers)

                    if select_item and select_item[0] and select_item[0][1]:
                        select_item[0][1]()
                except:
                    print("�G���[���������܂���")

            # �L�[�t�b�N�̒��Ŏ��Ԃ̂����鏈�������s�ł��Ȃ��̂ŁAdelayedCall() ���g���Ēx�����s����
            keymap.delayedCall(popLancherList, 0)

        # �����`���[���X�g���N������
        define_key(keymap_global, lancherList_key, lw_reset_search(reset_search(reset_undo(reset_counter(reset_mark(lw_lancherList))))))


    ####################################################################################################
    ## Excel �̏ꍇ�AC-Enter �� F2�i�Z���ҏW���[�h�ڍs�j�����蓖�Ă�i�I�v�V�����j
    ####################################################################################################
    if 0:
        keymap_excel = keymap.defineWindowKeymap(class_name="EXCEL*")

        # C-Enter �����ŁA�u�Z���ҏW���[�h�v�Ɉڍs����
        define_key(keymap_excel, "C-Enter", reset_search(reset_undo(reset_counter(reset_mark(self_insert_command("F2"))))))


    ####################################################################################################
    ## Emacs �̏ꍇ�AIME �؂�ւ��p�̃L�[�� C-\ �ɒu��������i�I�v�V�����j
    ####################################################################################################
    if 0:
        # NTEmacs �̗��p���� Windows �� IME �̐؊����𖳌��Ƃ��邽�߂̐ݒ�ł��B�imozc.el �𗘗p����ꍇ�Ȃǁj
        # �ǉ��������L�[������ꍇ�́A���̕��@�Œǉ�����L�[�̖��̂������̓R�[�h���m�F���A
        # �X�N���v�g���C�����Ă��������B
        # �@1) �^�X�N�o�[�ɂ��� Keyhac �̃A�C�R�������N���b�N���ăR���\�[�����J���B
        # �@2) �A�C�R�����E�N���b�N���ă��j���[���J���A�u�������O ON�v��I������B
        # �@3) �m�F�������L�[�������B

        keymap_real_emacs = keymap.defineWindowKeymap(class_name="Emacs")

        # IME �؂�ւ��p�̃L�[�� C-\ �ɒu��������
        keymap_real_emacs["(28)"]   = keymap.InputKeyCommand("C-Yen") # [�ϊ�] �L�[
        keymap_real_emacs["(29)"]   = keymap.InputKeyCommand("C-Yen") # [���ϊ�] �L�[
        keymap_real_emacs["(240)"]  = keymap.InputKeyCommand("C-Yen") # [�p��] �L�[
        keymap_real_emacs["(242)"]  = keymap.InputKeyCommand("C-Yen") # [�J�^�J�i�E�Ђ炪��] �L�[
        keymap_real_emacs["(243)"]  = keymap.InputKeyCommand("C-Yen") # [���p�^�S�p] �L�[
        keymap_real_emacs["(244)"]  = keymap.InputKeyCommand("C-Yen") # [���p�^�S�p] �L�[
        keymap_real_emacs["A-(25)"] = keymap.InputKeyCommand("C-Yen") # Alt-` �L�[

