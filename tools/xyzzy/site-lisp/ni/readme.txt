$Id: readme.txt 753 2007-11-21 11:46:20Z torihat $

 NetInstaller for xyzzy

 by Masashi Hattori

�� NetInstaller for xyzzy �Ƃ́H

	xyzzy �̃��[�U�[�g�����̂ւȂ��傱�C���X�g�[���ł��B
	���p�������Ɋւ��ẮALICENSE.txt�������������B

�� �����

	xyzzy-0.2.2.233+�i�ō�����̂Łj

	�𓀗p�A�[�J�C�oDLL�e��

�� �C���X�g�[��

	�𓀂����� ni �t�H���_��site-lisp�ȉ��ɒu���Ă��������B

���ݒ�

	.xyzzy�Ȃǂ֐ݒ�

		(require "ni/setup")
		;; �����ݒ�𗘗p����
		(ni-autoload)
		;; PROXY��ʂ��ꍇ
		;(setq ni::*http-proxy-host* "proxy.host")  ; PROXY �̃z�X�g
		;(setq ni::*http-proxy-port* 8080)          ; PROXY �̃|�[�g

���N��

	M-x netinstaller

	�E�ŏ��̓T�C�g���o�^����Ă��܂���̂ŁA

		"a" => "http://www7a.biglobe.ne.jp/~hat/xyzzy/packages.l"

	  �œo�^���Ă��������B

	�E�z�z�T�C�g�ꗗ�͈ȉ��������������B

	xyzzy Wiki �� NetInstaller/�z�z�p�b�P�[�W�ꗗ
	http://xyzzy.s53.xrea.com/wiki/index.php?NetInstaller%2F%C7%DB%C9%DB%A5%D1%A5%C3%A5%B1%A1%BC%A5%B8%B0%EC%CD%F7

���L�[�o�C���h

	[�T�C�g�ꗗ]
		Enter	�p�b�P�[�W�ꗗ���J��
		Space	����
		r	���̍s�̃T�C�g���X�V
		R	�S�T�C�g���X�V
		a	URL�܂��̓��[�J���t�@�C������T�C�g��ǉ�
		d	���̍s�̃T�C�g���폜
		C-k	���̍s�̃T�C�g��؂���
		C-y	�؂������T�C�g��\��t��
		o	�T�C�g���u���E�U�ŊJ��
		q	�I��

	[�p�b�P�[�W�ꗗ]
		Enter	���̍s�̃p�b�P�[�W�̃}�[�N������^�͂���
		U	�A�b�v�f�[�g���ꂽ�p�b�P�[�W�Ƀ}�[�N������
		a	�S�Ẵp�b�P�[�W�Ƀ}�[�N������
		A	�S�Ẵp�b�P�[�W�̃}�[�N���͂���
		i	���̍s�̃p�b�P�[�W���C���X�g�[��
		I	�}�[�N�����p�b�P�[�W���C���X�g�[��
		d	���̍s�̃p�b�P�[�W���A���C���X�g�[��
		D	�}�[�N�����p�b�P�[�W���A���C���X�g�[��
		Space	���̍s�̃p�b�P�[�W�ڍׂ�\��
		n	���̍s�̃p�b�P�[�W�ڍׂ�\��
		p	�O�̍s�̃p�b�P�[�W�ڍׂ�\��
		t	�\���p�b�P�[�W���g�O��
		q	�p�b�P�[�W�ꗗ�����

	[�p�b�P�[�W�ڍ�]
		Enter	�ڍׂ����
		q	����
		f	���̍s�̃C���X�g�[���ς݃t�@�C�����J��
		Space	���y�[�W�������͏ڍׂ����
		n	���̃p�b�P�[�W�ڍׂ�\��
		p	�O�̃p�b�P�[�W�ڍׂ�\��

	[�C���X�g�[���ς݃t�@�C��]
		Enter	�t�@�C�������
		Space	���y�[�W
		BS	�O�y�[�W
		q	�t�@�C�������

�� ���_

	�E�A���C���X�g�[�����Ƀf�B���N�g���������Ȃ��B
	�E���̑����Ԃ񂢂낢�날��ł��傤�B

�� ���̑�

	�ENetInstaller�����z�z���@�� howto.txt ��ǂ�ŉ������B

�� TODO

	�E�A�b�v�f�[�g�̎��̏����Ƃ��B
	�E�ˑ��֌W���݂�Ƃ��B

�� �ύX����

	[2007/11/25]
		ni-0.0.1.1-5
		�E[�\�t�g�E�F�A��] xyzzy�̎g������������ʂ₵ ��܂����� ����11 ��137���C���B

	[2007/11/21]
		ni-0.0.1.1-4
		�ESANO����w�E�̐Ǝ㐫�Ή��B(__)

	[2006/09/05]
		ni-0.0.1.1-3
		�E�u�S�T�C�g���X�V�v���ɓr���ŃG���[���������Ă������𑱂���悤�ɂ����B

	[2005/11/28]
		ni-0.0.1.1-2
		�Exyzzy Wiki NetInstaller ��kto����w�E�̃o�O���C���B(__)

	[2005/11/23]
		ni-0.0.1.1
		�E���C�Z���X��K�p
		�Eni::*base-directory* �� "~/.netinst" ���� "(si:system-root)/.netinst" ��
		�@�ύX�B

	[2003/06/21]
		ni-0.0.1.0
		�E�p�b�P�[�W�ڍׂŃC���X�g�[���ς݃t�@�C�����J����悤�ɂ����B
		�E�J�e�S���[�ʕ\��
		�E�e��hook�̒ǉ��B

	[2003/06/13]
		ni-0.0.0.9
		�E�p�b�P�[�W�ꗗ�ł̕\�����g�O���B
		�E*app-cols-name-max* �ňꗗ�ŕ\������p�b�P�[�W���̍ő�J���������w��ł�
		�@��悤�ɂ����B�f�t�H���g�͐����Ȃ��B

	[2003/05/11]
		ni-0.0.0.8
		�E�J�[�\���ʒu���ς������Ƃ����������ƏC���B
		�E�C���X�g�[����Ƀ��[�h����t�@�C�����w��ł���悤�ɂ����B(howto.txt�Q��)
		�E�A���C���X�g�[���O�Ƀ��[�h����t�@�C�����w��ł���悤�ɂ����B
		�E�T�C�g���̕ύX���C���X�g�[���ς݃p�b�P�[�W�ɔ��f����悤�ɂ����B

	[2003/05/01]
		ni-0.0.0.7
		�E�T�C�g�ꗗ�Ńp�b�P�[�W����\������悤�ɂ����B
		�EM-x netinstaller �ŕ��A���鎞�̓����������ƏC���B
		�E�T�C�g�̍X�V���Ȃ�ׂ������T�C�g�ꗗ�ɔ��f����悤�ɂ����B
		�E�A�b�v�f�[�g���ɗ]���Ȃ��̂܂őޔ�����ꍇ���������̂𒼂����B
		�E�p�b�P�[�W�ڍׂŃC���X�g�[�����ꂽ�t�@�C���ꗗ��\������悤�ɂ����B

	[2003/04/26]
		ni-0.0.0.6
		�E�p�b�P�[�W�ڍׂ�\������E�B���h�E�����������Ȃ�̂𒼂����B
		�E�T�C�g�ꗗ�̃R�}���h�ǉ��i�L�[�o�C���h�Q�Ɓj
		�E�p�b�P�[�W�ꗗ�̃R�}���h�ǉ��i�L�[�o�C���h�Q�Ɓj
		�E�A�b�v�f�[�g�̒��~���Ɍ��̃o�[�W������߂�������������B�i���M�Ȃ��j

	[2003/04/25]
		ni-0.0.0.5
		�E�ύX�_�A���ӏ��������ڂɒǉ��B

	[2003/04/15]
		ni-0.0.0.4
		�E�A�[�J�C�u�`���ɂ���ẮA��ɏ㏑���m�F���o�Ă��܂��̂𒼂����B

	[2003/04/13]
		ni-0.0.0.3
		�E�o�O�Ƃ�

	[2003/04/12]
		ni-0.0.0.2
		�E���j���[��1�s�ڂœo�^�ł��Ȃ��̂𒼂����B

	[2003/04/06]
		ni-0.0.0.1
		�E����
