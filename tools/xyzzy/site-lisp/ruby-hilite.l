;;; Ruby Hilite Copyrite by Yukio Sakaue <yukimi_sake@mbi.nifty.com>

;;  ���e�����ƃL�[���[�h�̕\���ݒ�

(setf
 ruby-str-fore-color      9     ; ������̐F
 ruby-str-back-color      0
 ruby-comment-fore-color 10     ; �R�����g�̐F
 ruby-comment-back-color  0
 ruby-rd-fore-color       8     ; RD�̐F
 ruby-rd-back-color      15
 ruby-rgx-fore-color      3     ; ���K�\���̐F
 ruby-rgx-back-color      0
 ruby-percent-fore-color  3     ; %���e�����̐F
 ruby-percent-back-color  0
 ruby-here-fore-color     9     ; �q�A�h�L�������g�̐F
 ruby-here-back-color     0
 ruby-show-bold-keyword   t     ; �L�[���[�h���{�[���h�ɂ���
 )

;;; ruby-bold-keywords �͔p�~

(defvar *ruby-variables-regexp* 
	;�ϐ����̑��̋����\�� :bold���g���邪RD�╶����n�ł��{�[���h�̂܂܂Ȃ̂ŗv����
	(compile-regexp-keyword-list
	 '(
		 ("\$[0-9_\x7f-\xff]"           t (:keyword 2))            ; ����ϐ�
		 ("\$[a-z_][a-z0-9_]*"          t (:keyword 2))            ; �O���[�o���ϐ�
		 ("@[a-z_][a-zA-Z0-9_]*"        t (:color   4))            ; �C���X�^���X�ϐ�
		 ("\\<[+-]?[0-9]+\\.?[0-9]*[eE][+-]?[0-9]+\\.?[0-9]*\\>" t (:color 1)) ; �w�����e����
		 ("\\<[+-]?[0-9]+\\.?[0-9]*\\>" t (:color   1))            ; ���l���e����
		 ("\\<0[xX][0-9a-f]+\\>"        t (:color   1))            ; 16�i���e����
		 ("[^:]\\(:[a-z_][a-z0-9_]*\\)" t (:color  11 0 ) t 1)     ; �V���{��
		 )))
