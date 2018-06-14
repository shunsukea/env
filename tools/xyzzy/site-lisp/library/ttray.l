;;; -*- Mode: Lisp; Package: WIN-USER -*-
;;;
;;; ttray.l --- �^�X�N�g���C�g��

;;; author
;;;     YAMAMOTO Taizo

;;; date
;;;     Last updated: "2003-04-29"

;;; description
;;;     �^�X�N�g���C�ɋT�̃A�C�R����\�����܂��B

;;; install
;;;     1.�𓀂��ďo����ttray.l��$XYZZY/site-lisp/�ɃR�s�[���܂��B
;;;     2.~/.xyzzy��������$XYZZY/site-lisp/siteini.l�ɋL�q��ǉ����܂��B
;;;	  
;;;		--------------------------------
;;;             (require "ttray")
;;;             (global-set-key '(#\C-x #\C-c) 'ttray-hide-xyzzy)
;;;		--------------------------------
;;;
;;;     3.��L�̐ݒ�𔽉f�����邽�߂ɁAxyzzy���ċN�����܂��B
;;;	  ��siteinit.l�ɋL�q�����ꍇ�ɂ͍ă_���v�����ĉ������B

;;; note
;;;     1.C-x C-c�ŏ풓���Ă镵�͋C�����킦�܂��B�����܂ŃE�B��
;;;     �@�h�E���\�����Ă邾���ł��̂ŁA�I���͑��̎�i��p��
;;;     �@�ĉ������B
;;;     2.�^�X�N�g���C�����N���b�N����ƕ\���^��\����؂�ւ����܂��B
;;;     3.�^�X�N�g���C�ŉE�N���b�N�Ń��j���[���\������܂��B
;;;     �@���j���[�͎��R�ɕύX���邱�Ƃ��\�ł��B
;;;     4.Win32API�����Ȃ�g���Ă܂����A�������g���Ă��鎩�M�͂���܂���B
;;;     �@�����A�E�B���h�E�̕\���𐧌䂷��A�N�Z�T���n�c�[���Ƃ̑���������
;;;     �@���Ƃ��\�z����܂��B�댯��������g�p�����~�߂ĉ������B

;;; customize
;;;     1.�P�Ƀ��j���[��ύX�������ꍇ�ɂ́Awin-user::*ttray-menu-list*��
;;;       .xyzzy�̒��Őݒ肵�Ă���(require "ttray")����B
;;;     2.���I�Ƀ��j���[��ς������ꍇ�ɂ́A�E�N���b�N����
;;;       win-user::*ttray-initmenupopup-hook*��funcall�����̂ŁA
;;;       win-user::ttray-build-popup-menu���g���čD���ȃ��j���[�ɕύX����B
;;;     ��user�ł͂Ȃ�win-user�̒��Œ�`���Ă���̂Ńp�b�P�[�W�C���q�ɒ���

;;; todo
;;;     1.���j���[��̍��ڂ���֐������s���Ă���������G�f�B�^���ɂ͔��f����Ȃ�
;;;     �@wndproc���ʃX���b�h�ɂȂ��Ă�̂������Ȃ̂ł��傤�ˁc
;;;     2.���[�h���X�t�@�C���������オ���Ă�ƍŏ������ɔ�\���ɂȂ�Ȃ�
;;;     �@����͖��������c

;;; changes
;;;	Sun, 27 Apr 2003 20:21:22 +0900
;;;		startup�����[�h�ς݂��ǂ������`�F�b�N���āA
;;;		siteinit.l���ǂ����𔻒肷�郍�W�b�N��ǉ��i���܂�ǂ��Ȃ��C������j
;;;	Fri, 24 May 2002 22:07:51 +0900
;;;		ExtractIconEx()���Ăяo����悤�ɕύX
;;;	Thu, 25 Apr 2002 01:40:03 +0900
;;;		�A�C�R����`�b�v���ȒP�ɕύX�ł���悤�Ȋ֐��ǉ�
;;;	Mon, 15 Apr 2002 23:24:13 +0900
;;;		���_�u���N���b�N������蓦���Ă��̂��C��
;;;     Sat, 13 Apr 2002 22:43:32 +0900
;;;             ���l���^���ŉE�N���b�N���j���[��ǉ�
;;;             �Ecallback����wndproc�������Ă�xyzzycli�������Ă�
;;;               ���ǉ�������̖����ł��Ȃ��Ƃ��낪����
;;;     Tue, 09 Apr 2002 12:51:48 +0900
;;;             ����

(provide "ttray")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "foreign")
  (require "wip/winapi"))

;;; winapi
(in-package "winapi")

(*define-c-macro LOWORD(l) `(WORD ,l))
(*define-c-macro HIWORD(l) `(WORD (truncate ,l #x10000)))

(*define-c-struct NOTIFYICONDATA
  (DWORD        cbSize)
  (HWND         hWnd)
  (UINT         uID)
  (UINT         uFlags)
  (UINT         uCallbackMessage)
  (HICON        hIcon)
  (CHAR         szTip 64))

(*define-dll-entry HMENU CreatePopupMenu () "user32")
(*define-dll-entry BOOL AppendMenu (HMENU UINT UINT LPSTR) "user32" "AppendMenuA")
(*define-dll-entry BOOL DestroyMenu (HMENU) "user32")
(*define-dll-entry BOOL GetCursorPos ((POINT *)) "user32")
(*define-dll-entry BOOL SetForegroundWindow (HWND) "user32")
(*define-dll-entry HICON ExtractIcon (HINSTANCE LPSTR UINT) "shell32" "ExtractIconA")
(*define-dll-entry HICON ExtractIconEx (LPSTR c:int (HICON *) (HICON *) UINT) "shell32" "ExtractIconExA")
(*define-dll-entry BOOL DestroyIcon (HICON) "user32")
(*define-dll-entry BOOL TrackPopupMenu (HMENU UINT int int int HWND (RECT *)) "user32")
(*define-dll-entry LRESULT PostMessage (HWND UINT WPARAM LPARAM) "user32" "PostMessageA")
(*define-dll-entry BOOL IsIconic (HWND) "user32")
(*define-dll-entry BOOL ShowWindow (HWND c:int) "user32")
(*define-dll-entry BOOL DestroyWindow (HWND) "user32")
(*define-dll-entry BOOL Shell_NotifyIcon (DWORD (NOTIFYICONDATA *)) "shell32" "Shell_NotifyIconA")
(*define-dll-entry BOOL UnregisterClass (LPCSTR HINSTANCE) "user32" "UnregisterClassA")

(*define NIM_ADD                #x0000)
(*define NIM_MODIFY             #x0001)
(*define NIM_DELETE             #x0002)

(*define NIF_MESSAGE            #x0001)
(*define NIF_ICON               #x0002)
(*define NIF_TIP                #x0004)

(*define SW_HIDE                0)
(*define SW_SHOWNORMAL          1)
(*define SW_NORMAL              1)
(*define SW_SHOWMINIMIZED       2)
(*define SW_SHOWMAXIMIZED       3)
(*define SW_MAXIMIZE            3)
(*define SW_SHOWNOACTIVATE      4)
(*define SW_SHOW                5)
(*define SW_MINIMIZE            6)
(*define SW_SHOWMINNOACTIVE     7)
(*define SW_SHOWNA              8)
(*define SW_RESTORE             9)
(*define SW_SHOWDEFAULT         10)
(*define SW_FORCEMINIMIZE       11)
(*define SW_MAX                 11)

(*define WM_NULL                #x0000)
(*define WM_ACTIVATE            #x0006)
(*define WM_CLOSE               #x0010)
(*define WM_SHOWWINDOW          #x0018)
(*define WM_COMMAND             #x0111)
(*define WM_INITMENUPOPUP       #x0117)
(*define WM_LBUTTONDOWN         #x0201)
(*define WM_LBUTTONUP           #x0202)
(*define WM_LBUTTONDBLCLK       #x0203)
(*define WM_RBUTTONDOWN         #x0204)
(*define WM_RBUTTONUP           #x0205)
(*define WM_RBUTTONDBLCLK       #x0206)
(*define WM_MBUTTONDOWN         #x0207)
(*define WM_MBUTTONUP           #x0208)
(*define WM_MBUTTONDBLCLK       #x0209)

(*define MF_ENABLED             #x0000)
(*define MF_GRAYED              #x0001)
(*define MF_DISABLED            #x0002)
(*define MF_SEPARATOR           #x0800)
(*define MF_UNCHECKED           #x0000)
(*define MF_CHECKED             #x0008)

(*define TPM_LEFTBUTTON         #x0000)
(*define TPM_RIGHTBUTTON        #x0002)

(*define TPM_LEFTALIGN          #x0000)
(*define TPM_CENTERALIGN        #x0004)
(*define TPM_RIGHTALIGN         #x0008)

(*define TPM_TOPALIGN           #x0000)
(*define TPM_VCENTERALIGN       #x0010)
(*define TPM_BOTTOMALIGN        #x0020)
(*define TPM_HORIZONTAL         #x0000)
(*define TPM_VERTICAL           #x0040)
(*define TPM_NONOTIFY           #x0080)
(*define TPM_RETURNCMD          #x0100)

(*define IDI_XYZZY              103)
(*define IDI_FILER              136)

(*define WM_TTRAY_NOTIFY        #x0400)

;;; win-user
(in-package "win-user")

(defconstant *ttray-class* "XyzzyTTrayClass")

; �V���v����
(defvar *ttray-use-xyzzycli* nil)
(defvar *ttray-menu-list*
  '(; ���� 1:name 2:checked 3:show-after-function 4:function
    ("�I��(&X)" nil nil
     ; wndproc�����kill�͊댯�ȋC������̂Ń��b�Z�[�W��ʒm
     (lambda () (PostMessage (ed::get-window-handle) WM_CLOSE 0 0)))))

;;; ; *ttray-menu-list*�̃T���v��
;;; (defvar *ttray-use-xyzzycli* t)
;;; (defvar *ttray-menu-list*
;;;   ; *ttray-menu-list*�̃T���v��
;;;   '(; ���� 1:name 2:checked 3:show-after-function 4:function
;;;     ("�V�K�쐬(&N)" nil t ed::new-file)
;;;     ("�J��(&O)..." nil t ed::open-file-dialog)
;;;     ("����݂��J��(&D)..." nil t ed::open-session-dialog)
;;;     ("grep�̎��s(&G)..." nil t ed::grep-dialog)
;;;     ("�ޯ̧�I��(&B)..." nil t ed::select-buffer)
;;;     :sep
;;;     ("̧��(&F)" nil nil ed::open-filer)
;;;     :sep
;;;     ; xyzzycli�����kill-xyzzy�͑��v����
;;;     ("�I��(&X)" nil nil ed::kill-xyzzy)))

(defvar *ttray-initmenupopup-hook* nil)
(defvar *ttray-show-hook* nil)
(defvar *ttray-hide-hook* nil)
(defvar *ttray-status* t)

(defvar *ttray-wc* nil)
(defvar *ttray-hwnd* nil)
(defvar *ttray-icon* nil)
(defvar *ttray-hmenu* nil)

(defvar *ttray-tip* (ed::concat "xyzzy version " (ed::software-version)))
(defvar *ttray-icon-show* nil)
(defvar *ttray-icon-hide* nil)

;;; ; *ttray-initmenupopup-hook*�̃T���v��
;;; (setq *ttray-use-xyzzycli* nil)
;;; (setq *ttray-menu-list*
;;;       '(("�\�����" ttray-status nil ttray-toggle-xyzzy)))
;;; (setq *ttray-initmenupopup-hook*
;;;       'ttray-build-popup-menu)

; window procedure
(defun-c-callable LRESULT ttray-wndproc
  ((HWND hwnd) (UINT msg) (WPARAM wparam) (LPARAM lparam))
  (cond ((= msg WM_NCDESTROY)
	 (setq break-loop t))
	((= msg WM_PAINT)
	 (let* ((ps (make-PAINTSTRUCT))
		(hdc (BeginPaint hwnd ps)))
	   (EndPaint hwnd ps))
	 (return-from ttray-wndproc 0))
	; initmenupopup
	((= msg WM_INITMENUPOPUP)
	 (when *ttray-initmenupopup-hook*
	   (funcall *ttray-initmenupopup-hook*))
	 (return-from ttray-wndproc 0))
	; tasktray
	((= msg WM_TTRAY_NOTIFY)
	 (cond ; popup menu
	  ((= lparam WM_RBUTTONUP) (ttray-popup-menu))
	  ; activation / deactivation
	  ((= lparam WM_LBUTTONDOWN) (ttray-toggle-xyzzy))
	  ((= lparam WM_LBUTTONDBLCLK) (ttray-toggle-xyzzy)))
	 (return-from ttray-wndproc 0))
	; popup-function
	((= msg WM_COMMAND)
	 (ttray-popup-callback (LOWORD wparam))
	 (return-from ttray-wndproc 0)))
  (DefWindowProc hwnd msg wparam lparam))

; popup menu
(defun ttray-popup-menu ()
  (let ((p (make-POINT)))
    (GetCursorPos p)
    ; PRB: Menus for Notification Icons Do Not Work Correctly (Q135788)
    (SetForegroundWindow *ttray-hwnd*)
    (TrackPopupMenu *ttray-hmenu*
		    (logior TPM_LEFTBUTTON TPM_RIGHTBUTTON)
		    (POINT-x p) (POINT-y p) 0 *ttray-hwnd* 0)
    (PostMessage *ttray-hwnd* WM_NULL 0 0)))

; popup menu callback
(defun ttray-popup-callback (idx)
  (let ((elm (nth idx *ttray-menu-list*)))
    (ed::refresh-screen t)
    (and (cadddr elm)
	 (cond (*ttray-use-xyzzycli*
		(ed::launch-application
		   (format nil "xyzzycli.exe -f ~S" (cadddr elm))))
	       (t
		(funcall (cadddr elm))))
	 (caddr elm)
	 (ttray-show-xyzzy))))

; build popup menu
(defun ttray-build-popup-menu (&optional menu-list)
  (when menu-list
    (setq *ttray-menu-list* menu-list))
  (when *ttray-hmenu*
    (DestroyMenu *ttray-hmenu*))
  (setq *ttray-hmenu* (CreatePopupMenu))
  (let ((cnt 0))
    (dolist (itm *ttray-menu-list*)
      (cond ((eq itm :sep)
	     (AppendMenu *ttray-hmenu* MF_SEPARATOR
			 cnt (si:make-string-chunk "")))
	    ((cdr itm)
	     (AppendMenu *ttray-hmenu*
			 (cond ((and (cadr itm)
				     (funcall (cadr itm)))
				(logior MF_ENABLED MF_CHECKED))
			       (t
				MF_ENABLED))
			 cnt (si:make-string-chunk (car itm))))
	    ((car itm)
	     (AppendMenu *ttray-hmenu* (logior MF_DISABLED MF_GRAYED)
			 cnt (si:make-string-chunk (car itm)))))
      (incf cnt))))

; setup routine
(defun ttray-setup ()
  ; icon
  (unless *ttray-icon-show*
    (setq *ttray-icon-show* (LoadIcon (GetModuleHandle 0) IDI_FILER)))
  (unless *ttray-icon-hide*
    (setq *ttray-icon-hide* (LoadIcon (GetModuleHandle 0) IDI_XYZZY)))
  ; window
  (setq *ttray-wc* (make-WNDCLASS))
  (setf (WNDCLASS-style *ttray-wc*) 0)
  (setf (WNDCLASS-lpfnWndProc *ttray-wc*) #'ttray-wndproc)
  (setf (WNDCLASS-cbClsExtra *ttray-wc*) 0)
  (setf (WNDCLASS-cbWndExtra *ttray-wc*) 0)
  (setf (WNDCLASS-hInstance *ttray-wc*) (GetModuleHandle 0))
  (setf (WNDCLASS-hIcon *ttray-wc*) (LoadIcon 0 (MAKEINTRESOURCE IDI_APPLICATION)))
  (setf (WNDCLASS-hCursor *ttray-wc*) (LoadCursor 0 (MAKEINTRESOURCE IDC_ARROW)))
  (setf (WNDCLASS-hbrBackground *ttray-wc*) (+ 1 COLOR_WINDOW))
  (setf (WNDCLASS-lpszMenuName *ttray-wc*) 0)
  (setf (WNDCLASS-lpszClassName *ttray-wc*) (si:make-string-chunk *ttray-class*))
  (RegisterClass *ttray-wc*)
  (setq *ttray-hwnd*
	(CreateWindow (si:make-string-chunk *ttray-class*)
		      (si:make-string-chunk "XyzzyTTrayWindow")
		      0
		      CW_USEDEFAULT CW_USEDEFAULT
		      CW_USEDEFAULT CW_USEDEFAULT
		      0 0 (GetModuleHandle 0) 0))
  (UpdateWindow *ttray-hwnd*)
  (let ((msg (make-MSG))
	(break-loop nil))
    (declare (special break-loop)))
  ; popup menu
  (ttray-build-popup-menu)
  ; icon
  (setq *ttray-icon* (make-NOTIFYICONDATA))
  (setf (NOTIFYICONDATA-cbSize *ttray-icon*)
	(si:chunk-size *ttray-icon*))
  (setf (NOTIFYICONDATA-hWnd *ttray-icon*)
	*ttray-hwnd*)
  (setf (NOTIFYICONDATA-uID *ttray-icon*) 0)
  (setf (NOTIFYICONDATA-uFlags *ttray-icon*)
	(logior NIF_ICON
		NIF_MESSAGE
		NIF_TIP))
  (setf (NOTIFYICONDATA-uCallbackMessage *ttray-icon*) WM_TTRAY_NOTIFY)
  (setf (NOTIFYICONDATA-hIcon *ttray-icon*) *ttray-icon-show*)
  (si:pack-string *ttray-icon*
     (c-struct-offset-of NOTIFYICONDATA winapi::szTip)
     *ttray-tip* 64)
  (Shell_NotifyIcon NIM_ADD *ttray-icon*))

; cleanup routine
(defun ttray-cleanup ()
  ; icon
  (Shell_NotifyIcon NIM_DELETE *ttray-icon*)
  ; popup menu
  (DestroyMenu *ttray-hmenu*)
  ; window
  (DestroyWindow *ttray-hwnd*)
  (UnregisterClass (si:make-string-chunk *ttray-class*) *ttray-hwnd*)
  (setq *ttray-wc* nil)
  (setq *ttray-hwnd* nil))

; toggle xyzzy
(defun ttray-toggle-xyzzy ()
  (if *ttray-status*
      (ttray-hide-xyzzy)
    (ttray-show-xyzzy)))

; hide xyzzy
(defun ttray-hide-xyzzy ()
  (when (and *ttray-status*
	     (ed::run-hook-with-args-while-success '*ttray-hide-hook*))
    (setq *ttray-status* nil)
    (ShowWindow (ed::get-window-handle) SW_HIDE)
    (ttray-change-icon :tip *ttray-tip* :icon-hide *ttray-icon-hide*)))

; show xyzzy
(defun ttray-show-xyzzy ()
  (if *ttray-status*
      (SetForegroundWindow (ed::get-window-handle))
    (when (ed::run-hook-with-args-while-success '*ttray-show-hook*)
      (setq *ttray-status* t)
      (ShowWindow (ed::get-window-handle) SW_SHOW)
      (PostMessage (ed::get-window-handle) WM_SHOWWINDOW 1 0)
      (unless (zerop (IsIconic (ed::get-window-handle)))
	(ShowWindow (ed::get-window-handle) SW_RESTORE))
      (ttray-change-icon :tip *ttray-tip* :icon-show *ttray-icon-show*))))

; change ...
(defun ttray-change-icon (&key tip icon-show icon-hide)
  (and tip
       (setq *ttray-tip* tip)
       (si:pack-string *ttray-icon*
	  (c-struct-offset-of NOTIFYICONDATA winapi::szTip)
	  *ttray-tip* 64))
  (and icon-show
       (setq *ttray-icon-show* icon-show)
       *ttray-status*
       (setf (NOTIFYICONDATA-hIcon *ttray-icon*) *ttray-icon-show*))
  (and icon-hide
       (setq *ttray-icon-hide* icon-hide)
       (not *ttray-status*)
       (setf (NOTIFYICONDATA-hIcon *ttray-icon*) *ttray-icon-hide*))
  (and (or tip icon-show icon-hide)
       (Shell_NotifyIcon NIM_MODIFY *ttray-icon*)))

; for modeless filer
(defun ttray-show-xyzzy-1 (dummy)
  (ttray-show-xyzzy)
  nil)

; for window minimizing
(defun ttray-hide-xyzzy-1 ()
  (unless (zerop (IsIconic (ed::get-window-handle)))
    (ttray-hide-xyzzy)))

; status
(defun ttray-status ()
  *ttray-status*)

;;; add-hook
(ed::add-hook 'ed::*before-find-file-hook* 'ttray-show-xyzzy-1)
(ed::add-hook 'ed::*deactivate-hook* 'ttray-hide-xyzzy-1)
(ed::add-hook 'ed::*activate-hook* 'ttray-show-xyzzy)
(ed::add-hook 'ed::*kill-xyzzy-hook* 'ttray-cleanup)

;;; ; *ttray-hide-hook*�̃T���v��
;;; (defun ttray-hide-default-function1 ()
;;;   (unwind-protect
;;;       (dolist (buf (ed::buffer-list))
;;;     (ed::delete-buffer buf))))
;;; (ed::add-hook '*ttray-hide-hook* 'ttray-hide-default-function1)

;;; ; *ttray-hide-hook*�̃T���v��
;;; (defun ttray-hide-default-function2 ()
;;;   (or (ed::yes-no-or-cancel-p "�^�X�N�g���C�ɏ풓���܂����H")
;;;       (ed::kill-xyzzy)))
;;; (ed::add-hook '*ttray-hide-hook* 'ttray-hide-default-function2)

;;; setup
(if (member "startup" *modules* :test #'string=)
    (ttray-setup)
  (ed::add-hook 'ed::*post-startup-hook* 'ttray-setup))

;;; user
(in-package "user")
(defun ttray-hide-xyzzy ()
  (interactive)
  (win-user::ttray-hide-xyzzy))