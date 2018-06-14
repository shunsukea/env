;;-------------------------------------------------------------------
;; treeview.l - treeview.dll �g�p�T���v��
;;-------------------------------------------------------------------

;; Copyright (C) 2002 kazu.y
;;

;;--------------------
;;  �T�v
;;--------------------
;;
;;  ���̃t�@�C���� TreeCtrl �� xyzzy �ɖ��ߍ���Ŏg�p���邽�߂̃��C�u����
;;  TreeView.dll �������T���v���ł��B
;;
;;  # �s�V���������g���Â炢�̂ŁA���̃t�@�C���𒼐ڎg�킸�ɓ����@�\��
;;    �Ǝ��̃��b�p�������ăA�v���P�[�V�������J�������ق����悢�Ǝv���܂��B
;;
;;  # TreeView.dll ���G�N�X�|�[�g���Ă���֐��̈ꗗ����ъȒP�Ȑ�����
;;    readme.txt �ɂ���܂��B

;;--------------------
;;  �g�p���@
;;--------------------
;;
;;  treeview.dll �� xyzzy �Ɠ����f�B���N�g���ɁAlistview.l �� site-lisp/ �ɂ����A
;;  listview.l �����[�h (load-library treeview) ������A�Ⴆ�Έȉ��̂悤��
;;  �R�[�h�����s���܂��B
;;
;;  ; �T���v������1
;;  ; # �I���� (treeview-close-sample1) �ł��B
;;  (defvar tviewid1 nil)
;;  (defun treeview-sample1 ()
;;    (interactive)
;;    (setq tviewid1 (treeview-create))
;;    ; �_�u���N���b�N���ꂽ�Ƃ��ɏ����X�e�[�^�X�o�[�ɕ\������
;;    (treeview-dblclk-callback tviewid1 #'(lambda (hitem)
;;      (message (format nil "Double Click!! Text = ~A HasChildren = ~A"
;;        (treeview-get-item-text tviewid1 hitem)
;;        (treeview-dll-item-has-children tviewid1 hitem)))))
;;    ; �A�C�e���̒ǉ�
;;    (let ((fluits (treeview-insert-item tviewid1 "�t���[�c" TVI_ROOT)))
;;      (treeview-insert-item tviewid1 "���" fluits)
;;      (treeview-insert-item tviewid1 "�΂Ȃ�" fluits))
;;    (let ((vegetables (treeview-insert-item tviewid1 "���" TVI_ROOT)))
;;      (treeview-insert-item tviewid1 "�L���x�c" vegetables)
;;      (treeview-insert-item tviewid1 "�g�}�g" vegetables)))
;;  
;;  (defun treeview-close-sample1 ()
;;    (interactive)
;;    (treeview-close tviewid1))
;;  
;;  
;;  ; �T���v������2
;;  ; # �I���� (treeview-close-sample2) �ł��B
;;  (c:*define VK_ESCAPE     #x1B)
;;  ;(c:define-dll-entry winapi:HWND SetFocus (winapi:HWND) "user32")
;;  (defvar tviewid2 nil)
;;  (defun treeview-sample2 ()
;;    (interactive)
;;    ; TreeView �̍쐬
;;    (setq tviewid2 (treeview-create))
;;    ; �X�^�C�������낢��ύX
;;    (treeview-dll-modify-style tviewid2 0
;;      (logior TVS_HASBUTTONS TVS_HASLINES TVS_LINESATROOT) 0)
;;    ; ESC �� xyzzy �Ƀt�H�[�J�X���ڂ�
;;    (treeview-keydown-callback tviewid2 #'(lambda (hitem vkey flag)
;;      (if (= vkey VK_ESCAPE) (SetFocus (get-window-handle)))))
;;    ; �A�C�R���̓o�^
;;    (let ((open-icon  (treeview-add-file-icon tviewid2 "D:\\bin\\xyzzy\\open.ico"))
;;          (close-icon (treeview-add-file-icon tviewid2 "D:\\bin\\xyzzy\\close.ico"))
;;          (file-icon  (treeview-add-file-icon tviewid2 "D:\\bin\\xyzzy\\file.ico")))
;;      ; �A�C�e���̒ǉ�
;;      (let ((hedder (treeview-insert-item tviewid2 "�w�b�_�[�t�@�C��" TVI_ROOT :icon close-icon :sicon open-icon)))
;;        (treeview-insert-item tviewid2 "hoge.h" hedder :icon file-icon :sicon file-icon)
;;        (treeview-insert-item tviewid2 "fugo.h" hedder :icon file-icon :sicon file-icon)
;;        ; �c���[��W�J
;;        (treeview-dll-expand tviewid2 hedder TVE_EXPAND))
;;      (let ((source (treeview-insert-item tviewid2 "�\�[�X�t�@�C��" TVI_ROOT :icon close-icon :sicon open-icon)))
;;        (treeview-insert-item tviewid2 "hoge.cpp" source :icon file-icon :sicon file-icon)
;;        (treeview-insert-item tviewid2 "fugo.cpp" source :icon file-icon :sicon file-icon)
;;        ; �c���[��W�J
;;        (treeview-dll-expand tviewid2 source TVE_EXPAND))))
;;  
;;  (defun treeview-close-sample2 ()
;;    (interactive)
;;    (treeview-close tviewid2))


(require "foreign")
(require "wip/winapi")

;;-----------------
;; �G���Ȓ�`��
;;-----------------

; treeview-create-ex��flags
(c:*define XPIS_LEFT    0)
(c:*define XPIS_TOP     1)
(c:*define XPIS_RIGHT   2)
(c:*define XPIS_BOTTOM  3)
(c:*define XPIS_NOMOVE  4)
(c:*define XPIS_POSMASK 7)
(c:*define XPIS_ORDMASK #x30)
(c:*define XPIS_OUTSIDE #x10)
(c:*define XPIS_INSIDE  #x20)
(c:*define XPIS_NOORDER #x30)
(c:*define XPIS_GROUP   #x40)

; �^�̒�`
(c:*define-c-type winapi:LPSTR    LPTSTR)
(c:*define-c-type winapi:LPCSTR   LPCTSTR)
(c:*define-c-type (winapi:UINT *) PUINT)
(c:*define-c-type winapi:HANDLE   HTREEITEM)

(c:*define-c-struct TVITEM
  (winapi:UINT    mask           )
  (HTREEITEM      hItem          )
  (winapi:UINT    state          )
  (winapi:UINT    stateMask      )
  (LPTSTR         pszText        )
  (c:int          cchTextMax     )
  (c:int          iImage         )
  (c:int          iSelectedImage )
  (c:int          cChildren      )
  (winapi:LPARAM  lParam         ))

; �X�^�C���ݒ�p - ModifyStyle ()
(c:*define TVS_HASBUTTONS       #x0001)
(c:*define TVS_HASLINES         #x0002)
(c:*define TVS_LINESATROOT      #x0004)
(c:*define TVS_EDITLABELS       #x0008)
(c:*define TVS_DISABLEDRAGDROP  #x0010)
(c:*define TVS_SHOWSELALWAYS    #x0020)
(c:*define TVS_RTLREADING       #x0040)
(c:*define TVS_NOTOOLTIPS       #x0080)
(c:*define TVS_CHECKBOXES       #x0100)
(c:*define TVS_TRACKSELECT      #x0200)
(c:*define TVS_SINGLEEXPAND     #x0400)
(c:*define TVS_INFOTIP          #x0800)
(c:*define TVS_FULLROWSELECT    #x1000)
(c:*define TVS_NOSCROLL         #x2000)
(c:*define TVS_NONEVENHEIGHT    #x4000)
(c:*define TVS_NOHSCROLL        #x8000)

; hitem �̑���Ɏg���� - InsertItem ()��
(c:*define TVI_ROOT       (- 0 #x10000))
(c:*define TVI_FIRST      (- 0 #x0FFFF))
(c:*define TVI_LAST       (- 0 #x0FFFE))
(c:*define TVI_SORT       (- 0 #x0FFFD))

; �ύX�ӏ��̃}�X�N - InsertItem (), SetItem ()
(c:*define TVIF_TEXT          #x0001)
(c:*define TVIF_IMAGE         #x0002)
(c:*define TVIF_PARAM         #x0004)
(c:*define TVIF_STATE         #x0008)
(c:*define TVIF_HANDLE        #x0010)
(c:*define TVIF_SELECTEDIMAGE #x0020)
(c:*define TVIF_CHILDREN      #x0040)

; ��ԕύX�p - InsertItem ()��
(c:*define TVIS_SELECTED        #x0002)
(c:*define TVIS_CUT             #x0004)
(c:*define TVIS_DROPHILITED     #x0008)
(c:*define TVIS_BOLD            #x0010)
(c:*define TVIS_EXPANDED        #x0020)
(c:*define TVIS_EXPANDEDONCE    #x0040)
(c:*define TVIS_OVERLAYMASK     #x0F00)
(c:*define TVIS_STATEIMAGEMASK  #xF000)
(c:*define TVIS_USERMASK        #xF000)
(c:*define TVIS_EXPANDPARTIAL   #x0080)

; Expand () �p
(c:*define TVE_COLLAPSE       #x0001)
(c:*define TVE_EXPAND         #x0002)
(c:*define TVE_TOGGLE         #x0003)
(c:*define TVE_COLLAPSERESET  #x8000)
(c:*define TVE_EXPANDPARTIAL  #x4000)

; GetNextItem (), Select ()
(c:*define TVGN_ROOT             #x0000)
(c:*define TVGN_NEXT             #x0001)
(c:*define TVGN_PREVIOUS         #x0002)
(c:*define TVGN_PARENT           #x0003)
(c:*define TVGN_CHILD            #x0004)
(c:*define TVGN_FIRSTVISIBLE     #x0005)
(c:*define TVGN_NEXTVISIBLE      #x0006)
(c:*define TVGN_PREVIOUSVISIBLE  #x0007)
(c:*define TVGN_DROPHILITE       #x0008)
(c:*define TVGN_CARET            #x0009)
(c:*define TVGN_LASTVISIBLE      #x000A)

;;-----------------
;; ����
;;-----------------

;; �֐��̃C���|�[�g

(let ((dll "TreeView.dll"))
  (c:define-dll-entry c:int treeview-dll-create (winapi:HWND (c:void *)) dll "Create")
  (c:define-dll-entry c:int treeview-dll-create-ex (winapi:HWND (c:void *) c:int winapi:DWORD) dll "CreateEx")
  (c:define-dll-entry c:int treeview-dll-close (c:int) dll "Close")
  (c:define-dll-entry winapi:HWND treeview-dll-get-hwnd (c:int) dll "GetHwnd")
  (c:define-dll-entry winapi:BOOL treeview-dll-setsize (c:int c:int c:int c:int c:int) dll "SetSize")
  (c:define-dll-entry winapi:BOOL treeview-dll-setpos (c:int winapi:DWORD) dll "SetPos")

  (c:define-dll-entry winapi:HWND treeview-dll-get-htree (c:int) dll "GetHtree")
  (c:define-dll-entry winapi:BOOL treeview-dll-modify-style (c:int winapi:DWORD winapi:DWORD winapi:UINT) dll "ModifyStyle")

  (c:define-dll-entry HTREEITEM treeview-dll-get-child-item (c:int HTREEITEM) dll "GetChildItem")
  (c:define-dll-entry winapi:UINT treeview-dll-get-count (c:int) dll "GetCount")
  (c:define-dll-entry winapi:BOOL treeview-dll-get-item (c:int (TVITEM *)) dll "GetItem")
  (c:define-dll-entry winapi:DWORD treeview-dll-get-item-data (c:int HTREEITEM) dll "GetItemData")
  (c:define-dll-entry winapi:UINT treeview-dll-get-item-state (c:int HTREEITEM winapi:UINT) dll "GetItemState")
  (c:define-dll-entry winapi:BOOL treeview-dll-get-item-text (c:int HTREEITEM LPTSTR c:int) dll "GetItemText")
  (c:define-dll-entry HTREEITEM treeview-dll-get-next-item (c:int HTREEITEM winapi:UINT) dll "GetNextItem")
  (c:define-dll-entry HTREEITEM treeview-dll-get-next-sibling-item (c:int HTREEITEM) dll "GetNextSiblingItem")
  (c:define-dll-entry HTREEITEM treeview-dll-get-parent-item (c:int HTREEITEM) dll "GetParentItem")
  (c:define-dll-entry HTREEITEM treeview-dll-get-prev-sibling-item (c:int HTREEITEM) dll "GetPrevSiblingItem")
  (c:define-dll-entry HTREEITEM treeview-dll-get-root-item (c:int) dll "GetRootItem")
  (c:define-dll-entry HTREEITEM treeview-dll-get-selected-item (c:int) dll "GetSelectedItem")
    (c:define-dll-entry winapi:BOOL treeview-dll-item-has-children (c:int HTREEITEM) dll "ItemHasChildren")
  (c:define-dll-entry winapi:BOOL treeview-dll-set-item
    (c:int HTREEITEM winapi:UINT LPCTSTR c:int c:int winapi:UINT winapi:UINT winapi:LPARAM) dll "SetItem")
  (c:define-dll-entry winapi:BOOL treeview-dll-set-item-data (c:int HTREEITEM winapi:DWORD) dll "SetItemData")
  (c:define-dll-entry winapi:BOOL treeview-dll-set-item-state (c:int HTREEITEM winapi:UINT winapi:UINT) dll "SetItemState")
  (c:define-dll-entry winapi:BOOL treeview-dll-set-item-text (c:int HTREEITEM LPCTSTR) dll "SetItemText")
  (c:define-dll-entry winapi:BOOL treeview-dll-delete-all-items (c:int) dll "DeleteAllItems")
  (c:define-dll-entry winapi:BOOL treeview-dll-delete-item (c:int HTREEITEM) dll "DeleteItem")
  (c:define-dll-entry winapi:HWND treeview-dll-edit-label (c:int HTREEITEM) dll "EditLabel")
  (c:define-dll-entry HTREEITEM treeview-dll-insert-item
    (c:int winapi:UINT LPCTSTR c:int c:int winapi:UINT winapi:UINT winapi:LPARAM HTREEITEM HTREEITEM) dll "InsertItem")
  (c:define-dll-entry winapi:BOOL treeview-dll-select (c:int HTREEITEM winapi:UINT) dll "Select")
  (c:define-dll-entry winapi:BOOL treeview-dll-expand (c:int HTREEITEM winapi:UINT) dll "Expand")
  (c:define-dll-entry winapi:UINT treeview-dll-sort-children (c:int HTREEITEM) dll "SortChildren")
  
  (c:define-dll-entry winapi:BOOL treeview-dll-set-click-callback (c:int (c:void *)) dll "SetClickCallback")
  (c:define-dll-entry winapi:BOOL treeview-dll-set-rclick-callback (c:int (c:void *)) dll "SetRclickCallback")
  (c:define-dll-entry winapi:BOOL treeview-dll-set-dblclk-callback (c:int (c:void *)) dll "SetDblclkCallback")
  (c:define-dll-entry winapi:BOOL treeview-dll-set-keydown-callback (c:int (c:int *)) dll "SetKeyDownCallback")
  (c:define-dll-entry winapi:BOOL treeview-dll-set-edit-label-callback (c:int (c:void *)) dll "SetEditLabelCallback")
  
  (c:define-dll-entry c:int treeview-dll-add-file-icon (c:int LPCTSTR) dll "AddFileIcon")
  (c:define-dll-entry c:int treeview-dll-add-icon (c:int winapi:HICON) dll "AddIcon")
  (c:define-dll-entry winapi:BOOL treeview-dll-remove-icon (c:int c:int) dll "RemoveIcon")
  (c:define-dll-entry winapi:BOOL treeview-dll-remove-all-icons (c:int) dll "RemoveAllIcons")

  (c:define-dll-entry winapi:BOOL treeview-dll-disable-char-jump (c:int winapi:BOOL) dll "DisableCharJump")

  (c:define-dll-entry c:int treeview-dll-simple-track-popup-menu (LPCTSTR) dll "SimpleTrackPopupMenu"))

;; (treeview-create)
;; <�T�v>
;;   �J��
;; <����>
;; <�߂�l>
;; <���>
(defun treeview-create ()
  (treeview-dll-create (get-window-handle) (si:plugin-arg)))

;; (treeview-create-ex (size 400) (pos XPIS_LEFT))
;; <�T�v>
;; �ʒu�A�T�C�Y���w�肵�ĊJ��
;; <����>
;; <�߂�l>
;; <���>
(defun treeview-create-ex (&optional (size 400) (pos XPIS_LEFT))
  (treeview-dll-create-ex (get-window-handle) (si:plugin-arg) size pos))

;; (treeview-close viewid)
;; <�T�v>
;;   ����
;; <����>
;; <�߂�l>
;; <���>
(defun treeview-close(viewid)
  (treeview-dll-close viewid))

;; (treeview-setpos lviewid size min max step)
;; <�T�v>
;;   �r���[�����T�C�Y����
;; <����>
;; <�߂�l>
;; <���>
;;   step��1�ł����C�����܂�
(defun treeview-setsize(viewid size min max step)
  (treeview-dll-setsize viewid size min max step))

;; (treeview-close lviewid)
;; <�T�v>
;;   �ʒu��ύX����
;; <����>
;;   pos - treeview-create-ex��pos�Ɠ���
;; <�߂�l>
;; <���>
(defun treeview-setpos(viewid pos)
  (treeview-dll-setpos viewid pos))

;; (treeview-insert-item viewid item-index text :icon :id)
;; <�T�v>
;; �A�C�e����}������
;; <����>
;; <�߂�l>
;; <���>
(defun treeview-insert-item (viewid text parent &key icon sicon)
  (let ((imask TVIF_TEXT))
    (if (null icon ) (setq icon  0) (setq imask (logior imask TVIF_IMAGE)))
    (if (null sicon) (setq sicon 0) (setq imask (logior imask TVIF_SELECTEDIMAGE)))
    (treeview-dll-insert-item
      viewid imask (si:make-string-chunk text) icon sicon 0 0 0 parent 0)))

;; (treeview-set-item-text viewid hitem text)
;; <�T�v>
;;   �A�C�e���̃e�L�X�g��ݒ肷��
;; <����>
;; <�߂�l>
;; <���>
(defun treeview-set-item-text (viewid hitem text)
  (treeview-dll-set-item-text viewid hitem (si:make-string-chunk text)))

;; (treeview-get-item-text viewid hitem (maxlen 100))
;; <�T�v>
;;   �A�C�e���̃e�L�X�g���擾����
;; <����>
;;   maxlen �͍ő�擾�\������B
;; <�߂�l>
;; <���>
(defun treeview-get-item-text (viewid hitem &optional (maxlen 100))
  (let ((text (si:make-chunk nil maxlen)))
    (treeview-dll-get-item-text viewid hitem text maxlen)
    (si:unpack-string text 0)))

;; (treeview-click-callback viewid fn)
;; <�T�v>
;;   �N���b�N���̃R�[���o�b�N
;; <����>
;; <�߂�l>
;; <���>
(let (callback)
  (c:defun-c-callable c:void treeview-click-callback-imp ((HTREEITEM hitem))
    (funcall callback hitem))
  (defun treeview-click-callback (viewid fn)
    (setq callback fn)
    (not (zerop (treeview-dll-set-click-callback viewid #'treeview-click-callback-imp)))))

;; (treeview-dblclk-callback viewid fn)
;; <�T�v>
;;   �_�u���N���b�N���̃R�[���o�b�N
;; <����>
;; <�߂�l>
;; <���>
(let (callback)
  (c:defun-c-callable c:void treeview-dblclk-callback-imp ((HTREEITEM hitem))
    (funcall callback hitem))
  (defun treeview-dblclk-callback (viewid fn)
    (setq callback fn)
    (not (zerop (treeview-dll-set-dblclk-callback viewid #'treeview-dblclk-callback-imp)))))

;; (treeview-rclick-callback viewid fn)
;; <�T�v>
;;   �E�N���b�N���̃R�[���o�b�N
;; <����>
;; <�߂�l>
;; <���>
(let (callback)
  (c:defun-c-callable c:void treeview-rclick-callback-imp ((HTREEITEM hitem))
    (funcall callback hitem))
  (defun treeview-rclick-callback (viewid fn)
    (setq callback fn)
    (not (zerop (treeview-dll-set-rclick-callback viewid #'treeview-rclick-callback-imp)))))

;; (treeview-keydown-callback viewid fn)
;; <�T�v>
;;   �L�[�_�E�����̃R�[���o�b�N
;; <����>
;; <�߂�l>
;; <���>
(let (callback)
  (c:defun-c-callable c:void treeview-keydown-callback-imp ((HTREEITEM hitem) (winapi:WORD vkey) (winapi:UINT flag))
    (funcall callback hitem vkey flag))
  (defun treeview-keydown-callback (viewid fn)
    (setq callback fn)
    (not (zerop (treeview-dll-set-keydown-callback viewid #'treeview-keydown-callback-imp)))))

;; (treeview-edit-label-callback viewid fn)
;; <�T�v>
;;   ���x���ҏW��̃R�[���o�b�N��ݒ肷��
;; <����>
;; <�߂�l>
;; <���>
;;   ���x���ҏW��ɓ��͂��ꂽ��������`�F�b�N�������ꍇ�ɗ��p���܂��B
;;   �R�[���o�b�N��ݒ肷��ƕҏW���ʂ� TreeCtrl �Ɏ����I�ɔ��f����Ȃ��Ȃ�܂��B
(let ((callback) (maxlen 100))
  (c:defun-c-callable c:void treeview-edit-label-callback-imp ((HTREEITEM hitem) (LPCTSTR text))
    (funcall callback hitem (si:unpack-string (si:make-chunk nil maxlen nil text) 0)))
  (defun treeview-edit-label-callback (viewid fn)
    (setq callback fn)
    (not (zerop (treeview-dll-set-edit-label-callback viewid #'treeview-edit-label-callback-imp)))))

;; (treeview-set-focus viewid)
;; <�T�v>
;;   TreeCtrl �Ƀt�H�[�J�X���ڂ�
;; <����>
;; <�߂�l>
;; <���>
;;  �t�͉��̂ق��̃h�L�������g�Q��
(c:define-dll-entry winapi:HWND SetFocus (winapi:HWND) "user32")
(defun treeview-set-focus (viewid)
  (interactive)
  (SetFocus (treeview-dll-get-hwnd viewid)))

#|
;; TreeCtrl �Ƀt�H�[�J�X������Ƃ���ESC�L�[�Ńt�H�[�J�X�� xyzzy �ɖ߂�
;; ���߂̃T���v��
(c:*define VK_ESCAPE     #x1B)
(treeview-keydown-callback viewid #'(lambda (index vkey flag)
  (if (= vkey VK_ESCAPE) (SetFocus (get-window-handle)))))
|#

;; (treeview-add-file-icon viewid filepath)
;; <�T�v>
;;   �A�C�R���t�@�C����o�^����
;; <����>
;; <�߂�l>
;;   �A�C�R���̓o�^�ԍ�
;; <���>
;;   �A�C�R���̓o�^�ԍ��͎g�p����Ƃ��̂��߂ɋL�����Ă����B
(defun treeview-add-file-icon (viewid filepath)
  (treeview-dll-add-file-icon viewid (si:make-string-chunk filepath)))

;; (treeview-set-item-icon viewid hitem icon sicon)
;; <�T�v>
;;   �A�C�e���̃A�C�R����ύX����
;; <����>
;;   icon �A�C�R���̓o�^�ԍ�
;; <�߂�l>
;; <���>
(defun treeview-set-item-icon (viewid hitem icon sicon)
  (treeview-dll-set-item viewid hitem
    (logior TVIF_IMAGE TVIF_SELECTEDIMAGE) 0 icon sicon 0 0 0))

;; (treeview-simple-track-popup-menu menu-format)
;; <�T�v>
;;   �|�b�v�A�b�v���j���[��\������
;; <����>
;;   menu-format
;;     ��) "�J��(&O)\n0\n�폜(&D)\n1\nsep\n2048\n�v���p�e�B(&P)\n0"
;;     "�J��(&O)\n0\n"��1�̃��j���[���ڂ�\���B
;;     "sep\n2048\n"�ŃZ�p���[�^�B
;;     "�폜(&D)\n1\n"�ŃA�C�e�����O���[�\���B
;;     �����̈Ӗ��͈ȉ��Q�ƁB
;;      #define MF_SEPARATOR        0x00000800L
;;      #define MF_ENABLED          0x00000000L
;;      #define MF_GRAYED           0x00000001L
;;      #define MF_DISABLED         0x00000002L
;;      #define MF_UNCHECKED        0x00000000L
;;      #define MF_CHECKED          0x00000008L
;;      #define MF_USECHECKBITMAPS  0x00000200L
;;      #define MF_STRING           0x00000000L
;; <�߂�l>
;;   �I�����ꂽ���j���[�̈ʒu��Ԃ�
;; <���>
;;    ���Ƃ���yzzy�֐��Ń|�b�v�A�b�v���j���[���r���[��ɕ\�����悤�Ƃ���ƁA
;;    �\���ʒu���}�E�X�J�[�\���̈ʒu���炸��Ă��܂������߂ɂ��̊֐�������
;;    ���܂������A�ǂ����define-popup-menu��track-popup-menu�Ő��������j���[
;;    �|�b�v�A�b�v���ł���݂����ł��B
;;    �Ƃ����킯�ŁA���G��(�Ɍ��炸)���j���[�̃|�b�v�A�b�v������ɂ́A
;;    ���̊֐����g�킸�A�ȉ��̃R�[�h��p���邱�Ƃ𐄏����܂��B
;;      (defun create-abc-menu ()
;;        (define-popup-menu
;;          (:item nil "�ق�(&H)"
;;           'hoge 100)
;;          :sep
;;          (:item nil "���ɂ�(&M)"
;;           'monyo)))
;;      (setq *abc-menu* (create-abc-menu))
;;      (track-popup-menu *abc-menu* :button2)
(defun treeview-simple-track-popup-menu (menu-format)
  (treeview-dll-simple-track-popup-menu (si:make-string-chunk menu-format)))

#|
�A�C�e�����\�[�g������@
ListView �Ƃ͈قȂ�AWin32API�����MFC�ɕW���Ŏ������\�[�g����֐�������܂��B

  (treeview-dll-sort-children viewid item)
|#

#|
�p�����L�[���������Ƃ��ɂ��̕�������n�܂�A�C�e���Ƀt�H�[�J�X���ړ����Ă��܂�
�̂�}�~������@�B

  (treeview-dll-disable-char-jump viewid 1)
|#