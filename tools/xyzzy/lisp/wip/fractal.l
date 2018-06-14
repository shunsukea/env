;;; -*- Mode: Lisp; Package: WIN-USER -*-
;;;
;;; �P�Ȃ�Fractal�}�`
;;;
;;; ��������
;;; (load-library "wip/fractal")
;;; (win-user::mandelbrot)
;;;
;;; �E�V�����ɂȂ�񂮂炢�x���̂ŃX�N���[���Z�[�o�[��؂��Ƃ��Ȃ���
;;;   �߂����v�������邩���B�C�̒Z���l��C-g�������ĂˁB
;;; �E*fractal-pels*�����点�΂�����Ƃ͑����Ȃ�B
;;; �E�F�̃o�����X�����܂����B�Ԗ������Ȃ��B
;;; �E��ʂ̐F����6���F�ȏザ��Ȃ��ƁA�F�������ƂłȂ�

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "wip/winapi"))

(in-package "win-user")

(defconstant *fractal-class* "fractalClass")

(defvar *hwnd-fractal* nil)

(defparameter *fractal-pels* 600)

(defparameter *fractal-width* 3.0d0)
(defparameter *mandelbrot-center* #C(-0.5d0 0.0d0))

(unless (fboundp 'fractal-wndproc-stub)
  (defun-c-callable LRESULT fractal-wndproc-stub
      ((HWND hwnd) (UINT msg) (WPARAM wparam) (LPARAM lparam))
    (fractal-wndproc hwnd msg wparam lparam)))

(defun fractal-wndproc (hwnd msg wparam lparam)
  (when (= msg WM_NCDESTROY)
    (setq *hwnd-fractal* nil))
  (DefWindowProc hwnd msg wparam lparam))

(defun register-fractal-window ()
  (let ((wc (make-WNDCLASS)))
    (setf (WNDCLASS-style wc) (logior CS_HREDRAW CS_VREDRAW))
    (setf (WNDCLASS-lpfnWndProc wc) #'fractal-wndproc-stub)
    (setf (WNDCLASS-cbClsExtra wc) 0)
    (setf (WNDCLASS-cbWndExtra wc) 0)
    (setf (WNDCLASS-hInstance wc) (GetModuleHandle 0))
    (setf (WNDCLASS-hIcon wc) (LoadIcon 0 (MAKEINTRESOURCE IDI_APPLICATION)))
    (setf (WNDCLASS-hCursor wc) (LoadCursor 0 (MAKEINTRESOURCE IDC_ARROW)))
    (setf (WNDCLASS-hbrBackground wc) (+ 1 COLOR_WINDOW))
    (setf (WNDCLASS-lpszMenuName wc) 0)
    (setf (WNDCLASS-lpszClassName wc)
	  (si:make-string-chunk *fractal-class*))
    (RegisterClass wc)))

(defun create-fractal-window ()
  (let ((hwnd (CreateWindow (si:make-string-chunk *fractal-class*)
			    (si:make-string-chunk "fractal")
			    WS_OVERLAPPEDWINDOW
			    CW_USEDEFAULT CW_USEDEFAULT
			    *fractal-pels* *fractal-pels*
			    0 0 (GetModuleHandle 0) 0)))
    (ShowWindow hwnd SW_SHOW)
    (UpdateWindow hwnd)
    hwnd))

(defun fractal-init ()
  (cond (*hwnd-fractal*
	 (InvalidateRect *hwnd-fractal* 0 1)
	 (UpdateWindow *hwnd-fractal*))
	(t
	 (register-fractal-window)
	 (setq *hwnd-fractal* (create-fractal-window)))))

(setf (get 'with-fractal-paint 'ed:lisp-indent-hook) 1)

(defmacro with-fractal-paint ((hdc) &body body)
  `(unwind-protect
       (let (,hdc)
	 (progn
	   (fractal-init)
	   (setq ,hdc (GetDC *hwnd-fractal*))
	   ,@body)
	 (when ,hdc
	   (ReleaseDC *hwnd-fractal* ,hdc)))))

(*define-dll-entry COLORREF SetPixel (HDC int int COLORREF) "gdi32")

(defun plot-color (n)
  (if (> n 512)
      0
    (+ (* (+ 31 (* 28 (rem n 8))) 65536)
       (* (+ 31 (* 28 (rem (truncate n 8) 8))) 256)
       (+ 31 (* 28 (truncate n 64))))))

(defun mandelbrot ()
  (with-fractal-paint (hdc)
    (let ((x-off (- (realpart *mandelbrot-center*)
		    (/ *fractal-width* 2.0d0)))
	  (y-off (- (imagpart *mandelbrot-center*)
		    (/ *fractal-width* 2.0d0)))
	  (ratio (/ (float *fractal-width* 0.0d0)
		    *fractal-pels*)))
      (dotimes (y *fractal-pels*)
	(dotimes (x *fractal-pels*)
	  (let ((C (complex (+ x-off (* ratio x))
			    (+ y-off (* ratio y)))))
	    (SetPixel hdc x y
		      (plot-color (do ((n 0 (1+ n))
				       (z 0.0d0 (+ (* z z) C)))
				      ((or (>= (abs z) 2.0d0)
					   (> n 512))
				       n))))))))))

;; Julia set���Ă���Ȃ�?
(defun julia ()
  (with-fractal-paint (hdc)
    (let ((x-off (- (/ *fractal-width* 2.0d0)))
	  (y-off (- (/ *fractal-width* 2.0d0)))
	  (ratio (/ (float *fractal-width* 0.0d0)
		    *fractal-pels*))
	  (C #C(0.4 0.6)))
      (dotimes (y *fractal-pels*)
	(dotimes (x *fractal-pels*)
	  (SetPixel hdc x y
		    (plot-color (do ((n 0 (1+ n))
				     (z
				      (complex (+ x-off (* ratio x))
					       (+ y-off (* ratio y)))
				      (+ (* z z) C)))
				    ((or (>= (abs z) 2.0d0)
					 (> n 512))
				     n)))))))))