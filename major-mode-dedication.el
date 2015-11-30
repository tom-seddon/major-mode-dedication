;;; major-mode-dedication.el --- dedicate windows to major mode(s)  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-14 Tom Seddon

;; Author: Tom Seddon <emacs@tomseddon.plus.com>
;; Created: 05 Aug 2013
;; Keywords: languages

;; This file is not part of GNU Emacs.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; major mode dedication is a little bit like making a window
;; dedicated to a buffer using `set-window-dedicated-p', but the
;; dedication is per major mode rather than per buffer.
;;
;; A window may be "dedicated" to multiple major modes. This stretches
;; the definition of "dedicated" a little, but there you go.
;;
;; When a window is dedicated to a major mode, attempts to display a
;; buffer with that mode using `display-buffer' will use that window.
;; If there are multiple appropriate dedicated windows, one that is
;; already showing a buffer of that mode will be chosen by preference.
;;
;; Commands such as `switch-buffer' are unaffected.
;;
;; Major mode dedication is intended for singleton-type buffers such
;; as help, scratch, messages, compilation, grep, and so on.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst mmd-major-modes 'mmd-major-modes
  "symbol used for the window parameter storing the major modes
  list.")

(defvar mmd-verbose nil)

(defvar mmd-auto-dedicating-major-modes nil
  "list of major modes that auto-dedicate windows to themselves
  while their buffer is visible.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mmd-message (string &rest objects)
  (when mmd-verbose
      (message (apply 'format string objects))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mmd-major-mode-of-buffer (b)
  (with-current-buffer b
    major-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mmd-filter (f xs)
  "returns a new list, containing each element X of XS for
which (F X) is non-nil."
  (let* (result)
    (mapc (lambda (x)
	    (when (funcall f x)
	      (setq result (cons x result))))
	  xs)
    (nreverse result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mmd-windows ()
  "returns a list of all non-minibuffer windows in the current frame."
  (let (ws)
    (walk-windows (lambda (w)
		    (setq ws (cons w ws)))
		  'f			;never include minibuffer
		  t			;all frames
		  )
    (nreverse ws)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mmd-window-is-dedicated-to-mode (w m)
  "returns 'solely if window W is dedicated to mode M only,
'partly if it is dedicated to mode M and others, or nil if it is
not dedicated to M at all."
  (let ((modes (window-parameter w mmd-major-modes)))
    (when (member m modes)
      (if (cdr modes)
	  'partly
	'solely))))

(defun mmd-windows-dedicated-to-mode (m flag)
  (mmd-filter (lambda (w)
		(when (or (null flag)
			  (eq (mmd-window-is-dedicated-to-mode w m) flag))
		  w))
	      (mmd-windows)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
(defun mmd-find-auto-dedicated-window-for-mode (m)
  (when (member m mmd-auto-dedicating-major-modes)
    ;; find all windows displaying buffers of that mode
    (let ((ws (mmd-filter (lambda (w)
			    (when (eq m (mmd-major-mode-of-buffer (window-buffer w)))
			      w))
			  (mmd-windows))))
	  (car ws))))

(defun mmd-find-dedicated-window-for-mode (m flag)
  (car (mmd-windows-dedicated-to-mode m flag)))

(defun mmd-find-window-for-major-mode (mode)
  (or (mmd-find-auto-dedicated-window-for-mode mode)
      (mmd-find-dedicated-window-for-mode mode 'solely)
      (mmd-find-dedicated-window-for-mode mode 'partly)))

(defun mmd-find-window-for-buffer (buffer)
  "find appropriate mmd-dedicated window for BUFFER.

May return nil if there's nothing specifically suitable. In that
case, should pass the request on to the default emacs
functionality."
  (let ((mode (mmd-major-mode-of-buffer buffer)))
    (mmd-find-window-for-major-mode mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun mmd-set-selected-window-dedicated-buffer ()
;;   (interactive)
;;   (let* ((w (selected-window))
;; 	 (modes (window-parameter w mmd-major-modes)))
;;     (mapc (lambda (b)
;; 	    (if (member (with-current-buffer b
;; 			  major-mode)
;; 			modes)
;; 		(

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mmd-use-custom-display-buffer (name action)
  (mmd-message "mmd-use-custom-display-buffer: name=%s action=%s (window=%s major-mode=%s)" name action (selected-window) major-mode)

  ;; TODO: should really check for certain built-in action functions.
  ;; Looks like it would make sense to test for
  ;; display-buffer-use-some-window and display-buffer-reuse-window at
  ;; the very least...
  ;;
  ;; TODO: should it just always return 't??
  (when (or (null action)
	    (null (car action)))
    ;; always give the mmd function a go.
    't))

(defun mmd-custom-display-buffer (buffer alist)
  (mmd-message "mmd-custom-display-buffer: selected-window=%s buffer major-mode=%s" (selected-window) (mmd-major-mode-of-buffer buffer))
  (let ((w (mmd-find-window-for-buffer buffer)))
    (when w
      (set-window-buffer w buffer)
      w)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mmd-show-major-mode-window-dedication ()
  "Shows a the selected window's major mode dedication (if any) as a message."
  (interactive)
  (let* ((w (selected-window))
	 (modes (window-parameter w mmd-major-modes)))
    (if modes
	(message (format "MMD major modes: %s" modes))
      (message "No MMD major modes for this window."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mmd-add-major-mode-window-dedication (mode
					     window)
  (let ((modes (window-parameter window mmd-major-modes)))
    (when (not (member mode modes))
      (setq modes (cons mode modes)))
    (set-window-parameter window mmd-major-modes modes)))

(defun mmd-remove-major-mode-window-dedication (mode
						window)
  (set-window-parameter window
			mmd-major-modes
			(remove mode
				(window-parameter window
						  mmd-major-modes))))

(defun mmd-set-major-mode-window-dedication (arg)
  "Sets the selected window's major mode dedication.

The major mode for the current buffer is added to (if invoked
without a prefix arg) or removed from (if invoked with a prefix
arg) the list of major modes to which the selected window is
dedicated."
  (interactive "P")
  (funcall (if (null arg)
	       'mmd-add-major-mode-window-dedication
	     'mmd-remove-major-mode-window-dedication)
	   major-mode
	   (selected-window))
  (mmd-show-major-mode-window-dedication))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mmd-reset-major-mode-window-dedication ()
  "Resets the selected window's major mode dedication, if any. If
there is no major mode dedication, this function does nothing."
  (interactive "P")
  (set-window-parameter (selected-window) mmd-major-modes nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mmd-set-major-mode-window-dedication-by-name ()
  "Sets the selected window's major mode dedication."

  (interactive)
  (let* (mode-names)
    (mapatoms
     (lambda (x)
       ;; Deciding whether a name is that of a major mode involves a
       ;; bit of guesswork...
       (when (and (fboundp x)
		  (commandp x)
		  (not (subrp (symbol-function x))))
	 (let ((name (symbol-name x)))
	   (when (and (string-suffix-p "-mode" name)
		      (not (string-suffix-p "-minor-mode" name)))
	     (add-to-list 'mode-names name))))))
    (let* ((mode (ido-completing-read "Mode: " mode-names)))
      (when mode
	(mmd-add-major-mode-window-dedication (intern mode)
					      (selected-window))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar mmd-basic-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "d") 'mmd-set-major-mode-window-dedication)
    (define-key map (kbd "s") 'mmd-show-major-mode-window-dedication)
    (define-key map (kbd "n") 'mmd-set-major-mode-window-dedication-by-name)
    map))

(defun mmd-update-alist (alist-name key value)
  (unless (assoc key (symbol-value alist-name))
    (add-to-list alist-name (cons key value))))

(defun mmd-install ()
  (interactive)
  (mmd-update-alist 'window-persistent-parameters 'mmd-major-modes 't)
  (mmd-update-alist 'display-buffer-alist 'mmd-use-custom-display-buffer (cons 'mmd-custom-display-buffer nil))

  (unless (keymapp (lookup-key global-map (kbd "C-x D")))
    (define-key global-map (kbd "C-x D") mmd-basic-map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'major-mode-dedication)
