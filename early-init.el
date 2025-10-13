;;; -*- lexical-binding: t; coding: utf-8 -*-

(add-to-list 'custom-theme-load-path (locate-user-emacs-file "themes/gruber-material-dark"))
(load-theme 'gruber-material-dark-intense :no-confirm)

(blink-cursor-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(tab-bar-mode 0)
(tool-bar-mode 0)

(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(defun display-startup-echo-area-message ()
  (message ""))
