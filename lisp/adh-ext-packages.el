;;; -*- lexical-binding: t; coding: utf-8 -*-

(defun adh-mc-or-meow-quit ()
  (interactive)
  (if (bound-and-true-p multiple-cursors-mode)
      (mc/keyboard-quit)
    (adh-keyboard-quit-dwim)))

(use-package zoxide
  :ensure t
  :hook
  ((find-file dired-file) . zoxide-add))

(use-package multiple-cursors
  :ensure t :demand t
  :custom
  (mc/always-run-for-all t)
  :config
  (put 'adh-mc-or-meow-quit 'mc/cmds-run-once t))

(use-package avy
  :ensure t
  :custom
  (avy-background t)
  (avy-keys '(?n ?r ?t ?s ?g ?y ?h ?a ?e ?i ?l ?d ?c ?f ?o ?u)))

(use-package yasnippet
  :ensure t :defer t
  :commands (yas-minor-mode yas-global-mode)
  :config
  (yas-reload-all))

(use-package snap-indent
  :ensure t
  :config
  (adh--rename-mode 'snap-indent-mode " snap")
  :hook
  ((text-mode prog-mode) . snap-indent-mode))

(use-package visual-regexp
  :ensure t :defer t
  :custom
  (vr/default-regexp-modifiers '(:I t :M t :S nil)))

(use-package visual-regexp-steroids
  :ensure t :defer t :after visual-regexp)

(use-package vundo
  :ensure t :defer t
  :custom
  (vundo-glyph-alist vundo-unicode-symbols))

(use-package wgrep
  :ensure t :defer t
  :custom
  (wgrep-auto-save-buffer t))

(use-package windower
  :ensure t :defer t)

(use-package rainbow-mode
  :ensure t :defer t
  :custom
  (rainbow-x-colors nil))

(use-package sudo-edit
  :ensure t :defer t)

(provide 'adh-ext-packages)
