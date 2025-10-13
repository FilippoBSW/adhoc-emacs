;;; -*- lexical-binding: t; coding: utf-8 -*-

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install t)
  :config
  (setq treesit-auto-langs (remove 'cmake treesit-auto-langs))
  (global-treesit-auto-mode 1))

(use-package cargo-mode :ensure t :defer t)
(use-package clang-format :ensure t :defer t)
(use-package cmake-mode :ensure t :defer t)
(use-package go-mode :ensure t :defer t)
(use-package haskell-mode :ensure t :defer t)
(use-package json-mode :ensure t :defer t)
(use-package markdown-mode :ensure t :defer t)
(use-package rust-mode :ensure t :defer t)
(use-package swift-mode :ensure t :defer t)
(use-package yaml-mode :ensure t :defer t)
(use-package zig-mode :ensure t :defer t)

(provide 'adh-prog-modes)
