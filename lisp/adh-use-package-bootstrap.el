;;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(require 'use-package)
(setq use-package-verbose t)
(setq native-comp-async-report-warnings-errors 'silent)

(provide 'adh-use-package-bootstrap)
