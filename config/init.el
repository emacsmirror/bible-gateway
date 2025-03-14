;;; init.el --- Load minimal init.el configuration -*- lexical-binding: t -*-

;;; Commentary:

;; This file bootstraps a minimal configuration for votd as footer in Emacs dashboard.

;;; Code:


;; Melpa needed to install Emacs dashboard
(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

;; Place any customize-based settings in custom.el.
(setq custom-file "~/.emacs.d/custom.el")


;; Install votd using :vc in use-package
(use-package votd
  :vc (:url "https://github.com/kristjoc/votd")) ; For Emacs>=30


;; Emacs dashboard configuration
(use-package dashboard
  :ensure t
  :init
  (dashboard-setup-startup-hook)
  :config 
  (setq dashboard-banner-logo-title "")
  (setq dashboard-footer-messages '(""))
  (setq dashboard-navigation-cycle t)
  (setq dashboard-show-shortcuts nil)
  (setq dashboard-center-content t)
  (setq dashboard-vertically-center-content t)
  (setq dashboard-items '((recents   . 3)
                          (projects  . 3)
                          (bookmarks . 3)))
  (setq dashboard-footer-icon "")
  ;; Set up the dashboard footer using votd
  (require 'votd)
  (setq dashboard-footer-messages (list (get-daily-verse-footer))))


(provide 'init)
;;; init.el ends here
