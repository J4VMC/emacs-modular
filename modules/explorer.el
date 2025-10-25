;;; explorer.el -- File-specific configurations -*- lexical-binding: t; -*-
;;; Commentary:
;; This file configures modes that enhance the user interface, focusing on
;; file management with Dired and aesthetic enhancements with icons.
;; It does not handle the core completion system or language-specific major modes.
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --- File Management (Dired) ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package dired
  :ensure nil
  :commands (dired)
  :hook
  ((dired-mode . dired-hide-details-mode)
   (dired-mode . hl-line-mode))
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t))

(use-package dirvish
  :ensure t
  :after dired
  :config
  ;; This ensures dirvish takes over dired BEFORE dired is ever opened.
  (dirvish-override-dired-mode t))

(use-package oil
  :ensure (:host github :repo "yibie/Oil.el"))

(provide 'explorer)
;;; explorer.el ends here
