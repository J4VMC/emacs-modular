;;; editor.el -- General editor settings and behaviors -*- lexical-binding: t; -*-

;;; Commentary:
;; This file configures the core editing experience in Emacs. It sets up
;; fundamental behaviors like line numbering, line wrapping, and automatic
;; reloading of files. It also loads packages that enhance visual feedback
;; and text manipulation, such as parenthesis matching, indent guides,
;; and semantic highlighting with Prism.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --- Core Editor Behavior ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Automatically add a trailing empty line to all files.
(setq require-final-newline t)

;; Don't ask for confirmation when killing active processes on exit
(setq confirm-kill-processes nil)

;; Use common C-c/C-v/C-x keystrokes for copy/paste/cut.
(cua-mode 1)

;; kill buffer on exit
(setq message-kill-buffer-on-exit t)

;; Automatically reread files from disk if they change.
(setq-default auto-revert-avoid-polling t)
(setq auto-revert-interval 5)
(setq auto-revert-check-vc-info t)
(global-auto-revert-mode 1)

;; A modern convention: disable the archaic double-space after a period.
(setq-default sentence-end-double-space nil)

;; Disable backup files (e.g., file.txt~).
(setq make-backup-files nil)

;; Automatically delete selected text when you start typing.
(delete-selection-mode 1)

;; Default to UTF-8 for file encoding.
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

;; History and Session Persistence
(use-package savehist
  :ensure nil
  :init (savehist-mode 1)
  :config (setq history-length 1000))

(use-package saveplace
  :ensure nil
  :init (save-place-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --- Editor Enhancements ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Visual flair for numbers and operators.2
(use-package highlight-numbers
  :ensure t
  :hook (prog-mode . highlight-numbers-mode))

(use-package highlight-operators
  :ensure t
  :hook (prog-mode . highlight-operators-mode))

;; Display line numbers in programming modes.
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(setq-default display-line-numbers-width 3)

;; In text modes, wrap long lines visually for better readability.
(add-hook 'text-mode-hook #'visual-line-mode)

;; Highlight the current line to improve focus in common modes.
(use-package hl-line
  :ensure nil ;; It's built-in
  :hook ((prog-mode . hl-line-mode)
         (text-mode . hl-line-mode)
         (markdown-mode . hl-line-mode)))

;; Automatically match and insert pairs of parentheses, quotes, etc.
(use-package smartparens
  :ensure t
  :hook ((prog-mode . smartparens-mode)
         (text-mode . smartparens-mode)
         (markdown-mode . smartparens-mode))
  :config
  ;; The require line needs to be wrapped
  (with-eval-after-load 'org
    (require 'smartparens-config)))

;; Highlight todos
(use-package hl-todo
  :ensure t
  :init (global-hl-todo-mode))

;; Crux bundles many useful interactive commands to enhance the overall Emacs experience
(use-package crux
  :ensure t)

;; Hydra for easier-to-remember keybindings
(use-package hydra
  :ensure t)

;; aggressive-indent-mode is a minor mode that keeps your code always indented. It reindents after every change
(use-package aggressive-indent
  :ensure t
  :custom
  (global-aggressive-indent-mode 1))

(use-package whole-line-or-region
  :ensure t
  :config (whole-line-or-region-global-mode t))

(use-package page-break-lines
  :ensure t
  :config (global-page-break-lines-mode))

(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C-M-j") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
  (global-set-key (kbd "C-M-=") 'mc/mark-all-symbols-like-this))

;; Expand region with treesit support
(use-package expreg
  :ensure t
  :config (global-set-key (kbd "M-J") 'expreg-expand))

;; visual replace provides a nicer interface to Query-Replace on Emacs than the built-in one.
(use-package visual-replace
  :defer t)

;; Give matching delimiters (parens, brackets) different colors.
(use-package rainbow-delimiters
  :ensure t
  :hook ((prog-mode . rainbow-delimiters-mode)
         (text-mode . rainbow-delimiters-mode)
         (markdown-mode . rainbow-delimiters-mode)))

;; Display vertical lines to visualize indentation levels.
(use-package highlight-indent-guides
  :ensure t
  :hook (prog-mode . highlight-indent-guides-mode))

;; Enable moving lines and regions up and down with M-up/M-down.
(use-package drag-stuff
  :ensure t
  :config
  (drag-stuff-global-mode 1)
  (drag-stuff-define-keys))

;; Avy: Jump to any visible text in a few keystrokes.
(use-package avy
  :ensure t
  :bind (("C-c j" . avy-goto-line)
         ("s-j"   . avy-goto-char-timer)))

;; rg.el & wgrep-rg: Editable Ripgrep Searches for project-wide refactoring.
(use-package rg
  :ensure t
  :after transient
  :config
  (setq wgrep-auto-save-buffer t)
  (add-hook 'rg-mode-hook 'wgrep-rg-setup))

;; Undo history improved
(use-package vundo
  :ensure t
  :init (setq vundo-glyph-alist vundo-unicode-symbols))

;; Clean whitespaces
(use-package whitespace-cleanup-mode
  :ensure (:host github :repo "purcell/whitespace-cleanup-mode")
  :hook (prog-mode . whitespace-cleanup-mode)
  :config (setq whitespace-cleanup-mode-preserve-point t))

;; Better search
(use-package ctrlf
  :ensure t
  :config
  ;; Standard CTRLF configuration
  (define-key
   ctrlf-minibuffer-mode-map (kbd "C-r") 'ctrlf-backward-default)
  (setq ctrlf-default-search-style 'literal)

  ;; Add a hook to automatically cancel CTRLF search when leaving the minibuffer
  (defun my/auto-cancel-ctrlf ()
    "Automatically cancel CTRLF search when leaving the minibuffer."
    (when (and (bound-and-true-p ctrlf--active-p)
               (not (minibufferp))
               (not (eq (current-buffer) ctrlf--minibuffer)))
      (ctrlf-cancel)))

  (add-hook 'post-command-hook #'my/auto-cancel-ctrlf)

  ;; Enable CTRLF globally
  (ctrlf-mode t))

;; A modern list API for Emacs. No 'cl required.
(use-package dash
  :ensure t)

;; A string manipulation library.
(use-package s
  :ensure t)

(use-package evil-nerd-commenter
  :ensure t
  :config
  (evilnc-default-hotkeys t))

(provide 'editor)
;;; editor.el ends here
