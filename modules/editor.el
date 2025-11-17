;;; editor.el -- General editor settings and behaviors -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file configures the "core" text editing experience. ✍️
;;
;; It's not about specific languages, but about the *act* of editing
;; text itself. This includes:
;;
;; 1.  **Core Behaviors**: How files are saved, loaded, and handled.
;;     (e.g., auto-reloading, UTF-8, no backup files).
;; 2.  **Session & History**: Remembering your minibuffer history and
;;     cursor position between sessions.
;; 3.  **Visual Aids**: Things that help you *see* your code better.
;;     (e.g., line numbers, word wrap, indent guides, parenthesis matching).
;; 4.  **Editing Enhancements**: Tools that add new "verbs" to your
;;     editing, making it faster.
;;     (e.g., multiple cursors, move-line-up/down, `avy` (quick-jump),
;;     easy commenting, and a better undo system).
;;
;; 'lexical-binding: t' is a "magic cookie" that tells Emacs to use a
;; more modern and predictable way of handling variables (lexical scoping).

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --- Core Editor Behavior ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A good POSIX practice: ensure all text files end with a single empty newline.
(setq require-final-newline t)

;; Don't ask for confirmation when quitting Emacs, even if a
;; sub-process (like an LSP server or a terminal) is still running.
(setq confirm-kill-processes nil)

;; Enable CUA (Common User Access) mode.
;; This makes `C-x` (cut), `C-c` (copy), and `C-v` (paste) work
;; just like they do in every other application.
(cua-mode 1)

;; This is not a standard Emacs variable. Its effect is likely
;; specific to a custom setup or a typo.
(setq message-kill-buffer-on-exit t)

;; --- Auto-revert Mode ---
;; Automatically reload any file that has been changed on disk
;; (e.g., by an external program like `git checkout`).
(setq-default auto-revert-avoid-polling t)
(setq auto-revert-interval 5)
(setq auto-revert-check-vc-info t)
(global-auto-revert-mode 1)

;; A modern convention: disable the archaic double-space after a period.
(setq-default sentence-end-double-space nil)

;; Don't create backup files (e.g., "file.txt~") on save.
;; We use Git for version control, so these are just clutter.
(setq make-backup-files nil)

;; Enable "delete selection" mode.
;; This makes Emacs behave like other editors: if you select a
;; region of text and start typing, it deletes the selection
;; and inserts what you typed.
(delete-selection-mode 1)

;; --- Encoding ---
;; Default to UTF-8 for all file operations. This is the modern
;; standard and ensures special characters display correctly.
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

;; --- History and Session Persistence ---

;; Remember minibuffer history (e.g., previous `M-x` commands)
;; between Emacs sessions.
(use-package savehist
  :ensure nil ; Built-in
  :init (savehist-mode 1)
  :config (setq history-length 1000)) ; Remember the last 1000 items

;; Remember the cursor position (and other state) of files you
;; open. When you re-open a file, your cursor will be where you
;; left it.
(use-package saveplace
  :ensure nil ; Built-in
  :init (save-place-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --- Editor Enhancements ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Display line numbers in programming modes.
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(setq-default display-line-numbers-width 3) ; Set width for the number column

;; In text modes (like plain .txt or Markdown), enable "word wrap"
;; (it's called visual-line-mode in Emacs).
(add-hook 'text-mode-hook #'visual-line-mode)

;; Highlight the current line to improve focus.
(use-package hl-line
  :ensure nil ;; It's built-in
  :hook ((prog-mode . hl-line-mode)
         (text-mode . hl-line-mode)
         (markdown-mode . hl-line-mode)))

;; This is the built-in "electric-pair" mode.
(use-package elec-pair
  :config
  ;; Define a custom function to disable auto-pairing in the minibuffer.
  ;; We don't want `C-x C-f` to auto-pair parens in the file path.
  (defun my/inhibit-electric-pair-mode (char)
    (or (minibufferp) (electric-pair-conservative-inhibit char)))
  (setq electric-pair-inhibit-predicate
        #'my/inhibit-electric-pair-mode)
  ;; Turn on the mode globally.
  (electric-pair-mode t)

  ;; This is a custom rule: it prevents auto-pairing for `<` and `>`
  ;; (which can be annoying in HTML/XML) and `~`.
  (setq electric-pair-inhibit-predicate
        (lambda (c)
          (or (member c '(?< ?> ?~))
              (electric-pair-default-inhibit c)))))

;; This explicitly *limits* the auto-pairing to just curly braces `{}`.
;; This overrides the default, which also includes `()`, `[]`, etc.
;; This is a very minimal and specific choice.
(setq electric-pair-pairs '((?\{ . ?\})))

;; Light-weight yet powerful package for manual manipulation of brackets
;; and related characters.
(use-package supreme-brackets
  :ensure (:host github :repo "J4VMC/supreme-brackets")
  :config
  (supreme-brackets-setup-extended-keybindings))

;; Highlight special words like "TODO", "FIXME", and "NOTE" in comments.
(use-package hl-todo
  :ensure t
  :init (global-hl-todo-mode))

;; `crux` bundles many useful interactive commands that feel
;; "missing" from Emacs (e.g., `crux-rename-file-and-buffer`).
(use-package crux
  :ensure t)

;; `hydra` is a powerful tool for creating pop-up keybinding
;; menus (like for `dap-mode`), making complex key sequences easier.
(use-package hydra
  :ensure t)

;; This package makes commands like `kill` (cut) and `copy`
;; apply to the *entire line* if no region (selection) is active.
(use-package whole-line-or-region
  :ensure t
  :config (whole-line-or-region-global-mode t))

;; This package displays `^L` (form feed / page break) characters
;; as a nice-looking horizontal line, which is great for
;; separating sections in code or org-mode.
(use-package page-break-lines
  :ensure t
  :config (global-page-break-lines-mode))

;; Enables "VS Code style" multiple cursors.
(use-package multiple-cursors
  :ensure t
  :config
  ;; `C-M-j` (`Ctrl-Alt-j`) to add a cursor on the line below/above.
  (global-set-key (kbd "C-M-j") 'mc/edit-lines)
  ;; `C->` to add a cursor at the next occurrence of the current word.
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  ;; `C-<` to add a cursor at the previous occurrence.
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  ;; `C-c C-<` to add cursors at *all* occurrences.
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
  (global-set-key (kbd "C-M-=") 'mc/mark-all-symbols-like-this))

;; "Expand region" (selection) semantically using Tree-sitter.
;; Pressing `M-J` repeatedly will expand the selection from the
;; current word -> string -> function argument -> function body -> whole function.
(use-package expreg
  :ensure t
  :config (global-set-key (kbd "M-J") 'expreg-expand))

;; `visual-replace` provides a nicer, more visual interface
;; for `query-replace` (find and replace).
(use-package visual-replace
  :defer t) ; Load it when first used

;; Give matching delimiters (like `()`, `[]`, `{}`) different
;; colors, making it easy to see which ones match in nested code.
(use-package rainbow-delimiters
  :ensure t
  :hook ((prog-mode . rainbow-delimiters-mode)
         (text-mode . rainbow-delimiters-mode)
         (markdown-mode . rainbow-delimiters-mode)))

;; Display vertical lines to visualize indentation levels.
;; This helps line up code blocks in Python, YAML, etc.
(use-package highlight-indent-guides
  :ensure t
  :hook (prog-mode . highlight-indent-guides-mode))

;; Enable moving lines and regions up and down with `M-up` and `M-down`.
(use-package drag-stuff
  :ensure t
  :config
  (drag-stuff-global-mode 1)
  (drag-stuff-define-keys))

;; `avy` is a "jump" package. You press a key, type a character,
;; and it shows you "hints" to jump instantly to any visible
;; place that character appears.
(use-package avy
  :ensure t
  :bind (;; `C-c j` to jump to any visible *line*.
         ("C-c j" . avy-goto-line)
         ;; `s-j` (Super-j) to jump to any visible *character*.
         ("s-j"   . avy-goto-char-timer)))

;; `rg.el` provides a frontend for `ripgrep` (a very fast search tool).
;; `wgrep-rg` makes the search results *editable*, so you can
;; perform project-wide find-and-replace.
(use-package rg
  :ensure t
  :after transient
  :config
  (setq wgrep-auto-save-buffer t)
  (add-hook 'rg-mode-hook 'wgrep-rg-setup))

;; A better, *visual* undo system.
;; Instead of mashing `C-/` and wondering where you are, this
;; shows you a "tree" of all your changes so you can jump to
;; any past state.
(use-package vundo
  :ensure t
  :init (setq vundo-glyph-alist vundo-unicode-symbols))

;; Automatically removes trailing whitespace from lines on save.
(use-package whitespace-cleanup-mode
  :ensure (:host github :repo "purcell/whitespace-cleanup-mode")
  :hook (prog-mode . whitespace-cleanup-mode)
  :config (setq whitespace-cleanup-mode-preserve-point t))

;; A modern, "VS Code style" search-in-buffer (like `C-f`).
;; It's non-blocking and highlights matches as you type.
(use-package ctrlf
  :ensure t
  :config
  ;; In the search box, make `C-r` search backward.
  (define-key
   ctrlf-minibuffer-mode-map (kbd "C-r") 'ctrlf-backward-default)
  ;; Default to a literal search (not regex).
  (setq ctrlf-default-search-style 'literal)

  ;; A helper function to automatically *cancel* the search
  ;; if you click or move your cursor outside the minibuffer.
  (defun my/auto-cancel-ctrlf ()
    "Automatically cancel CTRLF search when leaving the minibUffer."
    (when (and (bound-and-true-p ctrlf--active-p)
	       (not (minibufferp))
	       (not (eq (current-buffer) ctrlf--minibuffer)))
      (ctrlf-cancel)))

  (add-hook 'post-command-hook #'my/auto-cancel-ctrlf)

  ;; Enable `ctrlf` globally.
  (ctrlf-mode t))

;; `dash` is a powerful list manipulation library.
;; It's a dependency for many modern packages.
(use-package dash
  :ensure t)

;; `s` is a powerful string manipulation library.
;; Also a common dependency.
(use-package s
  :ensure t)

;; A very popular package for quickly commenting/uncommenting lines
;; or regions of code (e.g., with `M-/`).
(use-package evil-nerd-commenter
  :ensure t
  :config
  ;; This sets up the default keybindings for the package.
  (evilnc-default-hotkeys t))

;;; ----------------------------------------------------------------------
;;; Finalize
;;; ----------------------------------------------------------------------

;; This is the standard "magic line" for an Emacs Lisp module.
;; It tells Emacs that the file `editor.el` has been loaded successfully,
;; which allows `(require 'editor)` in your `init.el` to work.
(provide 'editor)

;;; editor.el ends here
