;;; completion.el -- Additional tools for completion utilities -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file configures the entire "completion" experience in Emacs. 🚀
;; "Completion" is Emacs's term for "autocompletion" and "fuzzy finding".
;;
;; This file is broken into two main parts:
;;
;; 1.  **Minibuffer Completion (The "Vertico Stack")**:
;;     This is for anything you do in the "command line" at the
;;     bottom of the screen (the minibuffer).
;;     - `M-x` (find command)
;;     - `C-x C-f` (find file)
;;     - `C-x b` (switch buffer)
;;     The main packages are: Vertico, Consult, Orderless, Marginalia, Embark.
;;
;; 2.  **In-Buffer Completion (The "Corfu Stack")**:
;;     This is the "autocomplete" pop-up that appears *in your code*
;;     as you are typing.
;;     The main packages are: Corfu, Cape, Tempel.
;;
;; These two "stacks" (Vertico and Corfu) are modern, minimal,
;; and work together perfectly to create a fast and powerful
;; completion system.

;;; Code:

;; First, some core Emacs completion behavior tweaks.
(use-package emacs
  :ensure nil ; `:ensure nil` means "don't install, this is built-in"
  :init
  ;; Allow running commands (like `consult-ripgrep`) *while*
  ;; you are already in the minibuffer. This is essential.
  (setq enable-recursive-minibuffers t)
  :custom
  ;; Start cycling through completions with TAB after 3 candidates.
  (completion-cycle-threshold 3)
  ;; Make TAB "smart": first, it tries to indent the current line.
  ;; If the line is already indented, it then tries to complete.
  (tab-always-indent 'complete)
  ;; For Emacs 30+, turn off a new built-in completion that
  ;; can conflict with our Corfu/Cape setup.
  (text-mode-ispell-word-completion nil)
  ;; Make M-x (`execute-extended-command`) show *all* commands,
  ;; not just a "helpful" subset.
  (read-extended-command-predicate #'command-completion-default-include-p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --- The Completion Suite (Vertico, Consult, Embark, etc.) ---
;; ---          (This is for the MINIBUFFER)                 ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Vertico: This is the *core* UI for the minibuffer.
;; It replaces the default, clunky completion with a clean,
;; fast, and modern *vertical* list. This is the "V" in "CVE".
(use-package vertico
  :ensure t
  :init
  (vertico-mode) ; Turn it on!
  :custom
  ;; `vertico-cycle`: Lets you press arrow-down at the bottom
  ;; of the list to wrap around to the top (and vice-versa).
  (vertico-cycle t))

;; A small add-on for Vertico to improve file navigation.
(use-package vertico-directory
  :ensure nil ; This is built-in with Vertico, just configuring
  :after vertico
  :bind (:map vertico-map
              ;; In `C-x C-f`, this lets you press `M-DEL` (Alt-Backspace)
              ;; to delete one part of the path (e.g., delete "bar/" from "foo/bar/").
              ("M-DEL" . vertico-directory-delete-word)))

;; Orderless: This is the "brains" of the filtering.
;; It lets you type search terms in *any order* ("out of order-less").
;; Example: `C-x b` and type "init comp" to find `completion.el` in your `init` dir.
(use-package orderless
  :ensure t
  :init
  ;; Set `orderless` as the *only* completion style. We don't
  ;; need the old ones anymore.
  (setq completion-styles '(orderless))
  (setq completion-category-defaults nil))

;; Marginalia: This provides the rich *annotations* (metadata)
;; you see on the right-hand side of the Vertico list.
;; - `M-x find-file` -> shows file permissions and size
;; - `M-x describe-function` -> shows the function's arguments
;; - `C-x b` -> shows the file path of the buffer
(use-package marginalia
  :ensure t
  :after vertico
  :init (marginalia-mode)) ; Turn it on!

;; Consult: Provides *better versions* of many built-in Emacs
;; commands, all of which use our new Vertico/Orderless setup.
(use-package consult
  :ensure t
  :after vertico
  :bind (;; `C-x b`: A *much* better `switch-to-buffer`.
         ("C-x b" . consult-buffer)
         ;; `M-y`: A fuzzy-searchable `yank-pop` (pasting from history).
         ("M-y" . consult-yank-pop)
         ;; `M-s r`: Project-wide search using `ripgrep` (rg). *Super* fast.
         ("M-s r" . consult-ripgrep)
         ;; `M-s l`: Fuzzy-search for a line *in the current buffer*.
         ("M-s l" . consult-line)
         ;; `M-s L`: Fuzzy-search for a line *in all open buffers*.
         ("M-s L" . consult-line-multi)
         ;; `M-s o`: Fuzzy-search for a headline (outline) in the current buffer.
         ("M-s o" . consult-outline))
  :config
  ;; These are the command-line arguments we pass to `ripgrep` (rg).
  ;; It's set to be smart-case, show line numbers, find hidden files,
  ;; and ignore the `.git` directory.
  (setq consult-ripgrep-args
        "rg --null --line-buffered --max-columns=150 --max-columns-preview --max-file-size 1M --ignore-case --path-separator /\\ --smart-case --no-heading --line-number --hidden --glob !.git/ .")
  ;; This lets you press `/` in a consult search to "narrow"
  ;; the results even further with a different search style.
  (setq consult-narrow-key "/"))

;; A small "glue" package to make Consult and Projectile work together.
(use-package consult-projectile
  :after (consult projectile)
  :ensure t)

;; Embark: This is the "actions" package. It answers the question:
;; "Okay, I've *found* the thing I'm looking for... now what?"
;;
;; With Embark, you can press a key (we bind it to `C-.`) on *any*
;; candidate in the minibuffer to get a pop-up of "verbs" (actions)
;; you can perform on that "noun" (the candidate).
;;
;; Example: In `consult-buffer` on "foo.el", press `C-.` to get a
;; list of actions like "Kill Buffer", "Rename File", "Show in Dired".
(use-package embark
  :ensure t
  :bind (;; `C-.` is the main "act" command.
         ("C-." . embark-act)
         ;; `C-;` tries to guess the most "obvious" action (Do What I Mean).
         ("C-;" . embark-dwim)
         ;; `C-h B` shows all Embark actions for the current *type* of thing.
         ("C-h B" . embark-bindings))
  :init
  ;; This integrates Embark with `which-key` so you get a
  ;; pop-up of available actions.
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; A small UI tweak to make Embark's "collect" buffers (which
  ;; show a list of all candidates) look cleaner by hiding their modeline.
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil (window-parameters (mode-line-format . none)))))

;; embark-consult: "Glue" package. This makes Embark's actions
;; (like "collect") use Consult's nice previewing features.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook
  ;; When you're in an "Embark Collect" buffer, enable live previews
  ;; as you move your cursor over the candidates.
  (embark-collect-mode . consult-preview-at-point-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --- Completion-at-Point (Corfu, Cape, Tempel) ---
;; --- (This is for the IN-BUFFER autocomplete pop-up) ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Corfu: This is the *core* UI for in-buffer completion.
;; It's a clean, fast, and minimal pop-up that shows completion
;; candidates as you type. This is the "Co" in "Corfu".
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode) ; Turn it on everywhere
  :custom
  (corfu-cycle t) ; Let navigation wrap around the list
  (corfu-auto t)  ; Show the pop-up automatically (don't wait for TAB)
  :bind (:map corfu-map
              ;; Make TAB and S-TAB (Shift-TAB) navigate the list.
              ;; This is a standard and very ergonomic setup.
              ("TAB" . corfu-next)
              ([tab] . corfu-next)
              ("S-TAB" . corfu-previous)
              ([backtab] . corfu-previous)))

;; A Corfu add-on to show documentation in a pop-up *next to* Corfu.
(use-package corfu-popupinfo
  :after corfu
  :ensure nil ; Part of Corfu, just configuring
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  ;; Set the delay before the info pop-up appears.
  (corfu-popupinfo-delay '(0.25 . 0.1))
  ;; Don't auto-hide the info pop-up.
  (corfu-popupinfo-hide nil)
  :config
  (corfu-popupinfo-mode))

;; Makes Corfu look good even when running Emacs in a terminal.
(use-package corfu-terminal
  :if (not (display-graphic-p)) ; Only load if *not* in a GUI.
  :ensure t
  :config
  (corfu-terminal-mode))

;; Cape: Provides *completion backends* (sources) for Corfu.
;; Corfu provides the *UI*, but Cape provides *what* to complete.
;; This is the "Ca" in "Cape".
(use-package cape
  :ensure t
  :init
  ;; `completion-at-point-functions` (or 'capf') is the official
  ;; Emacs variable for in-buffer completion sources.
  ;; We add Cape's backends to this list.
  (add-hook 'completion-at-point-functions #'cape-file)      ; Completes file paths
  (add-hook 'completion-at-point-functions #'cape-dabbrev)   ; Completes words from other buffers
  (add-hook 'completion-at-point-functions #'cape-elisp-block) ; Elisp-specific
  (add-hook 'completion-at-point-functions #'cape-keyword)   ; Completes programming keywords
  (add-hook 'completion-at-point-functions #'cape-dict)      ; Completes from a dictionary file
  :config
  ;; This helper function bundles several Cape backends into one
  ;; "super" function.
  (defun super-capf-cape ()
    (cape-wrap-super #'cape-dabbrev #'cape-dict #'cape-keyword))
  ;; We then set this "super" function as the main one.
  (setq-local completion-at-point-functions (list #'super-capf-cape)))

;; Tempel: This is our *snippet* (template) engine.
;; It lets you type a short word (e.g., "for") and expand it
;; into a full code block (e.g., a "for" loop).
(use-package tempel
  :ensure t
  :custom
  ;; Tell Tempel where to find *your* custom snippet file.
  (tempel-path (concat user-emacs-directory "custom_template_file"))
  :bind (;; `M-*` (Alt-*) inserts a snippet by name.
         ("M-*" . tempel-insert)
         ;; `M-+` (Alt-+) tries to complete the word at point as a snippet.
         ("M-+" . tempel-complete)
         ;; Define keys for *inside* an active snippet.
         :map tempel-map
         ("C-c RET" . tempel-done)   ; Finish snippet
         ("C-<down>" . tempel-next)  ; Go to next field
         ("C-<up>" . tempel-previous) ; Go to previous field
         ("M-<down>" . tempel-next)
         ("M-<up>" . tempel-previous))
  :init
  ;; --- The "Glue" that connects Tempel to Corfu ---
  ;; This function adds snippet expansion to Corfu's list.
  (defun tempel-setup-capf ()
    ;; This is the magic. It adds `tempel-expand` to the
    ;; list of completion functions. Now, your snippets
    ;; will appear *inside* the Corfu pop-up!
    (add-hook 'completion-at-point-functions #'tempel-expand -1 'local))
  ;; Run this "glue" function in all programming and text modes.
  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)
  :config
  ;; This mode tries to auto-expand snippets as you type (e.g., `for`+SPACE)
  (setq global-tempel-abbrev-mode 1))

;; This package provides a *pre-made collection* of snippets
;; for many common languages (Python, JS, Go, etc.).
;; It's a great starting point before you write your own.
(use-package tempel-collection
  :ensure t
  :after tempel)


;;; ----------------------------------------------------------------------
;;; Finalize
;;; ----------------------------------------------------------------------

;; This is the standard "magic line" for an Emacs Lisp module.
;; It tells Emacs that the file `completion.el` has been loaded successfully,
;; which allows `(require 'completion)` in your `init.el` to work.
(provide 'completion)

;;; completion.el ends here
