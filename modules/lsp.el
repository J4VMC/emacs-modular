;;; lsp.el -- Settings for LSP -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file configures `lsp-mode` (Language Server Protocol).
;;
;; What is LSP? 🧐
;; It's a system that lets Emacs (and other editors) talk to a
;; "language server" for a specific programming language (e.g.,
;; `pyright` for Python, `gopls` for Go).
;;
;; The server does all the "smart" work (like finding definitions,
;; checking for errors, and providing autocomplete) and sends the
;; results to Emacs.
;;
;; This gives us modern IDE features like:
;; - Go to Definition (`M-.`)
;; - Find References (`M-?`)
;; - Smart, context-aware autocompletion
;; - Error highlighting (diagnostics)
;; - Documentation on hover
;;
;; This file is the "central hub" for all of this.

;;; Code:

;;; ----------------------------------------------------------------------
;;; LSP Mode Core
;;; ----------------------------------------------------------------------
;;
;; This is the main package and its core configuration.
;; It's a *very* large configuration block!

(use-package lsp-mode
  ;; Shorten the "Lsp" text in the modeline (status bar) to just "LSP"
  :diminish "LSP"
  :ensure t
  :hook (;; When lsp-mode starts, also turn on error underlining
         (lsp-mode . lsp-diagnostics-mode)
         ;; Integrate with `which-key` to show keybindings
         (lsp-mode . lsp-enable-which-key-integration))
  :custom
  ;; --- Basic Settings ---

  ;; Set the main key prefix for all LSP commands to `C-c l`
  ;; (e.g., `C-c l r r` to rename a symbol).
  (lsp-keymap-prefix "C-c l")

  ;; **CRITICAL**: Set completion provider to `:none`.
  ;; This tells lsp-mode *NOT* to handle the *UI* for autocompletion.
  ;; We do this because we'll use a separate, dedicated package
  ;; (like `corfu` or `company`) to *display* the completions
  ;; that lsp-mode *provides*.
  (lsp-completion-provider :none)

  ;; Tell LSP to use `flycheck` (a popular syntax checker) to *display*
  ;; the errors and warnings it finds.
  (lsp-diagnostics-provider :flycheck)

  ;; Where to save LSP session information (helps restart faster).
  (lsp-session-file (locate-user-emacs-file ".lsp-session"))

  ;; **PERFORMANCE**: Don't log all the JSON messages between Emacs
  ;; and the server. Set this to `t` *only* if you are debugging lsp-mode.
  (lsp-log-io nil)
  (lsp-keep-workspace-alive nil)
  ;; How long (in seconds) to wait after you stop typing before
  ;; sending requests (like error checks) to the server.
  (lsp-idle-delay 0.5)

  ;; --- Core Features (Enable/Disable) ---

  (lsp-enable-xref t)           ; YES, enable this. Powers "go to definition".
  (lsp-auto-configure t)        ; Auto-detect and set up language servers.
  (lsp-eldoc-enable-hover t)    ; Show documentation in the minibuffer on hover.
  (lsp-enable-dap-auto-configure t) ; Help `dap-mode` (debugger) find settings.

  ;; **PERFORMANCE**: Disable built-in file watching. This can be
  ;; resource-intensive, and we might use a separate system.
  (lsp-enable-file-watchers nil)

  (lsp-enable-folding t)        ; Allow code folding (hiding function bodies).
  (lsp-enable-imenu t)          ; Let LSP power the `imenu` (list functions/classes).
  (lsp-enable-indentation t)    ; Let LSP handle indentation (can be overridden).
  (lsp-enable-links t)          ; Make links in documentation clickable.
  (lsp-enable-on-type-formatting t) ; Auto-format as you type (e.g., add '}')
  (lsp-enable-suggest-server-download t) ; Ask to auto-install missing servers.
  (lsp-enable-symbol-highlighting t) ; Highlight other uses of the symbol at point.
  (lsp-enable-text-document-color nil) ; Turn off backgrounding for color codes (e.g., `#FFF`).

  ;; --- UI Settings ---

  ;; Don't show documentation in the "sideline" (the left margin).
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-diagnostic-max-lines 20)

  ;; --- Completion Settings ---

  (lsp-completion-enable t)     ; YES, let LSP *provide* completions.
  (lsp-completion-enable-additional-text-edit t)
  ;; **PERFORMANCE**: Disable LSP-based snippets. We'll use a
  ;; dedicated snippet manager (like `yasnippet`) for this.
  (lsp-enable-snippet nil)
  (lsp-completion-show-kind t)  ; Show icons (function, class, etc.) in completion UI.

  ;; --- Headerline (Breadcrumbs) ---

  ;; Show a "breadcrumb" at the top of the buffer, e.g.,
  ;; `project > module > class > function`
  (lsp-headerline-breadcrumb-enable t)
  (lsp-headerline-breadcrumb-enable-diagnostics nil)
  (lsp-headerline-breadcrumb-enable-symbol-numbers nil)
  (lsp-headerline-breadcrumb-icons-enable nil)

  ;; --- Modeline (Status Bar) ---
  ;; **PERFORMANCE**: We disable most modeline indicators to keep
  ;; Emacs fast and the status bar clean.
  (lsp-modeline-code-actions-enable nil)  ; Don't show a 💡 lightbulb icon.
  (lsp-modeline-diagnostics-enable nil)   ; Don't show error counts.
  (lsp-modeline-workspace-status-enable nil)

  ;; --- Documentation Pop-ups ---

  (lsp-signature-doc-lines 1)   ; Max lines for function signature help.
  ;; **UI**: Use a "childframe" (a true floating pop-up) for docs.
  (lsp-ui-doc-use-childframe t)
  (lsp-eldoc-render-all nil)    ; Show docs *only* for the symbol at point.

  ;; --- Code Lens ---

  ;; **PERFORMANCE**: Disable Code Lens. These are the "X references"
  ;; or "Run Test" links that appear *inside* your code. Can be noisy.
  (lsp-lens-enable nil)

  ;; --- Semantic Tokens ---

  ;; **PERFORMANCE**: Disable semantic tokens. This is an advanced
  ;; highlighting system. We are using Tree-sitter (from `tree.el`)
  ;; for highlighting, so we don't need this.
  (lsp-semantic-tokens-enable nil)

  :init
  ;; This was also in `early-init.el`. We set it here again to
  ;; ensure it's active. This enables a *much faster* data format
  ;; for `lsp-mode` to use. ⚡
  (setq lsp-use-plists t))

;;; ----------------------------------------------------------------------
;;; Language-Specific Overrides
;;; ----------------------------------------------------------------------
;;
;; For JavaScript and TypeScript, we want to let `prettier` handle
;; all formatting, not LSP. These hooks turn *off* LSP's indentation
;; and on-type-formatting features for those languages so they don't fight.

(add-hook 'typescript-ts-mode-hook
          (lambda ()
            (setq-local lsp-enable-indentation nil)
            (setq-local lsp-enable-on-type-formatting nil)))
(add-hook 'tsx-ts-mode-hook
          (lambda ()
            (setq-local lsp-enable-indentation nil)
            (setq-local lsp-enable-on-type-formatting nil)))
(add-hook 'js-ts-mode-hook
          (lambda ()
            (setq-local lsp-enable-indentation nil)
            (setq-local lsp-enable-on-type-formatting nil)))

;;; ----------------------------------------------------------------------
;;; LSP Completions
;;; ----------------------------------------------------------------------

;; This package provides the "glue" between `lsp-mode` (which
;; *provides* completions) and our completion UI (e.g., `corfu`).
(use-package lsp-completion
  :no-require
  :hook ((lsp-mode . lsp-completion-mode)))

;;; ----------------------------------------------------------------------
;;; LSP UI (Visual Elements)
;;; ----------------------------------------------------------------------
;;
;; This package provides all the "pretty" UI elements:
;; - Floating documentation pop-ups
;; - Sideline error display
;; - Code action lightbulbs (if enabled)

(use-package lsp-ui
  :ensure t
  ;; `:commands` tells `use-package` to lazy-load this.
  ;; It will only load when one of these commands is first called.
  :commands
  (lsp-ui-doc-show
   lsp-ui-doc-glance)
  ;; Bind `C-c C-d` to "glance" at the documentation (show it in a pop-up).
  :bind (:map lsp-mode-map
              ("C-c C-d" . 'lsp-ui-doc-glance))
  :after (lsp-mode)
  :config
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-show-with-cursor nil)
  (setq lsp-ui-doc-include-signature t)
  ;; Show documentation pop-ups *right at the cursor*, not in the margin.
  (setq lsp-ui-doc-position 'at-point))

;;; ----------------------------------------------------------------------
;;; LSP Integrations (Consult & Treemacs)
;;; ----------------------------------------------------------------------

;; Integrates LSP with `consult` (our fuzzy-finder).
;; This makes "find references" or "find workspace symbols"
;; use the nice `consult` UI.
(use-package consult-lsp
  :ensure t
  :after (consult lsp-mode))

;; Integrates LSP with `treemacs` (our file explorer).
;; This will show error counts and highlights in the file tree.
(use-package lsp-treemacs
  :ensure t
  :after (lsp-mode treemacs))

;;; ----------------------------------------------------------------------
;;; LSP Language Servers (Add-ons)
;;; ----------------------------------------------------------------------
;;
;; These are specific `use-package` blocks for language servers
;; that need extra configuration packages.

;; --- ESLint (JavaScript/TypeScript) ---
(use-package lsp-eslint
  :after lsp-mode
  :custom
  (lsp-eslint-auto-fix-on-save nil) ; We'll use a different formatter.
  (lsp-eslint-enable t)
  (lsp-eslint-package-manager "npm")
  (lsp-eslint-server-command '("vscode-eslint-language-server" "--stdio"))
  :config
  ;; Help ESLint understand our Tree-sitter major modes
  (add-to-list 'lsp-language-id-configuration '(typescript-ts-mode . "typescript"))
  (add-to-list 'lsp-language-id-configuration '(tsx-ts-mode . "typescriptreact"))
  (add-to-list 'lsp-language-id-configuration '(js-ts-mode . "javascript")))

;; --- TailwindCSS ---
(use-package lsp-tailwindcss
  :ensure t
  :init (setq lsp-tailwindcss-add-on-mode t)
  :config
  ;; Tell the Tailwind server which modes to activate in.
  (dolist (tw-major-mode
           '(css-mode
             css-ts-mode
             typescript-mode
             typescript-ts-mode
             tsx-ts-mode
             js2-mode
             js-ts-mode
             clojure-mode))
    (add-to-list 'lsp-tailwindcss-major-modes tw-major-mode)))

;; --- Scala (Metals) ---
(use-package lsp-metals
  :ensure t
  :hook (scala-ts-mode . (lambda ()
                           (require 'lsp-metals)
                           ;; `lsp-deferred` is KEY for performance.
                           ;; It tells Emacs: "Don't start the LSP
                           ;; server *immediately* when I open the file.
                           ;; Wait until I'm idle."
                           (lsp-deferred)))
  :config
  (setq lsp-metals-server-args '("-J-Dmetals.allow-multiline-string-formatting=off")
        lsp-metals-show-implicit-arguments t
        lsp-metals-show-implicit-conversions-and-classes t
        lsp-metals-show-inferred-type t))

;; --- Python (Pyright) ---
(use-package lsp-pyright
  :ensure t
  ;; Use `basedpyright`, a community fork, instead of the default.
  :custom (lsp-pyright-langserver-command "basedpyright")
  (lsp-pyright-python-executable-cmd "python3")
  :hook (python-ts-mode . (lambda ()
                            (require 'lsp-pyright)
                            ;; Use `lsp-deferred` for fast file loading.
                            (lsp-deferred))))

(defun set-pyright-paths ()
  "Set pyright extraPaths and dynamically set pythonVersion from pyenv."
  ;; Find the root of the current project (e.g., the git repo root)
  (let ((project-root (projectile-project-root))) ; <-- Use projectile-project-root
    (when project-root
      (let* (
             ;; Call `pyenv version-name` to get the active version
             (py-version-full (shell-command-to-string "pyenv version-name"))
             
             ;; Clean the output (e.g., "3.14.0\n" -> "3.14.0")
             (py-version-clean (trim-string py-version-full))
             
             ;; Split into parts ("3" "14" "0")
             (py-version-parts (split-string py-version-clean "\\."))
             
             ;; Re-join as "3.14" (or whatever the current version is)
             (py-version-major-minor (concat (nth 0 py-version-parts) "." (nth 1 py-version-parts))))
        
        ;; Set the config for lsp-pyright
        (setq lsp-pyright-workspace-config
              `(:python.analysis.extraPaths [,project-root]
					    ;; Pass the dynamically found version
					    :pythonVersion ,py-version-major-minor))))))

;; Run our helper function every time we open a Python file.
(add-hook 'python-mode-hook 'set-pyright-paths)

;; Run our helper function every time we open a Python file.
(add-hook 'python-mode-hook 'set-pyright-paths)

;; --- Rust (rust-analyzer) ---
(use-package lsp-rust
  :after lsp-mode
  :hook (rust-ts-mode . (lambda ()
                          (require 'lsp-rust)
                          (lsp-deferred)))
  :config
  ;; When checking code, use `clippy` (the full linter)
  ;; instead of just `check` (which is faster but less thorough).
  (setq lsp-rust-analyzer-cargo-watch-command "clippy"
        ;; --- Inlay Hints ---
        ;; These settings control "inlay hints," the little grey
        ;; text labels that rust-analyzer adds inside your code.
        lsp-rust-analyzer-server-display-inlay-hints t
        lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial"
        lsp-rust-analyzer-display-chaining-hints t
        lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil
        lsp-rust-analyzer-display-closure-return-type-hints t
        lsp-rust-analyzer-display-parameter-hints nil
        lsp-rust-analyzer-display-reborrow-hints nil))

;;; ----------------------------------------------------------------------
;;; LSP Language Servers (Built-in)
;;; ----------------------------------------------------------------------
;;
;; These languages (like Go and SQL) have their LSP support
;; configured "built-in" to `lsp-mode`, so they don't need
;; separate packages. We use `with-eval-after-load` to
;; configure them *after* `lsp-mode` has loaded.

;; --- Go (gopls) ---
(with-eval-after-load 'lsp-mode
  ;; Start the Go LSP server (gopls) in Go files.
  (add-hook 'go-ts-mode-hook #'lsp-deferred)

  ;; Go-specific settings
  (setq lsp-go-analyses '((fieldalignment . t)
                          (nilness . t)
                          (unusedwrite . t)
                          (unusedparams . t))
        ;; Use `gofumpt` (a stricter formatter) instead of `gofmt`.
        lsp-go-use-gofumpt t
        lsp-go-codelenses '((generate . t)
                            (test . t)
			    (tidy . t))))

;; --- SQL (sql-language-server) ---
(with-eval-after-load 'lsp-mode
  ;; Start the SQL LSP server in SQL files.
  (add-hook 'sql-ts-mode-hook #'lsp-deferred)

  ;; This is a "manual registration" because `lsp-mode` doesn't
  ;; know about `sql-language-server` by default.
  (lsp-register-client
   (make-lsp-client
    ;; Tell lsp-mode how to start the server.
    :new-connection (lsp-stdio-connection '("sql-language-server" "up" "--method" "stdio"))
    :major-modes '(sql-mode sql-ts-mode)
    :priority -1
    :server-id 'sql-ls))

  (setq lsp-sqls-workspace-config-path nil))


;;; ----------------------------------------------------------------------
;;; Finalize
;;; ----------------------------------------------------------------------

;; This is the standard "magic line" for an Emacs Lisp module.
;; It tells Emacs that the file `lsp.el` has been loaded successfully,
;; which allows `(require 'lsp)` in your `init.el` to work.
(provide 'lsp)

;;; lsp.el ends here.
