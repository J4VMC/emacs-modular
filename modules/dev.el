;;; dev.el -- All settings relevant for Software Development -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file configures all the essential tools for a modern
;; software development workflow. It includes:
;;
;; 1.  `magit`: The best Git client, period.  Git 💻
;; 2.  `restclient`: For testing APIs (like Postman, but in Emacs). 📡
;; 3.  `emmet-mode`: For expanding HTML/CSS abbreviations. ⚡
;; 4.  `apheleia`: Our "hands-off" auto-formatting system. 🎨
;; 5.  `flycheck`: Our on-the-fly syntax/error checker (red squiggles). 🐞
;;
;; We spend a lot of time here defining *custom* formatters and
;; checkers to integrate perfectly with our language setups.

;;; Code:

;;; ----------------------------------------------------------------------
;;; Version Control (Magit)
;;; ----------------------------------------------------------------------

;; Magit is a full-featured Git client inside Emacs. It's
;; interactive, fast, and considered a "killer feature" by many.
(use-package magit
  :ensure t
  ;; Magit depends on `transient` for its pop-up menus.
  ;; `:after transient` ensures transient is loaded first.
  :after transient
  ;; Bind the most common command, `magit-status`, to `C-x g`.
  ;; This is the main entry point to all Git operations.
  :bind ("C-x g" . magit-status))

;; `forge` is an extension for Magit that integrates with Git "forges"
;; like GitHub and GitLab. It lets you view, create, and manage
;; pull requests and issues directly from inside Magit/Emacs.
(use-package forge
  :after magit ; Load this *after* Magit is loaded
  :ensure t
  :config
  ;; Tell Forge where to find your API authentication credentials
  ;; (e.g., your GitHub personal access token). It will read
  ;; from the `~/.authinfo.gpg` file, which is a standard
  ;; Emacs location for sensitive credentials.
  (setq auth-sources '("~/.authinfo.gpg")))

;; `magit-todos` is a small, handy extension for Magit.
;; It adds a section to the `magit-status` buffer that shows
;; all the "TODO", "FIXME", and "NOTE" comments in your project.
(use-package magit-todos
  :after magit ; Load this *after* Magit
  :ensure t
  :config
  (magit-todos-mode 1) ; turn it on
  ;; Tell magit-todos to *not* search for TODOs in the "templates"
  ;; directory (e.g., your `tempel` snippets file), which can be noisy.
  (setq magit-todos-exclude-globs '("templates")))

;;; ----------------------------------------------------------------------
;;; API Testing (restclient)
;;; ----------------------------------------------------------------------

;; `restclient` lets you write and execute HTTP requests from a
;; plain-text file, similar to Postman or Insomnia.
;; You can define variables, chain requests, and more.
(use-package restclient
  :ensure t
  ;; Automatically use `restclient-mode` for files ending in `.http`.
  :mode ("\\.http\\'" . restclient-mode)
  :config
  ;; Load an extension that adds support for writing tests
  ;; (e.g., asserting a 200 OK status or a JSON value).
  (use-package restclient-test :ensure t))

;;; ----------------------------------------------------------------------
;;; Web Development (Emmet)
;;; ----------------------------------------------------------------------

;; Emmet provides a high-speed way to write HTML and CSS.
;; For example, `div#page>ul.nav>li*5` [TAB] expands to a
;; full HTML structure.
(use-package emmet-mode
  :ensure t
  ;; Enable Emmet automatically in `web-mode` (for HTML) and `css-mode`.
  :hook ((web-mode . emmet-mode)
         (css-mode . emmet-mode))
  :config
  ;; A specific tweak for React/JSX:
  ;; Make Emmet use `className="..._"` (React's way)
  ;; instead of `class="..._"` (HTML's way) when expanding in JSX files.
  (setq emmet-expand-jsx-className? t))

;;; ----------------------------------------------------------------------
;;; Auto-Formatting (Apheleia)
;;; ----------------------------------------------------------------------

;; Apheleia is our "set it and forget it" code formatter.
;; It runs an external tool (like `prettier`, `black`, `ruff`, `gofmt`)
;; to format your *entire buffer* automatically every time you save.
;;
;; This configuration has two main parts:
;; 1.  Define `apheleia-formatters`: The *commands* to run.
;; 2.  Define `apheleia-mode-alist`: *Which* formatter to use
;;     for *which* major mode.
(use-package apheleia
  :ensure t
  ;; Hide the "Aph" minor mode indicator from the modeline.
  ;; It's always on, so we don't need to see it.
  :diminish ""
  :config

  ;; --- 1. Define Custom Formatters ---
  ;; Apheleia comes with many formatters built-in (like `prettier`),
  ;; but we define our own here for more control or for tools
  ;; it doesn't know about.

  ;; PHP - PSR12 standard
  ;; We're defining a new formatter named `phpcs-psr12`
  (setf (alist-get 'phpcs-psr12 apheleia-formatters)
        ;; It runs `phpcbf` (PHP Code Beautifier and Fixer)
        '("phpcbf" "--standard=PSR12" "--stdin-path=" (or buffer-file-name "stdin")))

  ;; Python - Ruff
  ;; Defining a formatter named `ruff` to use its new `format` command.
  (setf (alist-get 'ruff apheleia-formatters)
        '("ruff" "format" "--stdin-filename" filepath "-"))

  ;; Go - goimports (formats and organizes imports)
  (setf (alist-get 'goimports apheleia-formatters)
        '("goimports"))

  ;; Rust - rustfmt
  (setf (alist-get 'rustfmt apheleia-formatters)
        '("rustfmt" "--emit=stdout"))

  ;; Scala - scalafmt
  (setf (alist-get 'scalafmt apheleia-formatters)
        '("scalafmt" "--stdin" "--stdout"))

  ;; SQL - sql-formatter
  (setf (alist-get 'sql-formatter apheleia-formatters)
        '("sql-formatter"
          "--language" "postgresql"
          "--indent" "2"
          "--uppercase")) ; Make keywords (SELECT, FROM) uppercase


  ;; --- 2. Mode Associations ---
  ;; This is where we map a major mode (like `python-ts-mode`)
  ;; to the formatter *name* we defined above (like `'ruff`).

  ;; PHP
  (setf (alist-get 'php-ts-mode apheleia-mode-alist) 'phpcs-psr12)

  ;; Python (we map both the old and new -ts-mode)
  (setf (alist-get 'python-mode apheleia-mode-alist) 'ruff)
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) 'ruff)

  ;; Go
  (setf (alist-get 'go-mode apheleia-mode-alist) 'goimports)
  (setf (alist-get 'go-ts-mode apheleia-mode-alist) 'goimports)

  ;; Rust
  (setf (alist-get 'rust-ts-mode apheleia-mode-alist) 'rustfmt)

  ;; Scala
  (setf (alist-get 'scala-ts-mode apheleia-mode-alist) 'scalafmt)

  ;; SQL
  (setf (alist-get 'sql-mode apheleia-mode-alist) 'sql-formatter)
  (setf (alist-get 'sql-ts-mode apheleia-mode-alist) 'sql-formatter)

  ;; JavaScript / TypeScript (uses built-in `prettier` formatters)
  (setf (alist-get 'typescript-ts-mode apheleia-mode-alist) 'prettier-typescript)
  (setf (alist-get 'tsx-ts-mode apheleia-mode-alist) 'prettier-typescript)
  (setf (alist-get 'js-ts-mode apheleia-mode-alist) 'prettier-javascript)
  (setf (alist-get 'typescript-mode apheleia-mode-alist) 'prettier-typescript)
  (setf (alist-get 'js-mode apheleia-mode-alist) 'prettier-javascript)
  (setf (alist-get 'js2-mode apheleia-mode-alist) 'prettier-javascript)

  ;; JSON (uses built-in `prettier` formatters)
  (setf (alist-get 'json-mode apheleia-mode-alist) 'prettier-json)
  (setf (alist-get 'json-ts-mode apheleia-mode-alist) 'prettier-json)

  ;; CSS (uses built-in `prettier` formatters)
  (setf (alist-get 'css-mode apheleia-mode-alist) 'prettier-css)
  (setf (alist-get 'css-ts-mode apheleia-mode-alist) 'prettier-css)

  ;; HTML (uses built-in `prettier` formatters)
  (setf (alist-get 'html-mode apheleia-mode-alist) 'prettier-html)
  (setf (alist-get 'web-mode apheleia-mode-alist) 'prettier-html)

  ;; YAML (uses built-in `prettier` formatters)
  (setf (alist-get 'yaml-mode apheleia-mode-alist) 'prettier-yaml)
  (setf (alist-get 'yaml-ts-mode apheleia-mode-alist) 'prettier-yaml)

  ;; Markdown (uses built-in `prettier` formatters)
  (setf (alist-get 'markdown-mode apheleia-mode-alist) 'prettier-markdown)
  (setf (alist-get 'gfm-mode apheleia-mode-alist) 'prettier-markdown)

  ;; --- 3. Enable globally ---
  ;; Turn on Apheleia everywhere. It will only be *active*
  ;; in buffers that have a formatter associated with them.
  (apheleia-global-mode t))


;;; ----------------------------------------------------------------------
;;; On-the-Fly Syntax Checking (Flycheck)
;;; ----------------------------------------------------------------------

;; Flycheck is the package that gives you the "red squiggly lines"
;; for syntax errors, style warnings, and linter issues.
;; It runs *as you type* (or save).
(use-package flycheck
  :ensure t
  ;; Turn on Flycheck everywhere, for all buffers.
  :init (global-flycheck-mode)
  ;; Add some convenient keybindings to jump between errors.
  :bind (:map flycheck-mode-map
              ;; `M-n` (Alt-n) -> go to *n*ext error
              ("M-n" . flycheck-next-error)
              ;; `M-p` (Alt-p) -> go to *p*revious error
              ("M-p" . flycheck-previous-error))
  :config
  ;; --- Custom Checker Definitions ---
  ;; Like Apheleia, we can define our own "checkers"
  ;; for tools Flycheck doesn't know about or to customize
  ;; how they are run.

  ;; Custom checker for TypeScript (`tsc`)
  (flycheck-define-checker typescript-tsc-syntax
    "A TypeScript syntax checker using tsc."
    :command ("tsc"
              "--noEmit"         ; Don't create JS files, just check
              "--pretty" "false" ; Use concise, parsable output
              source-inplace)
    :error-patterns ; RegEx to parse the error messages from `tsc`
    ((error line-start (file-name) "(" line "," column "): error TS"
            (message) line-end))
    :modes (typescript-ts-mode tsx-ts-mode)) ; When to use this checker

  ;; Custom checker for Python (`ruff`)
  (flycheck-define-checker python-ruff
    "A Python syntax and style checker using Ruff."
    :command ("ruff" "check"
              ;; We select a *specific* set of rules
              ;; (E=pycodestyle, F=pyflakes, D=pydocstyle, etc.)
              "--select" "E,F,W,D,UP,B,SIM,S"
              "--output-format" "concise"
              "--stdin-filename" source-inplace
              "-")
    :standard-input t ; Read from stdin instead of a file
    :error-patterns ; RegEx to parse Ruff's "concise" output
    ((error line-start (file-name) ":" line ":" column ": " (message) line-end))
    :modes (python-mode python-ts-mode))

  ;; Custom checker for SQL (`sqlint`)
  (flycheck-define-checker sql-sqlint
    "A SQL syntax checker using sqlint.
See URL `https://github.com/purcell/sqlint'."
    :command ("sqlint")
    :standard-input t
    :error-patterns ; RegEx to parse sqlint's output
    ((error line-start "stdin:" line ":" column ":ERROR " (message) line-end)
     (warning line-start "stdin:" line ":" column ":WARNING " (message) line-end))
    :modes (sql-mode sql-ts-mode))

  ;; --- Registering Checkers ---
  ;; Now we tell Flycheck to *use* our new checker.
  (add-to-list 'flycheck-checkers 'sql-sqlint)

  ;; Configure a built-in checker: `phpcs`
  ;; Tell it to use the PSR12 standard, to match our formatter.
  (setq flycheck-phpcs-standard "PSR12"))

;;; --- Flycheck "Chaining" and Add-ons ---
;;
;; By default, Flycheck runs *all* available checkers for a mode.
;; This can be noisy (e.g., LSP and Ruff both report "unused import").
;;
;; "Chaining" tells Flycheck: "Try Checker A (like LSP) first.
;; If it succeeds, *then* run Checker B (like Ruff)."
;; This combines the strengths of both: LSP for fast, real-time
;; errors, and the linter for deeper style/logic checks.

;; Python: Run LSP first, then run our `python-ruff` checker.
(add-hook 'lsp-mode-hook
          (lambda ()
            (when (derived-mode-p 'python-mode 'python-ts-mode)
              ;; `flycheck-add-next-checker` sets up the chain.
              (flycheck-add-next-checker 'lsp 'python-ruff))))

;; Go: Run LSP first, then run `golangci-lint` (a powerful linter).
(add-hook 'lsp-mode-hook
          (lambda ()
            (when (derived-mode-p 'go-mode 'go-ts-mode)
              (flycheck-add-next-checker 'lsp 'golangci-lint))))

;; SQL: Run LSP first, then run our `sql-sqlint` checker.
(add-hook 'lsp-mode-hook
          (lambda ()
            (when (derived-mode-p 'sql-mode 'sql-ts-mode)
              (flycheck-add-next-checker 'lsp 'sql-sqlint))))

;; --- Flycheck Language-Specific Packages ---
;; These packages provide pre-configured checkers for specific languages.

;; Adds support for `golangci-lint`
(use-package flycheck-golangci-lint
  :ensure t
  :after flycheck
  :hook ((go-mode . flycheck-golangci-lint-setup)
         (go-ts-mode . flycheck-golangci-lint-setup)))

;; Adds support for Rust (`cargo check` and `clippy`)
(use-package flycheck-rust
  :ensure t
  :after flycheck
  :hook (rust-ts-mode . flycheck-rust-setup))

;; Adds support for `phpstan` (a static analyzer for PHP)
(use-package flycheck-phpstan
  :ensure t
  :after flycheck)

;; A custom function to set up PHP checking
(defun my-php-mode-setup ()
  "My PHP-mode hook."
  (require 'flycheck-phpstan) ; Make sure phpstan is loaded
  (flycheck-mode t)) ; Ensure flycheck is on

;; Run our custom setup when `php-ts-mode` starts
(add-hook 'php-ts-mode-hook 'my-php-mode-setup)

;; Scala: For Scala, the LSP server (Metals) provides all
;; the diagnostics we need. We don't need a separate checker.
;; This hook just ensures Flycheck is *enabled* so it can
;; *display* the errors that LSP finds.
(add-hook 'lsp-mode-hook
          (lambda ()
            (when (derived-mode-p 'scala-ts-mode)
              (flycheck-mode 1))))

;;; ----------------------------------------------------------------------
;;; Quick-Run
;;; ----------------------------------------------------------------------

;; `quickrun` provides a *single* command to quickly run the
;; current file, no matter what language it is (Python, Go,
;; Ruby, shell script, etc.). It's a great simple tool
;; for quick tests and scripts.
(use-package quickrun
  :ensure t
  ;; Bind `s-r` (Super-r) to the `quickrun` command.
  :bind ("s-r" . quickrun))

;;; ----------------------------------------------------------------------
;;; Finalize
;;; ----------------------------------------------------------------------

;; This is the standard "magic line" for an Emacs Lisp module.
;; It tells Emacs that the file `dev.el` has been loaded successfully,
;; which allows `(require 'dev)` in your `init.el` to work.
(provide 'dev)

;;; dev.el ends here.
