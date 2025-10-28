;;; languages.el -- Language-specific configurations -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file configures "major modes" for all the programming
;; and markup languages you use.
;;
;; What is a Major Mode? 🧐
;; A "major mode" is what gives Emacs all its language-specific
;; features, like:
;; - Syntax highlighting
;; - Special keybindings (e.g., `C-c C-c` to run code)
;; - Smart indentation
;; - "Commenting" commands
;;
;; What is a Tree-sitter (`-ts-mode`)?
;; You will see many modes ending in `-ts-mode` (e.g., `python-ts-mode`).
;; This means they are using the modern **Tree-sitter** parsing
;; system (from your `tree.el` file) for faster and more
;; accurate syntax highlighting and code navigation.
;;
;; Common Hooks You'll See Here:
;;
;; - `(some-mode . lsp-deferred)`:
;;   Starts the Language Server Protocol (LSP) client. We use
;;   `lsp-deferred` to *delay* this until Emacs is idle,
;;   making file-loading feel instant. ⚡
;;
;; - `(some-mode . apheleia-mode)`:
;;   Connects to an external code formatter (like `prettier`, `black`,
;;   `gofmt`). `apheleia` will format your *entire buffer*
;;   automatically every time you save.
;;
;; - `(some-mode . flycheck-mode)`:
;;   Enables on-the-fly syntax checking (the red squiggly
;;   underlines) to show you errors as you type.

;;; Code:

;;; ----------------------------------------------------------------------
;;; PHP Language
;;; ----------------------------------------------------------------------

(use-package php-mode
  :ensure t
  ;; Tell Emacs to use `php-ts-mode` (the Tree-sitter version)
  ;; for all files ending in .php, .phtml, etc.
  :mode (("\\.php\\'" . php-ts-mode)
         ("\\.phtml\\'" . php-ts-mode)
         ("\\.php[3-7]\\'" . php-ts-mode))
  ;; When `php-ts-mode` starts, automatically enable...
  :hook ((php-ts-mode . lsp-deferred)   ; ...LSP (code intelligence)
         (php-ts-mode . apheleia-mode)  ; ...auto-formatting on save
         (php-ts-mode . flycheck-mode))) ; ...error checking

;; Adds commands for interacting with Composer (PHP's package manager)
(use-package composer
  :ensure t
  :after php-mode ; Load this *after* php-mode
  ;; Make these commands available to be called with M-x
  :commands (composer-install composer-update composer-require))

;; Adds support for running PHPUnit tests from within Emacs
(use-package phpunit
  :ensure t
  :after php-mode)

;; Enable `docstr' inside these major modes.
(add-hook 'php-ts-mode-hook (lambda () (docstr-mode 1)))

;;; ----------------------------------------------------------------------
;;; JavaScript and TypeScript
;;; ----------------------------------------------------------------------

(use-package typescript-mode
  :ensure t
  ;; Associate all JS/TS file extensions with their Tree-sitter modes
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode)
         ("\\.js\\'" . js-ts-mode)
         ("\\.jsx\\'" . tsx-ts-mode))
  ;; Enable our standard helpers for all JS/TS modes
  :hook ((typescript-ts-mode . lsp-deferred)
         (typescript-ts-mode . apheleia-mode)
         (typescript-ts-mode . flycheck-mode)
         (tsx-ts-mode . lsp-deferred)
         (tsx-ts-mode . apheleia-mode)
         (tsx-ts-mode . flycheck-mode)
         (js-ts-mode . lsp-deferred)
         (js-ts-mode . apheleia-mode)
         (js-ts-mode . flycheck-mode))
  :config
  ;; Set the indentation level to 2 spaces (a common convention)
  (setq typescript-indent-level 2
        js-indent-level 2))

;; Enable `docstr' inside these major modes.
(add-hook 'typescript-ts-mode-hook (lambda () (docstr-mode 1)))
(add-hook 'tsx-ts-mode-hook (lambda () (docstr-mode 1)))
(add-hook 'js-ts-mode-hook (lambda () (docstr-mode 1)))

;;; ----------------------------------------------------------------------
;;; Web Technologies (HTML, Twig, JSX/TSX)
;;; ----------------------------------------------------------------------
;;
;; `web-mode` is a powerful, multi-language mode for files that
;; mix HTML, CSS, and server-side code (like PHP/Twig) or
;; client-side code (like JSX).

(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.twig\\'" . web-mode)
         ("\\.jsx\\'" . web-mode)
         ("\\.tsx\\'" . web-mode))
  :hook ((web-mode . lsp-deferred) ; For things like lsp-tailwindcss
         (web-mode . apheleia-mode))
  :config
  ;; Set a consistent 2-space indent for all parts of the file
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2))

;; Enable `docstr' inside these major modes.
(add-hook 'web-mode-hook (lambda () (docstr-mode 1)))

;;; ----------------------------------------------------------------------
;;; Python Language
;;; ----------------------------------------------------------------------

(use-package python
  ;; `:ensure nil` because `python` mode is built-in to Emacs.
  ;; We are just *configuring* it here.
  :ensure nil
  :hook ((python-ts-mode . lsp-deferred)
         (python-ts-mode . apheleia-mode)
         ;; This is a custom inline function (a 'lambda') that
         ;; runs *only* in Python buffers.
         (python-ts-mode . (lambda ()
                             ;; This enforces PEP 8 styling:
                             (setq-local tab-width 4            ; Tabs are 4 spaces
                                         python-indent-offset 4 ; Indents are 4 spaces
                                         indent-tabs-mode nil)))) ; Use spaces, NOT tabs
  :config
  ;; Set the default Python interpreter for commands like `M-x run-python`
  (setq python-shell-interpreter "python3")

  ;; --- Custom Helper Functions for Python ---

  ;; A custom command to easily run a Flask development server
  (defun python-flask-run ()
    "Run Flask development server in project root."
    (interactive)
    (let* (;; Find the project root (e.g., git repo)
           (default-directory (projectile-project-root))
           ;; Ask the user for the app name, defaulting to 'app:app'
           (flask-app (read-string "Flask app (e.g., 'app:app'): " nil nil "app:app")))
      ;; Set the environment variables Flask needs
      (setenv "FLASK_APP" flask-app)
      (setenv "FLASK_ENV" "development")
      ;; Run "flask run" in a compilation buffer
      (compile "flask run")))

  ;; A helper function to quickly add a Python debug breakpoint
  (defun python-add-breakpoint ()
    "Add a `breakpoint()` at current line."
    (interactive)
    (end-of-line)
    (newline-and-indent)
    (insert "breakpoint()  # FIXME: Remove this"))

  ;; A helper to clean up all breakpoints in the file
  (defun python-remove-all-breakpoints ()
    "Remove all `breakpoint()` calls in buffer."
    (interactive)
    (save-excursion ; Restore cursor position after
      (goto-char (point-min)) ; Go to start of file
      ;; Search for lines starting with `breakpoint()`
      (while (re-search-forward "^[[:space:]]*breakpoint().*$" nil t)
        ;; Delete the whole line
        (delete-region (line-beginning-position) (1+ (line-end-position))))))

  ;; --- Keybindings for our new functions ---
  ;; Bind these keys *only* in `python-mode-map`
  (define-key python-mode-map (kbd "C-c ! f") #'python-flask-run)
  (define-key python-mode-map (kbd "C-c b") #'python-add-breakpoint)
  (define-key python-mode-map (kbd "C-c B") #'python-remove-all-breakpoints))

;; This is a custom helper function to find and activate a Python venv
(defun my-pyvenv-autoload ()
  "Automatically activate venv if found in project."
  (interactive)
  ;; Look in the current directory and up for a file named ".venv"
  (when-let ((venv (locate-dominating-file default-directory ".venv")))
    ;; If found, activate it using `pyvenv`
    (pyvenv-activate (expand-file-name ".venv" venv))))

;; Configure the `pyvenv` package to manage virtual environments
(use-package pyvenv
  :ensure t
  :config
  ;; Enable the mode in all Python buffers
  (add-hook 'python-mode-hook #'pyvenv-mode)
  ;; Use our custom function to auto-load the venv when
  ;; opening a file or switching projects.
  (add-hook 'python-mode-hook #'my-pyvenv-autoload)
  (add-hook 'projectile-after-switch-project-hook #'my-pyvenv-autoload))

;; Integrates with the Poetry package manager (shows venv, etc.)
(use-package poetry
  :ensure t
  :hook (python-mode . poetry-tracking-mode))

;; Adds commands for running Pytest
(use-package python-pytest
  :ensure t
  :after python
  :commands (python-pytest-dispatch python-pytest-file python-pytest-function))

;; Adds helpers for Django projects
(use-package python-django
  :ensure t
  :hook ((python-ts-mode . (lambda ()
                             ;; When opening a Python file, check if a
                             ;; `manage.py` file exists in the project.
                             (when (locate-dominating-file default-directory "manage.py")
                               ;; If it does, enable `python-django-mode`
                               (python-django-mode 1))))))

;; Enable `docstr' inside these major modes.
(add-hook 'python-ts-mode-hook (lambda () (docstr-mode 1)))

;;; ----------------------------------------------------------------------
;;; Svelte
;;; ----------------------------------------------------------------------

(use-package svelte-mode
  :ensure t
  :mode ("\\.svelte\\'" . svelte-mode) ; For .svelte files
  :hook ((svelte-mode . lsp-deferred)  ; LSP
         (svelte-mode . apheleia-mode) ; Formatting
         (svelte-mode . flycheck-mode))) ; Error checking

;;; ----------------------------------------------------------------------
;;; Go Language
;;; ----------------------------------------------------------------------

(use-package go-mode
  :ensure t
  :hook ((go-ts-mode . lsp-deferred)
         (go-ts-mode . apheleia-mode)
         (go-ts-mode . flycheck-mode))
  :config
  ;; Use `goimports` instead of the default `gofmt`.
  ;; `goimports` does everything `gofmt` does, *plus*
  ;; it automatically adds and removes import statements.
  (setq gofmt-command "goimports"))

;; Enable `docstr' inside these major modes.
(add-hook 'go-ts-mode-hook (lambda () (docstr-mode 1)))

;;; ----------------------------------------------------------------------
;;; Rust Language (with Rustic)
;;; ----------------------------------------------------------------------
;;
;; `rustic` is a comprehensive package for Rust development.

(use-package rustic
  :ensure t
  :mode ("\\.rs\\'" . rustic-mode)
  :hook ((rustic-mode . lsp-deferred)
         (rustic-mode . apheleia-mode)
         (rustic-mode . flycheck-mode))
  ;; Set up a rich set of keybindings *only* for Rust files
  :bind (:map rustic-mode-map
              ;; --- Essential Bindings ---
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-d" . dap-hydra)
              ;; --- Navigation & Info ---
              ("M-." . lsp-find-definition)
              ("M-," . pop-tag-mark) ; Jump back from definition
              ("M-?" . lsp-find-references)
              ("C-c C-c h" . lsp-documentation)
              ("M-j" . lsp-ui-imenu) ; Show outline of file
              ;; --- Rust Analyzer Specific ---
              ("C-c C-c s" . lsp-rust-analyzer-status)
              ("C-c C-c e" . lsp-rust-analyzer-expand-macro)
              ("C-c C-c j" . lsp-rust-analyzer-join-lines)
              ;; --- Project & Workspace ---
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown))
  :config
  ;; **IMPORTANT**: Turn *off* rustic's built-in format-on-save.
  ;; We want `apheleia-mode` (enabled in the hook) to handle
  ;; formatting, so all languages are formatted consistently.
  (setq rustic-format-on-save nil)

  ;; Use lsp-mode as the backend (default and recommended)
  (setq rustic-lsp-client 'lsp-mode)

  ;; Ensure treesit is used for syntax highlighting and parsing
  (setq rustic-use-tree-sitter t)

  ;; Use `clippy` (the full Rust linter) for flycheck,
  ;; which is more thorough than the default checker.
  (setq rustic-flycheck-checker 'rustic-clippy)

  ;; Configure inlay hints (the gray helper text)
  ;; These are often set in `lsp.el` but are good to have here too.
  (setq rustic-analyzer-proc-macro-enable t)
  (setq rustic-display-inlay-hints t)
  (setq rustic-analyzer-display-chaining-hints t)
  (setq rustic-analyzer-display-closure-return-type-hints t)
  (setq rustic-analyzer-display-lifetime-elision-hints-enable "skip_trivial"))

;; Adds `cargo-minor-mode` for running Cargo (Rust's build tool)
(use-package cargo
  :ensure t
  :hook (rustic-mode . cargo-minor-mode))

;; Enable `docstr' inside these major modes.
(add-hook 'rustic-mode-hook (lambda () (docstr-mode 1)))

;;; ----------------------------------------------------------------------
;;; Scala Language
;;; ----------------------------------------------------------------------

(use-package scala-ts-mode
  :ensure t
  :interpreter ("scala" . scala-ts-mode)
  :hook ((scala-ts-mode . lsp-deferred)
         (scala-ts-mode . apheleia-mode)
         (scala-ts-mode . flycheck-mode)))

;; Enable sbt mode for executing sbt commands
(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: Fixes an issue where the SPACE key doesn't work
  ;; in the `sbt-command` minibuffer.
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  ;; WORKAROUND: Prevents `sbt-supershell` from breaking sbt-mode.
  (setq sbt:program-options '("-Dsbt.supershell=false")))

;; Enable `docstr' inside these major modes.
(add-hook 'scala-ts-mode-hook (lambda () (docstr-mode 1)))

;;; ----------------------------------------------------------------------
;;; SQL
;;; ----------------------------------------------------------------------

(use-package sql
  :ensure nil ; Built-in
  :hook ((sql-ts-mode . lsp-deferred)
         (sql-ts-mode . apheleia-mode)
         (sql-ts-mode . flycheck-mode))
  ;; Bind `C-c C-d` to `sql-connect` to easily connect to a database
  :bind (:map sql-mode-map
              ("C-c C-d" . sql-connect))
  :config
  ;; Set the default database 'product' to Postgres.
  (setq sql-product 'postgres))

;;; ----------------------------------------------------------------------
;;; Swift
;;; ----------------------------------------------------------------------

(use-package swift-mode
  :ensure t
  :hook ((swift-ts-mode . lsp-deferred)
         (swift-ts-mode . apheleia-mode)
         (swift-ts-mode . flycheck-mode)))

;;; ----------------------------------------------------------------------
;;; Docker and Containers
;;; ----------------------------------------------------------------------

;; Syntax highlighting for `Dockerfile`
(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile\\'")

;; Syntax highlighting for `compose.yml` or `compose.yaml`
(use-package docker-compose-mode
  :ensure t
  :mode "compose.*\\.ya?ml\\'")

;; A package for managing Docker containers/images from Emacs
(use-package docker
  :ensure t
  :commands (docker)
  ;; Binds `C-c d` to the main `docker` command interface
  :bind ("C-c d" . docker))

;;; ----------------------------------------------------------------------
;;; MongoDB
;;; ----------------------------------------------------------------------

(use-package mongo
  :ensure t
  ;; Associate .mongodb files with this package's script mode.
  :mode ("\\.mongodb\\'" . mongodb-mode)
  :config
  ;; Use the modern 'mongosh' shell instead of the legacy 'mongo' shell.
  (setq mongo-shell-program "mongosh"))

;;; ----------------------------------------------------------------------
;;; YAML
;;; ----------------------------------------------------------------------

(use-package yaml-mode
  :ensure t
  :hook ((yaml-ts-mode . lsp-deferred)  ; For the `yaml-language-server`
         (yaml-ts-mode . apheleia-mode)))

;;; ----------------------------------------------------------------------
;;; JSON
;;; ----------------------------------------------------------------------

(use-package json-mode
  :ensure t
  :hook ((json-ts-mode . lsp-deferred)  ; For the `json-language-server`
         (json-ts-mode . apheleia-mode)))

;;; ----------------------------------------------------------------------
;;; CSV
;;; ----------------------------------------------------------------------

(use-package csv-mode
  :ensure t
  :mode ("\\.csv\\'" . csv-mode)
  :bind (:map csv-mode-map
              ;; These bindings make navigating a CSV file feel
              ;; just like a spreadsheet!
              ("TAB" . csv-next-field)
              ("<tab>" . csv-next-field)
              ("<backtab>" . csv-previous-field))) ; Shift-Tab

;;; ----------------------------------------------------------------------
;;; XML
;;; ----------------------------------------------------------------------

(use-package xml-mode
  :ensure nil ; Built-in
  :hook ((xml-ts-mode . lsp-deferred)  ; For `lemminx` (XML LSP server)
         (xml-ts-mode . apheleia-mode)))

;;; ----------------------------------------------------------------------
;;; REDIS
;;; ----------------------------------------------------------------------

(use-package redis
  :ensure t
  :config
  ;; `redis` package needs `bookmark` to be loaded.
  (require 'bookmark))

;;; ----------------------------------------------------------------------
;;; Markdown (with Live Preview)
;;; ----------------------------------------------------------------------
;;
;; This section configures `markdown-mode` and adds a custom
;; live-preview feature. It uses `pandoc` (a command-line tool
;; you must install) to convert the Markdown to HTML, and then
;; displays that HTML in a live-updating Emacs `eww` (web browser) buffer.

;; FIX: Make sure Emacs can find `pandoc` and other shell commands.
;; You may need to change this path to match your system.
;; (Run `which pandoc` in your terminal to find the path).
(setq exec-path (append '("/usr/local/bin") exec-path))

(use-package markdown-mode
  :ensure t
  :hook ((markdown-mode . flycheck-mode) ; For linters like `markdownlint`
         (markdown-mode . apheleia-mode)) ; For formatters like `prettier`
  :config
  ;; Use Emacs's built-in font-locking for code blocks
  (setq markdown-fontify-code-blocks-natively t)
  ;; Tell markdown-mode to use `pandoc` for its commands
  (setq markdown-command "pandoc")

  ;; --- Custom Preview Functions ---

  ;; Define a variable to hold the name of our preview buffer
  (defvar my/markdown-preview-buffer "*markdown-preview-eww*"
    "Buffer name for the local Markdown preview.")

  ;; This is the core function that does the rendering
  (defun my/markdown-preview-render ()
    "Render the current buffer's Markdown to HTML and update the preview buffer."
    (let* ((markdown-buffer (current-buffer))
           ;; Create a temp buffer, copy our markdown into it,
           ;; and run `pandoc` on it to get HTML output.
           (html-output
            (with-temp-buffer
              (insert-buffer-substring markdown-buffer)
              (call-process-region (point-min) (point-max)
                                   "pandoc"  ; The command
                                   t         ; Replace buffer with output
                                   t         ; Output to this buffer
                                   nil       ; No error buffer
                                   "-f" "markdown" "-t" "html" "-s")
              (buffer-string)))) ; Grab the resulting HTML
      ;; As long as we got some HTML...
      (when (and html-output (> (length html-output) 0))
        ;; ...get or create our preview buffer.
        (with-current-buffer (get-buffer-create my/markdown-preview-buffer)
          (eww-mode) ; Put it in `eww` (web browser) mode
          (let ((inhibit-read-only t)) ; Allow us to edit it
            (erase-buffer)
            (insert html-output)
            ;; Use `shr` (Emacs's HTML renderer) to parse and
            ;; render the HTML content.
            (let ((document (libxml-parse-html-region (point-min) (point-max))))
              (erase-buffer)
              (shr-insert-document document)))))))

  ;; This is the user-facing command to start the preview
  (defun my/markdown-preview-split ()
    "Create a side-by-side live preview of the current Markdown buffer."
    (interactive)
    (delete-other-windows) ; Close all other windows
    (split-window-right)   ; Split the screen
    (my/markdown-preview-render) ; Do an initial render
    (other-window 1)       ; Move to the new window
    (switch-to-buffer my/markdown-preview-buffer) ; Show the preview
    (other-window -1)      ; Move back to the markdown file
    (my/markdown-live-preview-start)) ; Start the auto-updater

  ;; A variable to hold our live-update timer
  (defvar my/markdown-live-preview-timer nil
    "Idle timer for live Markdown preview updates.")

  ;; This function runs on the timer
  (defun my/markdown-live-preview-update ()
    "Update the Markdown preview if the buffer is modified and the preview is visible."
    ;; If the markdown buffer has changed AND the preview window is still open...
    (when (and (buffer-modified-p) (get-buffer-window my/markdown-preview-buffer))
      ;; ...re-render the content.
      (my/markdown-preview-render)))

  ;; Function to start the auto-update timer
  (defun my/markdown-live-preview-start ()
    "Start the live preview idle timer."
    (interactive)
    (unless my/markdown-live-preview-timer
      (setq my/markdown-live-preview-timer
            ;; Run `my/markdown-live-preview-update` every 1 second
            ;; when Emacs is idle.
            (run-with-idle-timer 1.0 t #'my/markdown-live-preview-update))))

  ;; Function to stop the preview
  (defun my/markdown-live-preview-stop ()
    "Stop the live preview timer and kill the preview buffer."
    (interactive)
    ;; Cancel the timer
    (when my/markdown-live-preview-timer
      (cancel-timer my/markdown-live-preview-timer)
      (setq my/markdown-live-preview-timer nil))
    ;; Kill the preview buffer
    (when-let ((buffer (get-buffer my/markdown-preview-buffer)))
      (kill-buffer buffer)))

  ;; --- Keybindings for Markdown Preview ---
  (define-key markdown-mode-map (kbd "C-c p") #'my/markdown-preview-split)
  (define-key markdown-mode-map (kbd "C-c P") #'my/markdown-live-preview-stop))

;;; ----------------------------------------------------------------------
;;; Terraform
;;; ----------------------------------------------------------------------

(use-package terraform-mode
  :ensure t) ; Provides major mode for `.tf` files


;;; ----------------------------------------------------------------------
;;; Finalize
;;; ----------------------------------------------------------------------

;; This is the standard "magic line" for an Emacs Lisp module.
;; It tells Emacs that the file `languages.el` has been loaded successfully,
;; which allows `(require 'languages)` in your `init.el` to work.
(provide 'languages)

;;; languages.el ends here
