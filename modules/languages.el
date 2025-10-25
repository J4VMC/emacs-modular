;;; languages.el -- Language-specific configurations -*- lexical-binding: t; -*-

;;; Commentary:
;; This file configures major modes for various programming and markup languages.
;; It relies on `treesit-auto` (configured in projects.el) for languages with
;; modern tree-sitter modes (JS, TS, Python, JSON, YAML, etc.).

;;; Code:

;; PHP Language
(use-package php-mode
  :ensure t
  :mode (("\\.php\\'" . php-ts-mode)
         ("\\.phtml\\'" . php-ts-mode)
         ("\\.php[3-7]\\'" . php-ts-mode))
  :hook ((php-ts-mode . lsp-deferred)
         (php-ts-mode . apheleia-mode)
         (php-ts-mode . flycheck-mode)))

(use-package composer
  :ensure t
  :after php-mode
  :commands (composer-install composer-update composer-require))

(use-package phpunit
  :ensure t
  :after php-mode)

;; JavaScript and TypeScript Configuration
(use-package typescript-mode
  :ensure t
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode)
         ("\\.js\\'" . js-ts-mode)
         ("\\.jsx\\'" . tsx-ts-mode))
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
  (setq typescript-indent-level 2
        js-indent-level 2))

;; Web Technologies (HTML, Twig, JSX/TSX)
(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.twig\\'" . web-mode)
         ("\\.jsx\\'" . web-mode)
         ("\\.tsx\\'" . web-mode))
  :hook ((web-mode . lsp-deferred)
         (web-mode . apheleia-mode))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2))

;; Python Language
(use-package python
  :ensure nil
  :hook ((python-ts-mode . lsp-deferred)
	 (python-ts-mode . apheleia-mode)
         (python-ts-mode . (lambda ()
                             (setq-local tab-width 4
					 python-indent-offset 4
					 indent-tabs-mode nil))))
  :config
  (setq python-shell-interpreter "python3")

  (defun python-flask-run ()
    "Run Flask development server in project root."
    (interactive)
    (let* ((default-directory (projectile-project-root))
           (flask-app (read-string "Flask app (e.g., 'app:app'): " nil nil "app:app")))
      (setenv "FLASK_APP" flask-app)
      (setenv "FLASK_ENV" "development")
      (compile "flask run")))

  (defun python-add-breakpoint ()
    "Add a `breakpoint()` at current line."
    (interactive)
    (end-of-line)
    (newline-and-indent)
    (insert "breakpoint()  # FIXME: Remove this"))

  (defun python-remove-all-breakpoints ()
    "Remove all `breakpoint()` calls in buffer."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^[[:space:]]*breakpoint().*$" nil t)
        (delete-region (line-beginning-position) (1+ (line-end-position))))))

  (define-key python-mode-map (kbd "C-c ! f") #'python-flask-run)
  (define-key python-mode-map (kbd "C-c b") #'python-add-breakpoint)
  (define-key python-mode-map (kbd "C-c B") #'python-remove-all-breakpoints))

(defun my-pyvenv-autoload ()
  "Automatically activate venv if found in project."
  (interactive)
  (when-let ((venv (locate-dominating-file default-directory ".venv")))
    (pyvenv-activate (expand-file-name ".venv" venv))))

(use-package pyvenv
  :ensure t
  :config
  ;; The hook to enable the mode itself can stay here
  (add-hook 'python-mode-hook #'pyvenv-mode)
  ;; Now add the hooks that use the globally defined function
  (add-hook 'python-mode-hook #'my-pyvenv-autoload)
  (add-hook 'projectile-after-switch-project-hook #'my-pyvenv-autoload))

(use-package poetry
  :ensure t
  :hook (python-mode . poetry-tracking-mode))

(use-package python-pytest
  :ensure t
  :after python
  :commands (python-pytest-dispatch python-pytest-file python-pytest-function))

(use-package python-django
  :ensure t
  :hook ((python-ts-mode . (lambda ()
                             (when (locate-dominating-file default-directory "manage.py")
                               (python-django-mode 1))))))

;; Svelte
(use-package svelte-mode
  :ensure t
  :mode ("\\.svelte\\'" . svelte-mode)
  :hook ((svelte-mode . lsp-deferred)
         (svelte-mode . apheleia-mode)
         (svelte-mode . flycheck-mode)))

;; Go Language
(use-package go-mode
  :ensure t
  :hook ((go-ts-mode . lsp-deferred)
         (go-ts-mode . apheleia-mode)
         (go-ts-mode . flycheck-mode))
  :config
  ;; Use the superior 'goimports' instead of the default 'gofmt'.
  ;; It formats and organizes imports automatically.
  (setq gofmt-command "goimports"))

;; Rust Language (with Rustic)
(use-package rustic
  :ensure t
  :mode ("\\.rs\\'" . rustic-mode)
  :hook ((rustic-mode . lsp-deferred)
         (rustic-mode . apheleia-mode)
         (rustic-mode . flycheck-mode))
  :bind (:map rustic-mode-map
              ;; --- Essential Bindings ---
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-d" . dap-hydra)
              ;; --- Navigation & Info ---
              ("M-." . lsp-find-definition)
              ("M-," . pop-tag-mark)
              ("M-?" . lsp-find-references)
              ("C-c C-c h" . lsp-documentation)
              ("M-j" . lsp-ui-imenu)
              ;; --- Rust Analyzer Specific ---
              ("C-c C-c s" . lsp-rust-analyzer-status)
              ("C-c C-c e" . lsp-rust-analyzer-expand-macro)
              ("C-c C-c j" . lsp-rust-analyzer-join-lines)
              ;; --- Project & Workspace ---
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown))
  :config
  ;; Don't use rustic's format-on-save. Let apheleia handle it consistently.
  (setq rustic-format-on-save nil)
  
  ;; Use lsp-mode as the backend (default and recommended)
  (setq rustic-lsp-client 'lsp-mode)
  
  ;; Ensure treesit is used for syntax highlighting and parsing
  ;; This is important for proper font-locking and structural features
  (setq rustic-use-tree-sitter t)
  
  ;; Use clippy for better linting
  (setq rustic-flycheck-checker 'rustic-clippy)
  
  ;; These are already configured via lsp-rust in your lsp.el,
  ;; but rustic variables also work as fallbacks
  (setq rustic-analyzer-proc-macro-enable t)
  (setq rustic-display-inlay-hints t)
  (setq rustic-analyzer-display-chaining-hints t)
  (setq rustic-analyzer-display-closure-return-type-hints t)
  (setq rustic-analyzer-display-lifetime-elision-hints-enable "skip_trivial"))

(use-package cargo
  :ensure t
  :hook (rustic-mode . cargo-minor-mode))

;; Scala Language
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
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false")))

;; SQL
(use-package sql
  :ensure nil
  :hook ((sql-ts-mode . lsp-deferred)
         (sql-ts-mode . apheleia-mode)
         (sql-ts-mode . flycheck-mode))
  :bind (:map sql-mode-map
              ("C-c C-d" . sql-connect))
  :config
  (setq sql-product 'postgres))

;; Swift
(use-package swift-mode
  :ensure t
  :hook ((swift-ts-mode . lsp-deferred)
         (swift-ts-mode . apheleia-mode)
         (swift-ts-mode . flycheck-mode)))

;; Docker and Containers
(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile\\'")

(use-package docker-compose-mode
  :ensure t
  :mode "compose.*\\.ya?ml\\'")

(use-package docker
  :ensure t
  :commands (docker)
  :bind ("C-c d" . docker))

;; MongoDB
(use-package mongo
  :ensure t
  ;; Associate .mongodb files with this package's script mode.
  :mode ("\\.mongodb\\'" . mongodb-mode)
  :config
  ;; Use the modern 'mongosh' shell instead of the legacy 'mongo' shell.
  (setq mongo-shell-program "mongosh"))

;; YAML
(use-package yaml-mode
  :ensure t
  :hook ((yaml-ts-mode . lsp-deferred)
         (yaml-ts-mode . apheleia-mode)))

;; JSON
(use-package json-mode
  :ensure t
  :hook ((json-ts-mode . lsp-deferred)
	 (json-ts-mode . apheleia-mode)))

;; CSV
(use-package csv-mode
  :ensure t
  :mode ("\\.csv\\'" . csv-mode)
  :bind (:map csv-mode-map
              ;; Use Tab to navigate between columns like in a spreadsheet
              ("TAB" . csv-next-field)
              ("<tab>" . csv-next-field)
              ("<backtab>" . csv-previous-field)))

;; XML Language
(use-package xml-mode
  :ensure nil ;; Built-in, but we configure it for tree-sitter
  :hook ((xml-ts-mode . lsp-deferred)   ; Auto-start LSP
         (xml-ts-mode . apheleia-mode))) ; Auto-formatting on save

;; REDIS
(use-package redis
  :ensure t
  :config
  (require 'bookmark))

;; --- Local, Live Markdown Preview (Pandoc + EWW) ---

;; FIX: Ensure Emacs can find pandoc and other shell commands
;; (Replace the path with the output of 'which pandoc' on your system)
(setq exec-path (append '("/usr/local/bin") exec-path))

(use-package markdown-mode
  :ensure t
  :hook ((markdown-mode . flycheck-mode)
         (markdown-mode . apheleia-mode))
  :config
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-command "pandoc")

  ;; Buffer name for preview
  (defvar my/markdown-preview-buffer "*markdown-preview-eww*"
    "Buffer name for the local Markdown preview.")

  (defun my/markdown-preview-render ()
    "Render the current buffer's Markdown to HTML and update the preview buffer."
    (let* ((markdown-buffer (current-buffer))
           (html-output
            (with-temp-buffer
              (insert-buffer-substring markdown-buffer)
              (call-process-region (point-min) (point-max)
                                   "pandoc"
                                   t        ; Replace buffer contents with output
                                   t        ; Output goes to this buffer
                                   nil      ; No error buffer
                                   "-f" "markdown" "-t" "html" "-s")
              (buffer-string))))
      (when (and html-output (> (length html-output) 0))
	(with-current-buffer (get-buffer-create my/markdown-preview-buffer)
          (eww-mode)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert html-output)
            (let ((document (libxml-parse-html-region (point-min) (point-max))))
              (erase-buffer)
              (shr-insert-document document)))))))

  (defun my/markdown-preview-split ()
    "Create a side-by-side live preview of the current Markdown buffer."
    (interactive)
    (delete-other-windows)
    (split-window-right)
    (my/markdown-preview-render)
    (other-window 1)
    (switch-to-buffer my/markdown-preview-buffer)
    (other-window -1)
    (my/markdown-live-preview-start))

  (defvar my/markdown-live-preview-timer nil
    "Idle timer for live Markdown preview updates.")

  (defun my/markdown-live-preview-update ()
    "Update the Markdown preview if the buffer is modified and the preview is visible."
    (when (and (buffer-modified-p) (get-buffer-window my/markdown-preview-buffer))
      (my/markdown-preview-render)))

  (defun my/markdown-live-preview-start ()
    "Start the live preview idle timer."
    (interactive)
    (unless my/markdown-live-preview-timer
      (setq my/markdown-live-preview-timer
            (run-with-idle-timer 1.0 t #'my/markdown-live-preview-update))))

  (defun my/markdown-live-preview-stop ()
    "Stop the live preview timer and kill the preview buffer."
    (interactive)
    (when my/markdown-live-preview-timer
      (cancel-timer my/markdown-live-preview-timer)
      (setq my/markdown-live-preview-timer nil))
    (when-let ((buffer (get-buffer my/markdown-preview-buffer)))
      (kill-buffer buffer)))

  (define-key markdown-mode-map (kbd "C-c p") #'my/markdown-preview-split)
  (define-key markdown-mode-map (kbd "C-c P") #'my/markdown-live-preview-stop))

(use-package terraform-mode
  :ensure t)

(provide 'languages)
;;; languages.el ends here
