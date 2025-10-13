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

;; Rust Language
(use-package rust-mode
  :ensure t
  :hook ((rust-ts-mode . lsp-deferred)      ; Auto-start LSP
         (rust-ts-mode . apheleia-mode)     ; Auto-formatting on save
         (rust-ts-mode . flycheck-mode)))   ; On-the-fly linting

(use-package cargo
  :ensure t
  :hook (rust-ts-mode . cargo-minor-mode))

;; Scala Language
(use-package scala-ts-mode
  :ensure t
  :interpreter ("scala" . scala-ts-mode)
  :hook (scala-ts-mode . lsp-deferred)) ; Only hook LSP, Metals handles the rest.

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

;; Markdown
(use-package markdown-mode
  :ensure t
  :hook ((markdown-ts-mode . lsp-deferred)
         (markdown-ts-mode . apheleia-mode)
         (markdown-ts-mode . flycheck-mode))
  :config
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-command "pandoc"))

(use-package markdown-preview-mode
  :ensure t
  :after markdown-mode
  ;; Automatically start the live preview when you open a markdown file.
  :hook (markdown-ts-mode . markdown-preview-mode))

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

(provide 'languages)
;;; languages.el ends here
