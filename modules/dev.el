;;; dev.el -- All settings relevant for Software Development -*- lexical-binding: t; -*-
;;; Commentary:
;; This file includes all the relevant settings for tools like lsp, rest client, etc.
;;; Code:
;; Magit: The definitive Git integration for Emacs.
(use-package magit
  :ensure t
  :after transient
  :bind ("C-x g" . magit-status))

;; API testing
(use-package restclient
  :ensure t
  :mode ("\\.http\\'" . restclient-mode)
  :config (use-package restclient-test :ensure t))

;; Emmet: Fast HTML/CSS abbreviation expansion.
(use-package emmet-mode
  :ensure t
  :hook ((web-mode . emmet-mode)
         (css-mode . emmet-mode))
  :config (setq emmet-expand-jsx-className? t))

;; Apheleia: Run code formatters automatically on save.
(use-package apheleia
  :ensure t
  :diminish ""
  :config
  (setf (alist-get 'phpcs-psr12 apheleia-formatters)
        '("phpcbf" "--standard=PSR12" "--stdin-path=" (or buffer-file-name "stdin")))
  (setf (alist-get 'php-ts-mode apheleia-mode-alist) 'phpcs-psr12)
  (setf (alist-get 'python-mode apheleia-mode-alist) 'ruff-isort)
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) 'ruff-isort)
  (apheleia-global-mode t))

;; Flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :bind (:map flycheck-mode-map
              ("M-n" . flycheck-next-error)
              ("M-p" . flycheck-previous-error))
  :config
  (flycheck-define-checker python-ruff
    "A Python syntax and style checker using Ruff."
    :command ("ruff" "check" 
              "--output-format" "concise"
              "--stdin-filename" source-inplace
              "-")
    :standard-input t
    :error-patterns
    ((error line-start (file-name) ":" line ":" column ": " (message) line-end))
    :modes (python-mode python-ts-mode))
  (setq flycheck-phpcs-standard "PSR12"))

(add-hook 'lsp-mode-hook
          (lambda ()
            (when (derived-mode-p 'python-mode 'python-ts-mode)
              (flycheck-add-next-checker 'lsp 'python-ruff))))

(use-package flycheck-golangci-lint
  :ensure t
  :after flycheck
  :hook ((go-mode . flycheck-golangci-lint-setup)
	 (go-ts-mode . flycheck-golangci-lint-setup)))

(use-package flycheck-rust
  :ensure t
  :after flycheck
  :hook (rust-ts-mode . flycheck-rust-setup))

(use-package flycheck-phpstan
  :ensure t
  :after flycheck)

(defun my-php-mode-setup ()
  "My PHP-mode hook."
  (require 'flycheck-phpstan)
  (flycheck-mode t))

(add-hook 'php-ts-mode-hook 'my-php-mode-setup)

;; Format code
(use-package format-all
  :ensure t
  :commands format-all-mode
  :hook (prog-mode . format-all-mode))

(provide 'dev)
;;; dev.el ends here.
