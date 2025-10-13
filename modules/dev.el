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
  ;; --- Custom Formatters ---
  ;; PHP - PSR12 standard
  (setf (alist-get 'phpcs-psr12 apheleia-formatters)
        '("phpcbf" "--standard=PSR12" "--stdin-path=" (or buffer-file-name "stdin")))

  ;; Python - Ruff
  (setf (alist-get 'ruff apheleia-formatters)
	'("ruff" "format" "--stdin-filename" filepath "-"))


  ;; --- Mode Associations ---
  ;; PHP
  (setf (alist-get 'php-ts-mode apheleia-mode-alist) 'phpcs-psr12)
  
  ;; Python
  (setf (alist-get 'python-mode apheleia-mode-alist) 'ruff)
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) 'ruff)
  
  ;; JavaScript / TypeScript
  (setf (alist-get 'typescript-ts-mode apheleia-mode-alist) 'prettier-typescript)
  (setf (alist-get 'tsx-ts-mode apheleia-mode-alist) 'prettier-typescript)
  (setf (alist-get 'js-ts-mode apheleia-mode-alist) 'prettier-javascript)
  (setf (alist-get 'typescript-mode apheleia-mode-alist) 'prettier-typescript)
  (setf (alist-get 'js-mode apheleia-mode-alist) 'prettier-javascript)
  (setf (alist-get 'js2-mode apheleia-mode-alist) 'prettier-javascript)
  
  ;; JSON
  (setf (alist-get 'json-mode apheleia-mode-alist) 'prettier-json)
  (setf (alist-get 'json-ts-mode apheleia-mode-alist) 'prettier-json)
  
  ;; CSS
  (setf (alist-get 'css-mode apheleia-mode-alist) 'prettier-css)
  (setf (alist-get 'css-ts-mode apheleia-mode-alist) 'prettier-css)
  
  ;; HTML
  (setf (alist-get 'html-mode apheleia-mode-alist) 'prettier-html)
  (setf (alist-get 'web-mode apheleia-mode-alist) 'prettier-html)
  
  ;; YAML
  (setf (alist-get 'yaml-mode apheleia-mode-alist) 'prettier-yaml)
  (setf (alist-get 'yaml-ts-mode apheleia-mode-alist) 'prettier-yaml)
  
  ;; Markdown
  (setf (alist-get 'markdown-mode apheleia-mode-alist) 'prettier-markdown)
  (setf (alist-get 'gfm-mode apheleia-mode-alist) 'prettier-markdown)
  
  ;; --- Enable globally ---
  (apheleia-global-mode t))



;; Flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :bind (:map flycheck-mode-map
              ("M-n" . flycheck-next-error)
              ("M-p" . flycheck-previous-error))
  :config
  (flycheck-define-checker typescript-tsc-syntax
    "A TypeScript syntax checker using tsc."
    :command ("tsc"
              "--noEmit"
              "--pretty" "false"
              source-inplace)
    :error-patterns
    ((error line-start (file-name) "(" line "," column "): error TS"
            (message) line-end))
    :modes (typescript-ts-mode tsx-ts-mode))
  
  (flycheck-define-checker python-ruff
    "A Python syntax and style checker using Ruff."
    :command ("ruff" "check"
              ;; Add the rule sets you want to enable globally
              "--select" "E,F,W,I,D,UP,B,SIM,S"
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

(provide 'dev)
;;; dev.el ends here.
