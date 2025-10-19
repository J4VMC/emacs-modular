;;; lsp.el -- Settings for LSP -*- lexical-binding: t; -*-
;;; Commentary:
;; All settings relevant to lsp-mode
;;; Code:

;; LSP-mode
(use-package lsp-mode
  :diminish "LSP"
  :ensure t
  :hook ((lsp-mode . lsp-diagnostics-mode)
         (lsp-mode . lsp-enable-which-key-integration))
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-completion-provider :none)
  (lsp-diagnostics-provider :flycheck)
  (lsp-session-file (locate-user-emacs-file ".lsp-session"))
  (lsp-log-io nil)
  (lsp-keep-workspace-alive nil)
  (lsp-idle-delay 0.5)
  ;; core
  (lsp-enable-xref t)
  (lsp-auto-configure t)
  (lsp-eldoc-enable-hover t)
  (lsp-enable-dap-auto-configure t)
  (lsp-enable-file-watchers nil)
  (lsp-enable-folding t)
  (lsp-enable-imenu t)
  (lsp-enable-indentation t)
  (lsp-enable-links t)
  (lsp-enable-on-type-formatting t)
  (lsp-enable-suggest-server-download t)
  (lsp-enable-symbol-highlighting t)
  (lsp-enable-text-document-color nil)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-diagnostic-max-lines 20)
  ;; completion
  (lsp-completion-enable t)
  (lsp-completion-enable-additional-text-edit t)
  (lsp-enable-snippet nil)
  (lsp-completion-show-kind t)
  ;; headerline
  (lsp-headerline-breadcrumb-enable t)
  (lsp-headerline-breadcrumb-enable-diagnostics nil)
  (lsp-headerline-breadcrumb-enable-symbol-numbers nil)
  (lsp-headerline-breadcrumb-icons-enable nil)
  ;; modeline
  (lsp-modeline-code-actions-enable nil)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-modeline-workspace-status-enable nil)
  (lsp-signature-doc-lines 1)
  (lsp-ui-doc-use-childframe t)
  (lsp-eldoc-render-all nil)
  ;; lens
  (lsp-lens-enable nil)
  ;; semantic
  (lsp-semantic-tokens-enable nil)
  
  :init
  (setq lsp-use-plists t))

;; Disable LSP indentation and on-type formatting for JS/TS (let Prettier handle it)
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

(use-package lsp-completion
  :no-require
  :hook ((lsp-mode . lsp-completion-mode)))

(use-package lsp-ui
  :ensure t
  :commands
  (lsp-ui-doc-show
   lsp-ui-doc-glance)
  :bind (:map lsp-mode-map
              ("C-c C-d" . 'lsp-ui-doc-glance))
  :after (lsp-mode)
  :config (setq lsp-ui-doc-enable t
                lsp-ui-doc-show-with-cursor nil
                lsp-ui-doc-include-signature t
                lsp-ui-doc-position 'at-point))

(use-package consult-lsp
  :ensure t
  :after (consult lsp-mode))

(use-package lsp-treemacs
  :ensure t
  :after (lsp-mode treemacs))

(use-package lsp-eslint
  :after lsp-mode
  :custom
  (lsp-eslint-auto-fix-on-save nil)
  (lsp-eslint-enable t)
  (lsp-eslint-package-manager "npm")
  (lsp-eslint-server-command '("vscode-eslint-language-server" "--stdio"))
  :config
  (add-to-list 'lsp-language-id-configuration '(typescript-ts-mode . "typescript"))
  (add-to-list 'lsp-language-id-configuration '(tsx-ts-mode . "typescriptreact"))
  (add-to-list 'lsp-language-id-configuration '(js-ts-mode . "javascript")))

(use-package lsp-tailwindcss
  :ensure t
  :init (setq lsp-tailwindcss-add-on-mode t)
  :config
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

(use-package lsp-metals
  :ensure t
  :hook (scala-ts-mode . (lambda ()
                           (require 'lsp-metals)
                           (lsp-deferred)))
  :config
  (setq lsp-metals-server-args '("-J-Dmetals.allow-multiline-string-formatting=off")
        lsp-metals-show-implicit-arguments t
        lsp-metals-show-implicit-conversions-and-classes t
        lsp-metals-show-inferred-type t))

(use-package lsp-pyright
  :ensure t
  :custom (lsp-pyright-langserver-command "basedpyright")
  :hook (python-ts-mode . (lambda ()
                            (require 'lsp-pyright)
                            (lsp-deferred))))

;; Ensure project.el is loaded
(require 'project)

;; Function to set pyright paths based on the current project
(defun set-pyright-paths ()
  "Set the python.analysis.extraPaths for pyright based on the current project."
  (let ((project-root (project-current)))
    (when project-root
      (setq lsp-pyright-workspace-config
            `(:python.analysis.extraPaths [,project-root])))))

;; Add the function to the python-mode hook
(add-hook 'python-mode-hook 'set-pyright-paths)


(use-package lsp-rust
  :after lsp-mode
  :hook (rust-ts-mode . (lambda ()
                          (require 'lsp-rust)
                          (lsp-deferred)))
  :config
  (setq lsp-rust-analyzer-cargo-watch-command "clippy"
        lsp-rust-analyzer-server-display-inlay-hints t
        lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial"
        lsp-rust-analyzer-display-chaining-hints t
        lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil
        lsp-rust-analyzer-display-closure-return-type-hints t
        lsp-rust-analyzer-display-parameter-hints nil
        lsp-rust-analyzer-display-reborrow-hints nil))

;; Go LSP configuration (built into lsp-mode)
(with-eval-after-load 'lsp-mode
  (add-hook 'go-ts-mode-hook #'lsp-deferred)
  
  ;; Go-specific settings
  (setq lsp-go-analyses '((fieldalignment . t)
                          (nilness . t)
                          (unusedwrite . t)
                          (unusedparams . t))
        lsp-go-use-gofumpt t
        lsp-go-codelenses '((generate . t)
                            (test . t)
                            (tidy . t))))

;; SQL LSP configuration
(with-eval-after-load 'lsp-mode
  (add-hook 'sql-ts-mode-hook #'lsp-deferred)
  
  ;; Register SQL language server
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("sql-language-server" "up" "--method" "stdio"))
    :major-modes '(sql-mode sql-ts-mode)
    :priority -1
    :server-id 'sql-ls))
  
  (setq lsp-sqls-workspace-config-path nil))

(provide 'lsp)
;;; lsp.el ends here.
