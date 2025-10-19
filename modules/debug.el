;;; debug.el -- All necessary tools for debugging -*- lexical-binding: t; -*-
;;; Commentary:
;; All the relevant settings for dap-mode
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --- DAP Mode Core ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package dap-mode
  :ensure t
  :after lsp-mode
  :init
  ;; --- Keybinding Setup ---
  (define-prefix-command 'my-dap-prefix-map)
  (global-set-key (kbd "C-c d") 'my-dap-prefix-map)
  (define-key my-dap-prefix-map (kbd "b") #'dap-breakpoint-toggle)
  (define-key my-dap-prefix-map (kbd "B") #'dap-breakpoint-delete-all)
  (define-key my-dap-prefix-map (kbd "d") #'dap-debug)
  (define-key my-dap-prefix-map (kbd "n") #'dap-next)
  (define-key my-dap-prefix-map (kbd "i") #'dap-step-in)
  (define-key my-dap-prefix-map (kbd "o") #'dap-step-out)
  (define-key my-dap-prefix-map (kbd "c") #'dap-continue)
  (define-key my-dap-prefix-map (kbd "e") #'dap-debug-edit-template)
  (define-key my-dap-prefix-map (kbd "w") #'dap-switch-stack-frame)
  (define-key my-dap-prefix-map (kbd "l") #'dap-debug-last)
  (define-key my-dap-prefix-map (kbd "r") #'dap-disconnect)
  (define-key my-dap-prefix-map (kbd "R") #'dap-debug-recent)
  :hook
  ((lsp-mode . dap-mode)
   (lsp-mode . dap-ui-mode))
  :custom
  (dap-python-executable "python3")
  (dap-python-debugger 'debugpy)
  :config
  (require 'dap-python)
  (require 'dap-php)
  (require 'dap-dlv-go)
  (require 'dap-lldb)
  (require 'dap-gdb-lldb)
  (dap-gdb-lldb-setup)
  (require 'dap-node)
  (dap-register-debug-template "Python :: Debug (Flask)"
			       (list :type "python" :args "" :cwd nil :env '(("FLASK_APP" . "app:app") ("FLASK_ENV" . "development"))
				     :module "flask" :name "Python :: Debug (Flask)"))
  (dap-register-debug-template "Python :: Debug (Django)"
			       (list :type "python" :args "manage.py runserver" :cwd nil :name "Python :: Debug (Django)"))
  (dap-register-debug-template "Python :: Debug (Pytest)"
			       (list :type "python" :args "-m pytest -v" :cwd nil :name "Python :: Debug (Pytest)"))
  (add-hook 'dap-stopped-hook (lambda (arg) (call-interactively #'dap-hydra))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --- DAP UI Configuration ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package dap-ui
  :ensure nil
  :after dap-mode
  :config
  (dap-ui-mode 1)
  (dap-ui-controls-mode 1)
  :custom
  (dap-ui-buffer-configurations
   `((,(regexp-quote "*dap-ui-locals*")     . ((side . right) (slot . 1) (window-width . 0.20)))
     (,(regexp-quote "*dap-ui-expressions*") . ((side . right) (slot . 2) (window-width . 0.20)))
     (,(regexp-quote "*dap-ui-breakpoints*") . ((side . left) (slot . 2) (window-width . 0.20)))
     (,(regexp-quote "*dap-ui-sessions*")    . ((side . left) (slot . 3) (window-width . 0.20))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --- Language Adapters ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package dap-python
  :ensure nil
  :after dap-mode
  :hook (python-ts-mode . dap-python-setup)
  :config
  (defun my/dap-python-get-interpreter ()
    "Get the Python interpreter, preferring venv if available."
    (let ((venv (getenv "VIRTUAL_ENV")))
      (if (and venv (> (length venv) 0))
	  (concat venv "/bin/python3")
	"python3")))
  (setq dap-python-executable (my/dap-python-get-interpreter)))

(use-package dap-php
  :ensure nil
  :after dap-mode
  :hook (php-ts-mode . dap-php-setup)
  :config
  (dap-register-debug-template "PHP :: Listen for XDebug"
			       (list :type "php" :name "PHP :: Listen for XDebug" :port 9003))
  (dap-register-debug-template "PHP :: Debug (Laravel)"
			       (list :type "php" :name "PHP :: Debug (Laravel)" :port 9003
				     :runtimeExecutable "php" :runtimeArgs '("artisan" "serve")
				     :program "${workspaceFolder}")))

(use-package dap-dlv-go
  :ensure nil
  :after dap-mode
  :hook (go-ts-mode . dap-go-setup)
  :config
  (dap-register-debug-template "Go :: Debug (Current File)"
			       (list :type "go" :name "Go :: Debug (Current File)" :mode "debug"
				     :request "launch" :program "${file}")))

(use-package dap-node
  :ensure nil
  :after dap-mode
  :hook ((js-ts-mode typescript-ts-mode tsx-ts-mode) . dap-node-setup)
  :config
  (dap-register-debug-template "JS/TS :: Debug (Current File)"
			       (list :type "node" :name "JS/TS :: Debug (Current File)" :request "launch"
				     :program "${file}" :cwd "${fileDirname}"
				     :runtimeArgs (when (string-match-p "\\.ts$" (or (buffer-file-name) "")) '("-r" "ts-node/register"))
				     :skipFiles '("<node_internals>/**"))))

;; Combined configuration for Rust and Swift
(use-package dap-lldb
  :ensure nil
  :after dap-mode
  :hook ((rust-ts-mode) . dap-lldb-setup)
  :config
  (dap-register-debug-template "Rust :: Debug (Binary)"
			       (list :type "lldb" :name "Rust :: Debug (Binary)" :request "launch"
				     :program "${workspaceFolder}/target/debug/${workspaceFolderBasename}"
				     :cwd "${workspaceFolder}")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --- Utility Functions ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dap-enable-logging ()
  "Enable DAP logging for debugging dap-mode itself."
  (interactive)
  (setq dap-print-io t))

(defun dap-disable-logging ()
  "Disable DAP logging."
  (interactive)
  (setq dap-print-io nil))

(provide 'debug)
;;; debug.el ends here
