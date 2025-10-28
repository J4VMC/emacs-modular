;;; debug.el -- All necessary tools for debugging -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file sets up `dap-mode` (Debug Adapter Protocol).
;;
;; What is DAP?
;; It's a standard that lets Emacs (or any editor) talk to a
;; "debug adapter" for a specific language. This gives us a
;; full-featured, visual, step-by-step debugging experience,
;; similar to what you'd find in an IDE like VS Code.
;;
;; We will:
;; 1.  Configure the main `dap-mode` package and its keybindings.
;; 2.  Set up `dap-ui` for a nice visual layout.
;; 3.  Configure the specific adapters for each language we use
;;     (Python, Go, PHP, etc.).
;; 4.  Create "Debug Templates," which are like pre-saved "launch"
;;     configurations.

;;; Code:

;;; ----------------------------------------------------------------------
;;; DAP Mode Core
;;; ----------------------------------------------------------------------
(use-package dap-mode
  :ensure t
  ;; `:after lsp-mode` ensures LSP (Language Server Protocol) is
  ;; loaded first, as DAP and LSP often work together.
  :after lsp-mode
  :init
  ;; --- Keybinding Setup ---
  ;; We will create our own keymap for all debugging commands,
  ;; prefixed by `C-c d`. This keeps all debug commands in one place.

  ;; 1. Define an empty "prefix command" map.
  (define-prefix-command 'my-dap-prefix-map)
  ;; 2. Bind this prefix map to `C-c d`.
  (global-set-key (kbd "C-c d") 'my-dap-prefix-map)

  ;; 3. Add all our debug commands to this map.
  ;; `C-c d b` -> Toggle Breakpoint
  (define-key my-dap-prefix-map (kbd "b") #'dap-breakpoint-toggle)
  ;; `C-c d B` -> Delete All Breakpoints
  (define-key my-dap-prefix-map (kbd "B") #'dap-breakpoint-delete-all)
  ;; `C-c d d` -> Start Debugging (will ask *what* to debug)
  (define-key my-dap-prefix-map (kbd "d") #'dap-debug)
  ;; `C-c d n` -> Step Over (Next line)
  (define-key my-dap-prefix-map (kbd "n") #'dap-next)
  ;; `C-c d i` -> Step In (to a function)
  (define-key my-dap-prefix-map (kbd "i") #'dap-step-in)
  ;; `C-c d o` -> Step Out (of a function)
  (define-key my-dap-prefix-map (kbd "o") #'dap-step-out)
  ;; `C-c d c` -> Continue (run until next breakpoint)
  (define-key my-dap-prefix-map (kbd "c") #'dap-continue)
  ;; `C-c d e` -> Edit Debug Template (launch configuration)
  (define-key my-dap-prefix-map (kbd "e") #'dap-debug-edit-template)
  ;; `C-c d w` -> Switch Stack Frame (jump up/down the call stack)
  (define-key my-dap-prefix-map (kbd "w") #'dap-switch-stack-frame)
  ;; `C-c d l` -> Debug Last (re-run the last debug session)
  (define-key my-dap-prefix-map (kbd "l") #'dap-debug-last)
  ;; `C-c d r` -> Disconnect (stop the debugger)
  (define-key my-dap-prefix-map (kbd "r") #'dap-disconnect)
  ;; `C-c d R` -> Debug Recent (pick from a list of recent sessions)
  (define-key my-dap-prefix-map (kbd "R") #'dap-debug-recent)

  :hook
  ;; This hook automatically starts `dap-mode` and `dap-ui-mode`
  ;; *whenever* `lsp-mode` is started. This ensures that debugging
  ;; features are ready to go in any programming buffer.
  ((lsp-mode . dap-mode)
   (lsp-mode . dap-ui-mode))

  :custom
  ;; Set the default Python executable for DAP.
  ;; This can be overridden by the language-specific config below.
  (dap-python-executable "python3")
  ;; Tell `dap-mode` to use the 'debugpy' adapter for Python,
  ;; which is the modern standard.
  (dap-python-debugger 'debugpy)

  :config
  ;; --- Load Language Adapters ---
  ;; We `require` the specific language adapters we need.
  ;; The configurations for these are in separate `use-package`
  ;; blocks below, but this ensures they are loaded by `dap-mode`.
  (require 'dap-python)   ;; Python (debugpy)
  (require 'dap-php)      ;; PHP (XDebug)
  (require 'dap-dlv-go)   ;; Go (Delve)
  (require 'dap-lldb)     ;; C, C++, Rust, Swift (LLDB)
  (require 'dap-gdb-lldb) ;; C, C++, Rust (GDB/LLDB bridge)
  (dap-gdb-lldb-setup)   ;; Run the setup function for GDB/LLDB
  (require 'dap-node)     ;; JavaScript/TypeScript (Node.js)

  ;; --- Debug Templates ---
  ;; These are *crucial*. They are pre-defined "launch configurations"
  ;; (like `launch.json` in VS Code). When you hit `C-c d d` (dap-debug),
  ;; Emacs will offer these as options.

  ;; A template for debugging a Flask web app.
  (dap-register-debug-template "Python :: Debug (Flask)"
			       (list :type "python" :args "" :cwd nil
				     ;; This sets environment variables
				     ;; *before* launching the app.
				     :env '(("FLASK_APP" . "app:app")
					    ("FLASK_ENV" . "development"))
				     :module "flask" :name "Python :: Debug (Flask)"))

  ;; A template for debugging a Django web app.
  (dap-register-debug-template "Python :: Debug (Django)"
			       (list :type "python" :args "manage.py runserver" :cwd nil
				     :name "Python :: Debug (Django)"))

  ;; A template for debugging Python tests with Pytest.
  (dap-register-debug-template "Python :: Debug (Pytest)"
			       (list :type "python" :args "-m pytest -v" :cwd nil
				     :name "Python :: Debug (Pytest)"))

  ;; --- Hooks ---
  ;; This is a great user-experience tweak.
  ;; `dap-stopped-hook` runs *every time* the debugger pauses
  ;; (e.g., at a breakpoint or when you step).
  ;; This line automatically pops up the `dap-hydra` (a handy
  ;; keybinding cheat-sheet) so you know what to press next.
  (add-hook 'dap-stopped-hook (lambda (arg) (call-interactively #'dap-hydra))))

;;; ----------------------------------------------------------------------
;;; DAP UI Configuration
;;; ----------------------------------------------------------------------
;;
;; This package provides the visual "IDE-like" windows for debugging:
;; Locals, Watch, Breakpoints, Call Stack, etc.

(use-package dap-ui
  ;; `:ensure nil` means "don't install this." Why? Because
  ;; `dap-mode` already lists it as a dependency, so Elpaca
  ;; has already installed it. This block is just for *configuring* it.
  :ensure nil
  :after dap-mode
  :config
  ;; Automatically turn on the UI and the debug controls.
  (dap-ui-mode 1)
  (dap-ui-controls-mode 1)

  :custom
  ;; --- Window Layout ---
  ;; This defines where each `dap-ui` window should appear.
  ;; This is a common and very readable setup.
  (dap-ui-buffer-configurations
   `((,(regexp-quote "*dap-ui-locals*")      . ((side . right) (slot . 1) (window-width . 0.20)))
     (,(regexp-quote "*dap-ui-expressions*") . ((side . right) (slot . 2) (window-width . 0.20)))
     (,(regexp-quote "*dap-ui-breakpoints*") . ((side . left) (slot . 2) (window-width . 0.20)))
     (,(regexp-quote "*dap-ui-sessions*")    . ((side . left) (slot . 3) (window-width . 0.20))))))

;;; ----------------------------------------------------------------------
;;; Language Adapters (Specific Setups)
;;; ----------------------------------------------------------------------
;;
;; The `use-package` blocks below are for language-specific *tweaks*.
;; They all use `:ensure nil` because the main `dap-mode` config
;; above already `(require ...)`d them, which handles the installation.
;; These blocks just add hooks and templates.

(use-package dap-python
  :ensure nil
  :after dap-mode
  ;; Run `dap-python-setup` whenever we enter a Python buffer.
  :hook (python-ts-mode . dap-python-setup)
  :config
  ;; This is a helper function to *smartly* find the Python executable.
  (defun my/dap-python-get-interpreter ()
    "Get the Python interpreter, preferring venv if available."
    ;; Check if the $VIRTUAL_ENV environment variable is set.
    (let ((venv (getenv "VIRTUAL_ENV")))
      (if (and venv (> (length venv) 0))
	  ;; If it is, use the Python from *inside* that venv.
	  (concat venv "/bin/python3")
	;; Otherwise, fall back to the system's "python3".
	"python3")))
  ;; Set the dap-python-executable to use our smart function.
  (setq dap-python-executable (my/dap-python-get-interpreter)))

(use-package dap-php
  :ensure nil
  :after dap-mode
  ;; Run `dap-php-setup` whenever we enter a PHP buffer.
  :hook (php-ts-mode . dap-php-setup)
  :config
  ;; A template to tell Emacs to listen for an XDebug connection
  ;; from an external source (like a web browser).
  (dap-register-debug-template "PHP :: Listen for XDebug"
			       (list :type "php" :name "PHP :: Listen for XDebug" :port 9003))

  ;; A template to debug a Laravel application by running its
  ;; `artisan serve` command and attaching the debugger.
  (dap-register-debug-template "PHP :: Debug (Laravel)"
			       (list :type "php" :name "PHP :: Debug (Laravel)" :port 9003
				     :runtimeExecutable "php" :runtimeArgs '("artisan" "serve")
				     :program "${workspaceFolder}")))

(use-package dap-dlv-go
  :ensure nil
  :after dap-mode
  ;; Run `dap-go-setup` whenever we enter a Go buffer.
  :hook (go-ts-mode . dap-go-setup)
  :config
  ;; A simple template to compile and debug the *current file*.
  (dap-register-debug-template "Go :: Debug (Current File)"
			       (list :type "go" :name "Go :: Debug (Current File)" :mode "debug"
				     :request "launch" :program "${file}")))

(use-package dap-node
  :ensure nil
  :after dap-mode
  ;; Run `dap-node-setup` in all JavaScript/TypeScript buffers.
  :hook ((js-ts-mode typescript-ts-mode tsx-ts-mode) . dap-node-setup)
  :config
  ;; A template to debug the current JS or TS file.
  (dap-register-debug-template "JS/TS :: Debug (Current File)"
			       (list :type "node" :name "JS/TS :: Debug (Current File)" :request "launch"
				     :program "${file}" :cwd "${fileDirname}"
				     ;; This is a smart bit:
				     ;; IF the file ends in ".ts" (TypeScript)...
				     :runtimeArgs (when (string-match-p "\\.ts$" (or (buffer-file-name) ""))
						    ;; ...automatically add the `ts-node`
						    ;; flag to run it.
						    '("-r" "ts-node/register"))
				     ;; Don't step into internal Node.js code.
				     :skipFiles '("<node_internals>/**"))))

;; Combined configuration for Rust (and Swift, etc.) using LLDB.
(use-package dap-lldb
  :ensure nil
  :after dap-mode
  ;; Run `dap-lldb-setup` whenever we enter a Rust buffer.
  :hook ((rust-ts-mode) . dap-lldb-setup)
  :config
  ;; A template to debug a compiled Rust binary.
  ;; Note: This assumes a standard Cargo project layout.
  (dap-register-debug-template "Rust :: Debug (Binary)"
			       (list :type "lldb" :name "Rust :: Debug (Binary)" :request "launch"
				     ;; It looks for the compiled binary here:
				     :program "${workspaceFolder}/target/debug/${workspaceFolderBasename}"
				     :cwd "${workspaceFolder}")))

;;; ----------------------------------------------------------------------
;;; Utility Functions
;;; ----------------------------------------------------------------------
;;
;; These are helper functions for debugging `dap-mode` *itself*,
;; not for debugging your code.

(defun dap-enable-logging ()
  "Enable DAP logging for debugging dap-mode itself."
  (interactive)
  ;; This will print all the JSON messages sent between
  ;; Emacs and the debug adapter to the *Messages* buffer.
  (setq dap-print-io t))

(defun dap-disable-logging ()
  "Disable DAP logging."
  (interactive)
  (setq dap-print-io nil))


;;; ----------------------------------------------------------------------
;;; Finalize
;;; ----------------------------------------------------------------------

;; This is the standard "magic line" for an Emacs Lisp module.
;; It tells Emacs that the file `debug.el` has been loaded successfully,
;; which allows `(require 'debug)` in your `init.el` to work.
(provide 'debug)

;;; debug.el ends here
