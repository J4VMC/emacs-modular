;;; init.el -- Main initialization file  -*- lexical-binding: t; -*-

;;; Commentary:
;; This file serves as the main entry point for the Emacs configuration.
;; It loads all other configuration files in a specific, logical order
;; to ensure that dependencies are met and settings are applied correctly.

;;; Code:

;; Start the Emacs server if it's not already running.
;; This allows connecting to this Emacs session from the terminal using `emacsclient`.
(require 'server)
(unless (server-running-p)
  (server-start))

(setq insert-directory-program (executable-find "gls"))
(setq dired-use-ls-dired t)
(setq dired-listing-switches "-alh --group-directories-first")

(setq warning-suppress-log-types '((elpaca)))

;; Set up Elpaca package manager
(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

(setq use-package-verbose t) ; Enable package install logging for debugging

(use-package transient
  :ensure t)

;; Slurp environment variables from the shell
(use-package exec-path-from-shell
  :ensure t
  :demand t
  :init
  (setq exec-path-from-shell-variables '("PATH" "MANPATH" "VIRTUAL_ENV" "PYTHONPATH"))
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))


;; Load diminish package to reduce mode-line clutter
(use-package diminish
  :ensure t
  :config
  (diminish 'eldoc-mode))

;; Install and load the Gruvbox theme.
(use-package gruvbox-theme
  :ensure t
  :init
  ;; Load the theme in :init to prevent a "flash" of the default theme on startup.
  (load-theme 'gruvbox-dark-hard t))

;; Customize file for custom settings
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; 1. Add the `modules` directory to Emacs's `load-path`.
;; This allows Emacs to find the files in the `modules/` subdirectory
;; so we can use `require` with short names instead of full paths.
(eval-when-compile
  (add-to-list 'load-path (expand-file-name "modules" user-emacs-directory)))

;; 2. Load the modular configuration files in order.
;; We use `require` instead of `load-file` because it's smarter: it ensures
;; each file is loaded only once, which is a best practice.

;; Load UI customizations (theme, dashboard, mode-line).
(require 'interface)

;; Load general editor settings (line numbers, matching parens, etc.).
(require 'editor)

;; File management
(require 'explorer)

;; Tools for terminal
(require 'terminal)

;; Tools for completion
(require 'completion)

;; Tree-sitter configuration
(require 'tree)

;; Load language-specific major modes and Tree-sitter setup.
(require 'languages)

;; Tools for development
(require 'dev)

;; Load Language Server Protocol (LSP) settings.
(require 'lsp)

;; Tools for debugging
(require 'debug)

;; Load project management tools (Projectile, Treemacs).
(require 'projects)


;; Reset garbage collection
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 2 1024 1024))))

;; --- Automatic Daily Elpaca Update ---

(defvar my/last-elpaca-update-date nil
  "The date of the last automatic Elpaca package upgrade (YYYY-MM-DD).")

(defun my/elpaca-auto-update ()
  "Run `elpaca-update-all` safely and silently.
Logs progress to *Messages* without requiring user interaction."
  (message "Elpaca: Checking for package updates...")
  (let ((inhibit-message t)          ;; suppress noisy logs
        (elpaca-log-functions nil))  ;; disable elpaca log buffer
    (condition-case err
        (progn
          (elpaca-update-all)
          (message "Elpaca: Packages updated successfully on %s."
                   (format-time-string "%Y-%m-%d %H:%M")))
      (error
       (message "Elpaca: Update failed — %s" (error-message-string err)))))
  ;; Always log completion in Messages buffer.
  (message "Elpaca: Automatic daily update process finished."))

(defun my/run-daily-elpaca-update-if-needed ()
  "Check today's date and run `my/elpaca-auto-update` if not already run."
  (let ((current-date (format-time-string "%Y-%m-%d")))
    (unless (equal my/last-elpaca-update-date current-date)
      (message "Elpaca: Running daily package update...")
      (my/elpaca-auto-update)
      (setq my/last-elpaca-update-date current-date)
      (customize-save-variable
       'my/last-elpaca-update-date my/last-elpaca-update-date))))

;; Run the check automatically after Emacs startup, once per day.
;; Using idle timer ensures it runs only after Emacs has fully loaded and settled.
(add-hook 'emacs-startup-hook
          (lambda ()
            (run-with-idle-timer 10 nil #'my/run-daily-elpaca-update-if-needed)))


;;; init.el ends here
