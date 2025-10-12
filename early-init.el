;;; configuration -- summary -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Prevent default package manager from loading
(setq package-enable-at-startup nil)

;; Pointing to the libgccjit library
(when (eq system-type 'darwin)
  (setenv "LIBRARY_PATH"
          (concat (getenv "LIBRARY_PATH")
                  ":/opt/homebrew/lib/gcc/current/gcc/aarch64-apple-darwin25/15")))

;; Increase garbage collection threshold during startup
;; This will be reset in init.el after startup completes
(setq gc-cons-threshold 100000000) ; 100MB

;; Enable plist deserialization for LSP-mode (required by LSP-Booster)
(setenv "LSP_USE_PLISTS" "true")

;; Suppress warnings about obsolete functions during byte compilation
(setq byte-compile-warnings '(not obsolete))

;; Suppress compilation and byte-compilation warnings
(setq warning-suppress-log-types '((comp) (bytecomp)))

;; Suppress reporting of warnings and errors during native compilation
(setq native-comp-async-report-warnings-errors 'silent)

;; Native compilation settings (Emacs 28+)
(when (featurep 'native-compile)
  (setq native-comp-speed 2)
  (setq native-comp-deferred-compilation t))

(setq inhibit-startup-message t
      initial-scratch-message nil)
(fset 'display-startup-echo-area-message #'ignore)

(tool-bar-mode -1)

;; Improve IO performance for LSP
(setq read-process-output-max (* 1024 1024)) ; 1MB

;; Default frame configuration
(setq frame-resize-pixelwise t)
(setq default-frame-alist
      '((fullscreen . maximized)
        (background-color . "#000000")
        (foreground-color . "#ffffff")
        (ns-appearance . dark)
        (ns-transparent-titlebar . t)))

;; On macOS, allow the right Option key to be used as Option/Alt.
(when (eq system-type 'darwin)
  (global-unset-key (kbd "s-p"))
  (setq mac-command-modifier 'super)
  (setq ns-right-alternate-modifier 'none))

(provide 'early-init)
;;; early-init.el ends here
