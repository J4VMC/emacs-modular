;;; configuration -- summary -*- lexical-binding: t; -*-
;;; Commentary:
;;;
;;; This is the 'early-init.el' file. 🚀
;;;
;;; It's loaded by Emacs *before* your main 'init.el' and *before*
;;; package initialization. This is the place for settings that
;;; need to be applied very early, especially to:
;;;
;;; 1.  Speed up startup (e.g., skipping package loading, deferring garbage collection).
;;; 2.  Prevent UI "flickering" (e.g., setting a dark theme *before* the window appears).
;;; 3.  Configure package loading itself.
;;;
;;; 'lexical-binding: t' is a magic cookie that tells Emacs to use a
;;; more modern and predictable way of handling variables (lexical scoping).
;;; It's standard practice to put this on the first line.
;;;
;;; Code:

;;; ----------------------------------------------------------------------
;;; Package Management
;;; ----------------------------------------------------------------------

;; Stop Emacs from loading its built-in package system (ELPA) at the
;; very start. We will likely be using a different package manager
;; (like 'straight.el' or 'use-package') or loading packages manually
;; later in our main 'init.el'.
;; This is a key performance boost for startup. ⚡
(setq package-enable-at-startup nil)

;;; ----------------------------------------------------------------------
;;; OS-Specific Tweaks (macOS)
;;; ----------------------------------------------------------------------

;; The 'when' block checks if the operating system is macOS ('darwin').
;; These settings will only run on a Mac.
(when (eq system-type 'darwin)

  ;; --- Native Compilation Fix (Apple Silicon) ---

  ;; This is a fix for macOS (especially Apple Silicon).
  ;; Emacs's "native compilation" (which makes Lisp run faster)
  ;; needs a library called 'libgccjit'. This line tells Emacs
  ;; where to find it if it's installed via Homebrew.
  ;; Note: This path might need updating depending on your Homebrew/GCC versions.
  (setenv "LIBRARY_PATH"
          (concat (getenv "LIBRARY_PATH")
                  ":/opt/homebrew/lib/gcc/current/gcc/aarch64-apple-darwin25/15"))

  ;; --- Keyboard Configuration ---

  ;; Tell Emacs to treat the ⌘ (Command) key as the 'Super' modifier.
  (setq mac-command-modifier 'super)

  ;; Unbind 'Super-p' (which is now ⌘-p). This is often done to
  ;; prevent conflicts with the system-wide "Print" dialog.
  (global-unset-key (kbd "s-p"))

  ;; **IMPORTANT**: Make the *right Option* key act as a normal 'Meta' (Alt) key.
  ;; By default, macOS uses the right Option key to type special
  ;; characters (like 'é', '©'). Setting this to 'none' lets us use
  ;; it for Emacs commands (like M-x), just like the left Option key.
  (setq ns-right-alternate-modifier 'none))

;;; ----------------------------------------------------------------------
;;; Performance & Startup Optimizations
;;; ----------------------------------------------------------------------

;; --- Garbage Collection ---

;; Postpone garbage collection (GC) during startup.
;; GC is Emacs's way of cleaning up memory. By setting this to a
;; large value (100MB), we tell Emacs: "Don't stop to clean up
;; while you're busy loading; just load everything first."
;; This will be reset to a more normal value later in 'init.el'.
(setq gc-cons-threshold 100000000) ; 100MB

;; --- LSP & Process I/O ---

;; Enable a faster data format (plists) for 'lsp-mode' (Language Server Protocol).
;; This is often used with 'lsp-booster' to improve performance.
(setenv "LSP_USE_PLISTS" "true")

;; Increase the amount of data Emacs can read from external processes
;; (like Language Servers) at one time. This prevents hangs or
;; slowdowns when an external tool sends a large chunk of data.
(setq read-process-output-max (* 1024 1024)) ; 1MB

;; --- Compilation Warnings (Noise Reduction) ---

;; Tell Emacs to be less "chatty" when compiling Lisp files.
;; We're suppressing several types of warnings to keep the *Messages* buffer clean.

;; Don't warn us if we use an "obsolete" (old) function.
(setq byte-compile-warnings '(not obsolete))

;; Suppress logging for (comp)ilation and (bytecomp)ilation warnings.
(setq warning-suppress-log-types '((comp) (bytecomp)))

;; Be completely silent about warnings/errors during native compilation.
(setq native-comp-async-report-warnings-errors 'silent)

;; --- Native Compilation (Emacs 28+) ---

;; Configure native compilation (compiling Lisp to machine code) if available.
(when (featurep 'native-compile)
  ;; Set the optimization level. 2 is a good balance between
  ;; compilation speed and runtime speed (0=none, 3=max).
  (setq native-comp-speed 2)

  ;; Compile packages in the background when Emacs is idle,
  ;; rather than immediately when they are installed.
  (setq native-comp-deferred-compilation t))

;;; ----------------------------------------------------------------------
;;; UI & Window (Frame) Configuration
;;; ----------------------------------------------------------------------
;;
;; Setting these UI elements here is crucial. It prevents the
;; default white Emacs window from "flashing" before your
;; real theme gets loaded in 'init.el'.
;;
;; --- Startup Screen ---

;; Hide the default Emacs startup splash screen and any "welcome" messages.
;; We want a clean, empty buffer when Emacs opens.
(setq inhibit-startup-message t
      initial-scratch-message nil)
;; Also hide the "Welcome to..." message in the minibuffer (echo area).
(fset 'display-startup-echo-area-message #'ignore)

;; --- Default UI Elements ---

;; Disable the graphical toolbar (with the icons) at the top of the window.
;; We use keyboard shortcuts instead. (Use 1 to enable, -1 to disable).
(tool-bar-mode -1)

;; --- Default Frame (Window) Settings ---

;; Allow smoother, pixel-by-pixel window resizing (vs. character-by-character).
(setq frame-resize-pixelwise t)

;; 'default-frame-alist' is a list of settings for all new windows (frames).
(setq default-frame-alist
      '(;; Open new windows maximized.
        (fullscreen . maximized)
        ;; Set a basic dark theme to avoid the "white flash".
        (background-color . "#000000")
        (foreground-color . "#ffffff")
        ;; macOS-specific UI settings.
        (ns-appearance . dark)           ; Use the 'dark' macOS theme for window elements.
        (ns-transparent-titlebar . t)))  ; Make the title bar transparent.

;;; ----------------------------------------------------------------------
;;; Finalize
;;; ----------------------------------------------------------------------

;; This line tells Emacs that the 'early-init' feature (this file)
;; has finished loading. It's a required convention for Emacs Lisp files.
(provide 'early-init)

;;; early-init.el ends here
