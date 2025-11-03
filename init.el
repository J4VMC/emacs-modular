;;; init.el -- Main initialization file  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Welcome to your main 'init.el' file! 🚀
;;
;; This file is the central nervous system of your Emacs configuration.
;; It's loaded *after* 'early-init.el'.
;;
;; Its main jobs are:
;; 1.  Setting up your package manager (Elpaca).
;; 2.  Installing and configuring all your packages using 'use-package'.
;; 3.  Loading your custom, modular configuration files (from the 'modules/' directory).
;; 4.  Setting any final configuration options.
;;
;; 'lexical-binding: t' is a "magic cookie" that tells Emacs to use a
;; more modern and predictable way of handling variables (lexical scoping).
;; It's standard practice to put this on the first line.

;;; Code:

;;; ----------------------------------------------------------------------
;;; Emacs Server
;;; ----------------------------------------------------------------------
;;
;; This allows you to open files in this *existing* Emacs session
;; from your terminal using `emacsclient -c` or `emacsclient -t`.
;; This is much faster than starting a new Emacs process every time.

(require 'server) ;; Load the built-in server features.
(unless (server-running-p) ;; Check if a server is *already* running.
  (server-start)) ;; If not, start one.

;;; ----------------------------------------------------------------------
;;; Dired (File Manager) Configuration
;;; ----------------------------------------------------------------------
;;
;; These settings customize Dired, the built-in Emacs file manager.

;; Use 'gls' (GNU ls, from coreutils) if available. It's often
;; installed via Homebrew on macOS and has more features (like
;; --group-directories-first) than the default BSD 'ls'.
(setq insert-directory-program (executable-find "gls"))

;; Tell Dired to use the 'ls-dired' program, which is necessary
;; for parsing the custom switches below.
(setq dired-use-ls-dired t)

;; Set the flags to pass to 'ls' (or 'gls').
;; -a: Show all files (including hidden dotfiles).
;; -l: Use long listing format (permissions, owner, size, etc.).
;; -h: Show human-readable file sizes (e.g., "5.0K", "1.2M").
;; --group-directories-first: List all directories before files.
(setq dired-listing-switches "-alh --group-directories-first")

;;; ----------------------------------------------------------------------
;;; Warning Suppression
;;; ----------------------------------------------------------------------

;; Suppress warnings coming specifically from the 'elpaca' package.
;; This helps keep the *Messages* buffer clean during startup.
(setq warning-suppress-log-types '((elpaca)))

;;; ----------------------------------------------------------------------
;;; Package Management (Elpaca)
;;; ----------------------------------------------------------------------
;;
;; This large block of code is the "bootstrap" for Elpaca, our
;; package manager. It's responsible for downloading and installing
;; Elpaca itself, which will then manage all our other packages.
;; This code is generally "copy-paste" and doesn't need to be
;; modified unless you're upgrading the bootstrap logic itself.

(defvar elpaca-installer-version 0.11)
;; Define the directory where Elpaca will live.
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
;; Sub-directories for builds and downloaded repositories.
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))

;; Define the "recipe" for Elpaca itself: where to download it from,
;; what files to include, etc.
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))

;; This 'let*' block is the core bootstrap logic.
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  ;; Add Elpaca's code to the Emacs 'load-path' so it can be found.
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  ;; If the Elpaca repository isn't already downloaded...
  (unless (file-exists-p repo)
    (make-directory repo t) ;; ...create the directory for it.
    (when (<= emacs-major-version 28) (require 'subr-x))
    ;; This 'condition-case' is like a 'try...catch' block.
    (condition-case-unless-debug err
        ;; Try to...
        (if-let* (;; ...run 'git clone' to download Elpaca.
                  (buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ;; ...run 'git checkout' to get the right version.
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ;; ...run a *new* Emacs process in batch mode to byte-compile Elpaca.
                  ;; This makes it load much faster.
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ;; ...load the newly compiled Elpaca.
                  ((require 'elpaca))
                  ;; ...and generate the autoloads file (for lazy loading).
                  ((elpaca-generate-autoloads "elpaca" repo)))
            ;; If all of the above succeeded, print the success message.
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          ;; If any step failed, print the error.
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ;; If an error was caught ('catch' block)...
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  ;; Load the autoloads file so Emacs knows about Elpaca's functions.
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))

;; Tell Elpaca to process any pending package operations (installs, etc.)
;; *after* Emacs has finished initializing.
(add-hook 'after-init-hook #'elpaca-process-queues)

;; Finally, tell Elpaca to install *itself* using the recipe defined above.
(elpaca `(,@elpaca-order))

;;; ----------------------------------------------------------------------
;;; `use-package` Configuration
;;; ----------------------------------------------------------------------
;;
;; 'use-package' is a macro that *dramatically* simplifies package
;; configuration. We will use it for almost everything.

;; Install the Elpaca-aware version of 'use-package'.
(elpaca elpaca-use-package
  ;; Enable the bridge so `use-package :ensure t` works with Elpaca.
  (elpaca-use-package-mode))

;; Tell 'use-package' to be "verbose," meaning it will print
;; detailed information to the *Messages* buffer as it loads packages.
;; This is extremely helpful for debugging.
(setq use-package-verbose t)

;; Install 'transient', which is a required dependency for many
;; popular packages like Magit. It helps create complex pop-up menus.
(use-package transient
  :ensure t)

;;; ----------------------------------------------------------------------
;;; Shell Environment
;;; ----------------------------------------------------------------------
;;
;; This is a *critical* package, especially on macOS.
;; It makes Emacs inherit the environment variables (like $PATH)
;; from your regular shell (zsh, bash, etc.). Without this,
;; Emacs might not be able to find programs like 'git', 'rg', 'python', etc.

(use-package exec-path-from-shell
  :ensure t ;; Make sure it's installed.
  :demand t ;; Load it immediately (don't lazy-load).
  :init
  ;; Tell it *which* variables to copy from the shell *before* it loads.
  (setq exec-path-from-shell-variables '("PATH" "MANPATH" "VIRTUAL_ENV" "PYTHONPATH"))
  :config
  ;; *After* loading, run the function to actually copy the variables.
  ;; We only do this in graphical Emacs. Terminal Emacs (`emacs -nw`)
  ;; already has the correct environment from the shell it started in.
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;;; ----------------------------------------------------------------------
;;; Core Packages & UI
;;; ----------------------------------------------------------------------

;; Load 'diminish' to help clean up the mode-line (the status bar).
(use-package diminish
  :ensure t
  :config
  ;; 'diminish' lets you hide or shorten minor mode names.
  ;; As an example, we hide 'Eldoc Mode', since it's almost always on.
  (diminish 'eldoc-mode))

;; Install and load the Gruvbox theme.
(use-package gruvbox-theme
  :ensure t
  :init
  ;; We load the theme in ':init' (which runs *before* the package
  ;; is loaded) to prevent the default Emacs theme from "flashing"
  ;; at startup. This provides a smooth, flicker-free launch.
  (load-theme 'gruvbox-dark-hard t))

;;; ----------------------------------------------------------------------
;;; Customization File
;;; ----------------------------------------------------------------------

;; Tell Emacs to save settings from the 'M-x customize' interface
;; into a separate file ('~/.emacs.d/custom.el').
;; This keeps our handwritten 'init.el' clean from auto-generated code.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Load the 'custom.el' file if it exists.
;; The 'noerror' argument prevents Emacs from throwing an error
;; if the file doesn't exist (e.g., on a brand new installation).
(load custom-file 'noerror)

;;; ----------------------------------------------------------------------
;;; Modular Configuration
;;; ----------------------------------------------------------------------
;;
;; This is the heart of a modular setup. Instead of putting all
;; our code in this one giant file, we 'require' smaller, focused
;; files from the 'modules/' subdirectory.

;; 1. Add the `modules` directory to Emacs's `load-path`.
;;    This tells Emacs *where* to look for the files we are about to 'require'.
;;    'eval-when-compile' ensures this path is set even when byte-compiling.
(eval-when-compile
  (add-to-list 'load-path (expand-file-name "modules" user-emacs-directory)))

;; 2. Load the modular configuration files in a logical order.
;;    We use `require` (which is smarter than `load-file`) to load
;;    each "module." Each of these files (e.g., 'interface.el')
;;    should end with `(provide 'interface)`.

;; Load UI customizations (theme, dashboard, mode-line).
(require 'interface)

;; Load general editor settings (line numbers, matching parens, etc.).
(require 'editor)

;; File management (Dired, etc.).
(require 'explorer)

;; Tools for the built-in terminal (vterm, eshell).
(require 'terminal)

;; Completion frameworks (Vertico, Company, etc.).
(require 'completion)

;; Tree-sitter configuration (for better syntax highlighting).
(require 'tree)

;; Language-specific major modes (Python, Go, etc.).
(require 'languages)

;; Tools for development (Git/Magit, Docker, etc.).
(require 'dev)

;; Load Language Server Protocol (LSP) settings (Eglot or lsp-mode).
(require 'lsp)

;; Tools for debugging (DAP-mode).
(require 'debug)

;; Load project management tools (Projectile, Treemacs).
(require 'projects)

;;; ----------------------------------------------------------------------
;;; Final Performance Tweaks
;;; ----------------------------------------------------------------------

;; Reset garbage collection (GC) threshold.
;; In 'early-init.el', we set this to a *huge* value (100MB) to speed
;; up startup. Now that Emacs is fully loaded, we set it back to a
(add-hook 'emacs-startup-hook
          (lambda ()
            ;; Set the GC threshold to 32MB. This is a common, balanced
            ;; value. Emacs will now clean up memory after 32MB of
            ;; new data is allocated, which is more responsive.
            (setq gc-cons-threshold (* 32 1024 1024))))

;;; ----------------------------------------------------------------------
;;; Automatic Daily Elpaca Update
;;; ----------------------------------------------------------------------
;;
;; This section defines a set of functions to automatically update
;; your packages once per day, silently in the background.

;; Use `defcustom` here so the variable is registered with the
;; customize system and will be reliably saved in `custom.el`.
(defcustom my/last-elpaca-update-date nil
  "The date of the last automatic Elpaca package upgrade (YYYY-MM-DD)."
  :type '(choice (const nil) string)  ; Fixed: Allow nil or string
  :group 'convenience)

;; This is a "safe" function to run the update.
(defun my/elpaca-auto-update ()
  "Run `elpaca-update-all` safely and silently.
Logs progress to *Messages* without requiring user interaction."
  (message "Checking for package updates...")
  ;; We still use `let` to set `elpaca-log-functions` to nil,
  ;; which prevents the *Elpaca Log* buffer from popping up.
  ;; We *remove* `inhibit-message` so we can see success or failure.
  (let ((elpaca-log-functions nil))
    ;; Use 'condition-case' (try...catch) to handle errors.
    (condition-case err
        ;; --- Try ---
        (progn
          (elpaca-update-all) ;; Run the update
          (message "Packages updated successfully on %s."
                   (format-time-string "%Y-%m-%d %H:%M")))
      ;; --- Catch ---
      (error
       ;; If an error happens, log it to *Messages* instead of crashing.
       (message "Automatic daily package update failed — %s" (error-message-string err))))))

;; This function checks if an update is needed.
(defun my/run-daily-elpaca-update-if-needed ()
  "Check today's date and run `my/elpaca-auto-update` if not already run."
  (let ((current-date (format-time-string "%Y-%m-%d")))
    ;; If the last update date is *not* today...
    (unless (equal my/last-elpaca-update-date current-date)
      (message "Running daily package update in the background!")
      ;; ...run the safe update function.
      (my/elpaca-auto-update)
      ;; ...update the variable to today's date.
      (setq my/last-elpaca-update-date current-date)
      ;; ...and save this new date to 'custom.el' so we remember it
      ;; next time Emacs starts.
      (customize-save-variable
       'my/last-elpaca-update-date my/last-elpaca-update-date)
      (message "Package update finished!")))
  (message "All packages are up to date!"))

;; Finally, schedule the check to run *after* Elpaca initializes.
;; Changed from 'emacs-startup-hook' to 'elpaca-after-init-hook'
;; to ensure Elpaca is fully ready before attempting updates.
(add-hook 'elpaca-after-init-hook
          (lambda ()
            ;; Don't run this *immediately* at startup; that would slow
            ;; it down. Instead, wait for Emacs to be idle for 30
            ;; seconds, *then* run the check. This is polite and safe.
            (run-with-idle-timer 30 nil #'my/run-daily-elpaca-update-if-needed)))

;;; init.el ends here
