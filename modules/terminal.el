;;; terminal.el -- Settings for the terminal -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file configures everything related to using a terminal *inside* Emacs.
;;
;; The main package we use is `vterm`, which is a "full" terminal
;; emulator (like iTerm, Alacritty, or GNOME Terminal) that lives *inside*
;; an Emacs buffer. This is different from `shell` (which is basic) or
;; `eshell` (which is an Emacs-Lisp-based shell).
;;
;; We also configure `vterm-toggle` to give us a "Quake-style" pop-up
;; terminal at the press of a key.
;;
;;; Code:

;;; ----------------------------------------------------------------------
;;; Fish Mode (Syntax Highlighting)
;;; ----------------------------------------------------------------------

;; This simply adds syntax highlighting and support for `.fish` files,
;; which is useful since we are setting Fish as our default shell below.
(use-package fish-mode
  :ensure t)

;;; ----------------------------------------------------------------------
;;; Vterm (The Terminal Emulator)
;;; ----------------------------------------------------------------------

(use-package vterm
  :ensure t
  ;; `:init` block runs *before* the package is loaded.
  ;; These are settings that need to be in place early.
  :init
  ;; When copying text from vterm, *include* the shell prompt
  ;; (e.g., "user@host> "). Set to `t` to exclude the prompt.
  (setq vterm-copy-exclude-prompt nil)

  ;; **IMPORTANT**: Vterm relies on a compiled C module for its speed.
  ;; This line tells Emacs to *always* try to compile this module
  ;; if it's not already compiled. This is crucial for vterm to work.
  (setq vterm-always-compile-module t)

  ;; Set the number of lines of history to keep in the terminal buffer.
  (setq vterm-max-scrollback 20000)

  ;; Set the timer delay for responsiveness. Lower values can make
  ;; the terminal feel "snappier" but may use more CPU.
  (setq vterm-timer-delay 0.01)

  ;; Teach vterm which shells to try when connecting to a remote
  ;; machine using Emacs's TRAMP (Transparent Remote Access, e.g., via SSH).
  (setq vterm-tramp-shells
        '("/usr/bin/bash" "/bin/bash" "/bin/zsh" "docker" "/bin/sh"))

  ;; `:bind` sets up keybindings that are active *only* in vterm buffers.
  :bind (:map vterm-mode-map
              ;; **CRUCIAL OVERRIDE**:
              ;; By default, `C-y` in Emacs yanks from the Emacs "kill-ring".
              ;; In a terminal, you expect `C-y` (or more commonly Cmd-V/Ctrl-Shift-V)
              ;; to paste from the *system clipboard*.
              ;; `vterm-yank` is the function that does this.
              ("C-y" . vterm-yank))

  ;; `:config` block runs *after* the package is loaded.
  :config
  ;; Set the default shell for vterm to use.
  ;; `(executable-find "fish")` finds the path to the 'fish' shell.
  (setq vterm-shell (executable-find "fish"))

  ;; This is an advanced feature. It lets vterm intercept shell
  ;; commands and run Emacs Lisp functions instead.
  ;; For example, if you (or a script) output a special command,
  ;; Emacs can run `(dired "/some/path")` instead of the shell.
  (setq vterm-eval-cmds '(("find-file" find-file)
                          ("message" message)
                          ("vterm-clear-scrollback" vterm-clear-scrollback)
                          ("dired" dired)
                          ("ediff-files" ediff-files)))

  ;; Automatically kill the vterm buffer (close the tab) when you
  ;; type `exit` in the shell and the process finishes.
  (setq vterm-kill-buffer-on-exit t)

  ;;; --- Custom Vterm Kill Function (M-k) ---
  ;;
  ;; This section creates a "force-kill" keybinding (`M-k` or Alt-k)
  ;; for vterm. By default, Emacs will ask "Process is running, kill it?"
  ;; This bypasses that check for a faster workflow.

  ;; 1. A helper function to kill a buffer AND any windows displaying it.
  (defun kill-buffer-and-its-windows (buffer)
    "Kill BUFFER and delete its windows. Default is `current-buffer'."
    (interactive (list
                  (read-buffer "Kill buffer: "
                               (current-buffer)
                               'existing)))
    (setq buffer (get-buffer buffer))
    (if (buffer-live-p buffer) ; Kill live buffer only.
        (let ((wins (get-buffer-window-list buffer nil t))) ; On all frames.
          (when (and (buffer-modified-p buffer)
                     (fboundp '1on1-flash-ding-minibuffer-frame))
            (1on1-flash-ding-minibuffer-frame t))
          (when (kill-buffer buffer)
            (dolist (win wins)
              (when (window-live-p win)
                (condition-case nil
                    (delete-window win)
                  (error nil))))))
      (when (interactive-p)
        (error "Cannot kill buffer. Not a live buffer: `%s'"
               buffer))))

  ;; 2. The keybinding itself.
  (define-key
   vterm-mode-map (kbd "M-k")
   ;; We bind `M-k` to an anonymous `lambda` (inline) function.
   (lambda ()
     (interactive)
     (let ((buffer (current-buffer)))
       ;; This is the magic part: It tells Emacs "Don't ask me
       ;; for confirmation when you kill the process for this buffer."
       (set-process-query-on-exit-flag (get-buffer-process buffer) nil)
       ;; Now, call our helper to kill the buffer and its window.
       (kill-buffer-and-its-windows buffer))))

  ;;; --- Other Vterm Configurations ---

  ;; Enable `goto-address-mode` in vterm.
  ;; This makes file paths (like "/foo/bar.txt:10") and URLs
  ;; (like "http://google.com") clickable inside the terminal!
  (add-hook 'vterm-mode-hook 'goto-address-mode))

;;; ----------------------------------------------------------------------
;;; Vterm-Toggle (Pop-up "Quake-style" Terminal)
;;; ----------------------------------------------------------------------

(use-package vterm-toggle
  ;; `:after vterm` ensures that `vterm` is loaded *before* we
  ;; try to configure this add-on package.
  :after vterm
  :ensure t
  :config
  ;; `dedicated` means one vterm buffer is shared across all windows.
  (setq vterm-toggle-scope 'dedicated)

  ;; **AWESOME FEATURE**: When opening the terminal, automatically
  ;; `cd` to the root of the current project (e.g., the git repo root).
  (setq vterm-toggle-project-root t)

  ;; Don't automatically create a new vterm buffer if we're not
  ;; in a project.
  (setq vterm-toggle-cd-auto-create-buffer nil)

  ;; When hiding the vterm, reset the window configuration.
  (setq vterm-toggle-reset-window-configuration-after-exit t)

  ;; Don't open the vterm in fullscreen.
  (setq vterm-toggle-fullscreen-p nil)

  ;; How to hide the window.
  (setq vterm-toggle-hide-method 'delete-window)

  ;; --- Vterm Window Display Rules ---
  ;; This is a complex Elisp rule that tells Emacs *how* to
  ;; display the vterm-toggle buffer.
  ;;
  ;; **The TL;DR for newbies**: "When vterm-toggle runs, pop up a
  ;; window at the *bottom* of the screen, and make it take up 30%
  ;; of the window height."
  (add-to-list
   'display-buffer-alist
   '(;; 1. The "condition": IF the buffer is a vterm-mode buffer...
     (lambda (buffer-or-name _)
       (let ((buffer (get-buffer buffer-or-name)))
         (with-current-buffer buffer
           (or (equal major-mode 'vterm-mode)
               (string-prefix-p
                vterm-buffer-name (buffer-name buffer))))))
     ;; 2. The "action": Display it using these rules...
     (display-buffer-reuse-window display-buffer-in-direction)
     (direction . bottom)    ;; ...at the bottom.
     (dedicated . t)         ;; ...make the window dedicated to this buffer.
     (reusable-frames . visible)
     (window-height . 0.3)   ;; ...at 30% height.
     (window-width . 0.3)))  ;; (width is less relevant for a bottom-pop-up)

  ;;; --- Vterm-Toggle Project Root Overrides ---
  ;;
  ;; The following code *overrides* the default functions in
  ;; `vterm-toggle` to use a more robust project-finding method.
  ;; This is advanced Elisp, but the goal is to make finding the
  ;; "project root" work better (e.g., in git submodules).

  ;; 1. Our custom, "smarter" project root finding function.
  (defun my/get-project-root ()
    "Find the project root using multiple methods, robustly."
    (or
     ;; Method 1: Try Emacs's built-in `project.el`.
     (when (fboundp 'project-root)
       (when-let* ((project (project-current nil)))
         (project-root project)))
     ;; Method 2: Try the popular `projectile` package.
     (when (fboundp 'projectile-project-root)
       (projectile-project-root))
     ;; Method 3: Fallback to finding the nearest `.git` directory.
     (when-let ((git-dir (locate-dominating-file default-directory ".git")))
       (expand-file-name git-dir))
     ;; Method 4: If all else fails, just use the current directory.
     default-directory))

  ;; 2. Override the `vterm-toggle` function for creating a new terminal.
  ;;    This is a *direct re-definition* of the function.
  (defun vterm-toggle--new (&optional buffer-name)
    "New vterm buffer. (OVERRIDDEN to use `my/get-project-root`)."
    (let* ((buffer-name (or buffer-name vterm-buffer-name))
           (default-directory
            (if vterm-toggle-project-root
                ;; Use OUR function here instead of the default.
                (my/get-project-root)
              default-directory)))
      ;; (The rest of the function is as normal)
      (if vterm-toggle-fullscreen-p
          (vterm buffer-name)
        (if (eq major-mode 'vterm-mode)
            (let ((display-buffer-alist nil))
              (vterm buffer-name))
          (vterm-other-window buffer-name)))))

  ;; 3. Override the internal helper function as well.
  (defun vterm-toggle--project-root ()
    "Get project root. (OVERRIDDEN to use `my/get-project-root`)."
    (my/get-project-root)))

;;; ----------------------------------------------------------------------
;;; Global Keybinding for Vterm-Toggle
;;; ----------------------------------------------------------------------

;; Finally, bind the vterm-toggle command to a global key.
;; `s-9` means Super(Cmd/Win) + 9.
(global-set-key (kbd "s-9") 'vterm-toggle)

;;; ----------------------------------------------------------------------
;;; Multiple Vterm Management
;;; ----------------------------------------------------------------------

;; `multi-vterm` is a companion package that makes it easier to
;; create and manage *multiple* vterm buffers. For example, it provides
;; commands to quickly create a new vterm (`multi-vterm`), switch to the
;; next/previous vterm, or open a vterm at the project root.
(use-package multi-vterm :ensure t)

;;; ----------------------------------------------------------------------
;;; Finalize
;;; ----------------------------------------------------------------------

;; This is the standard "magic line" for an Emacs Lisp module.
;; It tells Emacs that the file `terminal.el` has been loaded successfully,
;; which allows `(require 'terminal)` in your `init.el` to work.
(provide 'terminal)

;;; terminal.el ends here
