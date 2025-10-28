;;; interface.el -- User interface customizations -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file handles the main visual "look and feel" of the Emacs interface. 🎨
;; It's all about aesthetics and core UI behavior.
;;
;; We configure:
;; - Core settings like fonts, cursor style, and scrolling.
;; - Icons (using `all-the-icons`) to make things pretty.
;; - The startup screen (`dashboard`) for a modern welcome.
;; - The mode-line/status bar (`telephone-line`).
;; - A better help system (`helpful`).
;;
;; 'lexical-binding: t' is a "magic cookie" that tells Emacs to use a
;; more modern and predictable way of handling variables. It's standard
;; practice to put this on the first line.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --- Core UI Tweaks ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set the window transparency (alpha). 100 is fully opaque, 0 is invisible.
;; This sets it to 70% opacity.
(add-to-list 'default-frame-alist '(alpha-background . 70))

;; Set the default font size. The value '150' means 15.0 points.
(set-face-attribute 'default nil :height 150)

;; Use a thin horizontal bar for the cursor instead of the default block.
;; Other options include 'block', 'box', or '(bar . 5)' (a vertical bar 5 pixels wide).
(setq-default cursor-type 'hbar)

;; Show the current column number in the mode-line (the status bar at the bottom).
(column-number-mode 1)

;; Turn on line numbers globally (in all buffers).
;; This uses the modern, built-in `display-line-numbers-mode`, which is
;; faster and smarter (e.g., can show relative line numbers) than older packages.
(global-display-line-numbers-mode 1)

;; A small tweak to make underlines (like for links or spell-check)
;; render in a more aesthetically pleasing way, respecting font descenders.
(setq-default x-underline-at-descent-line t)

;; Disable the *audible* 'beep' sound (e.g., on error) and
;; use a silent *visual flash* instead.
(setq visible-bell t)

;; When Emacs asks a yes/no question (e.g., "Save file?"),
;; allow us to just press "y" or "n" instead of typing "yes" or "no".
(setq use-short-answers t)

;; In Emacs 29 and newer, enable pixel-perfect smooth scrolling.
;; We wrap this in a version check to prevent errors on older Emacs versions.
(when (>= emacs-major-version 29)
  (pixel-scroll-precision-mode 1))

;; Enable the right-click context menu in graphical Emacs.
(when (display-graphic-p)
  (context-menu-mode 1))

;; Enable `windmove` commands to switch between split windows (panes)
;; using the Super (Cmd/Win) key + Arrow keys.
(global-set-key (kbd "s-<left>") 'windmove-left)
(global-set-key (kbd "s-<right>") 'windmove-right)
(global-set-key (kbd "s-<up>") 'windmove-up)
(global-set-key (kbd "s-<down>") 'windmove-down)

;; Enable horizontal scrolling (e.g., with a trackpad or mouse tilt-wheel).
(setq mouse-wheel-tilt-scroll t)
(setq mouse-wheel-flip-direction t) ; For "natural" scrolling direction

;; Enable Emacs's built-in tab-bar (like browser tabs) at the top.
(setq tab-bar-mode 1)
(setq tab-bar-show 1) ; 1 = show even if there's only one tab
;; Bind Super-t and Super-l to open and close tabs.
(global-set-key (kbd "s-t") 'tab-new)
(global-set-key (kbd "s-l") 'tab-close)

;; --- Add a live clock to the tab-bar ---
;; This is a bit advanced, but it modifies the `tab-bar-format`
;; variable to add a right-aligned time display.
(add-to-list 'tab-bar-format 'tab-bar-format-align-right 'append)
(add-to-list 'tab-bar-format 'tab-bar-format-global 'append)
(setq display-time-format "%a %F %T") ; Format: "Sat 2025-10-25 18:32:13"
(setq display-time-interval 1) ; Update every 1 second
(display-time-mode) ; Turn it on

;; Highlight lines that have been modified (git diff markers in the margin).
(use-package line-reminder
  :ensure t
  :config
  (setq global-line-reminder-mode t))

;; Preview colors (e.g., show "#FFFFFF" as a white-background swatch)
(use-package colorful-mode
  :diminish ; Hides the "Colorful" text from the mode-line
  :ensure t
  :custom
  (colorful-use-prefix t)
  (colorful-only-strings 'only-prog) ; Only highlight in programming modes
  (css-fontify-colors nil) ; Let this package handle it, not css-mode
  :config
  (global-colorful-mode t) ; Turn it on everywhere
  ;; Also enable it in `helpful` (help buffer) mode
  (add-to-list 'global-colorful-modes 'helpful-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --- Icons ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This section installs icon fonts and packages to display them
;; in various parts of Emacs.

;; This is the *core* package that provides all the icons and fonts.
(use-package all-the-icons
  :ensure t)

;; This package *adds* icons to the minibuffer completion UI.
(use-package all-the-icons-completion
  :ensure t
  ;; Load this *after* `marginalia` (our completion annotation package).
  :after (marginalia)
  :config
  ;; This hook is the "glue" that tells Marginalia to use
  ;; `all-the-icons-completion` to add icons.
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup))

;; This package *adds* icons to Dired (the Emacs file manager).
(use-package all-the-icons-dired
  :ensure t
  :hook
  ;; When Dired mode starts, turn on the icon mode.
  (Dired-mode . all-the-icons-dired-mode))

;; This package provides icons for `corfu` (our completion pop-up).
;; It shows a small icon next to each completion candidate
;; (e.g., 🟪 for function, 🟦 for variable).
(use-package kind-icon
  :ensure t
  :after corfu ; Load this after `corfu`
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --- Dashboard ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This section configures a graphical "Welcome" screen for
;; when Emacs starts up, similar to what you'd see in VS Code.

(use-package dashboard
  :ensure t
  :after project ; Make sure `project` is loaded first
  :custom
  ;; Define the *layout* of the dashboard: banner first, then a newline,
  ;; then the list of items.
  (dashboard-startupify-list '(dashboard-insert-banner
                               dashboard-insert-newline
                               dashboard-insert-items))
  ;; Use the built-in Emacs 'logo' as the banner.
  (dashboard-startup-banner 'logo)
  ;; Center the content horizontally and vertically.
  (dashboard-center-content t)
  (dashboard-vertically-center-content t)
  ;; Use icons for item headings (e.g., "Projects")
  (dashboard-set-heading-icons t)
  ;; Use icons for recent files
  (dashboard-set-file-icons t)
  ;; Tell dashboard to get its project list from `projectile`.
  (dashboard-projects-backend 'projectile)
  ;; Define *what* items to show: 5 recent files and 5 projects.
  (dashboard-items '((recents . 5)
                     (projects . 5)))
  ;; Hide the default "Emacs shortcuts" section.
  (dashboard-show-shortcuts nil)
  (dashboard-set-footer nil)
  :config
  ;; This is the main function that enables the dashboard on startup.
  (dashboard-setup-startup-hook)
  ;; Customize the icon for the "Projects" heading.
  (dashboard-modify-heading-icons '((projects . "file-directory")))

  ;; **IMPORTANT**: This is what makes the dashboard the *default* buffer.
  ;; `initial-buffer-choice` tells Emacs what to open on a fresh start.
  ;; We set it to `dashboard-open`...
  (unless (> (length command-line-args) 1)
    ;; ...*unless* we started Emacs with a file argument (e.g., `emacs file.txt`).
    (setq initial-buffer-choice #'dashboard-open))

  ;; A small cleanup hook: when the dashboard loads,
  ;; kill the `*scratch*` buffer, since we don't need it.
  (add-hook 'dashboard-after-initialize-hook
            (lambda ()
              (when (get-buffer "*scratch*")
                (kill-buffer "*scratch*")))))

;; --- Custom Function: Hide Cursor on Dashboard ---
(defun my-dashboard-hide-cursor ()
  "Hide the cursor in the dashboard buffer."
  ;; Set the cursor type to `nil` locally, making it invisible.
  (setq-local cursor-type nil)
  ;; Also, disable blinking just in this buffer.
  (setq-local blink-cursor-mode nil))

;; Run our new function every time `dashboard-mode` is activated.
(add-hook 'dashboard-mode-hook #'my-dashboard-hide-cursor)

;; --- Custom "Lockdown" Mode for Dashboard ---
;;
;; The following code is an advanced setup to make the dashboard
;; *completely* static: non-editable and non-scrollable.
;; We do this by creating our own "minor mode" that disables all
;; movement and scrolling keys.

;; --- 1. The Keymap: Disable all movement and scrolling keys ---
(defvar dashboard-lock-keymap
  (let ((map (make-sparse-keymap)))
    ;; Disable all common keyboard and mouse scrolling methods
    (define-key map (kbd "<up>") 'ignore)
    (define-key map (kbd "<down>") 'ignore)
    (define-key map (kbd "<left>") 'ignore)
    (define-key map (kbd "<right>") 'ignore)
    (define-key map (kbd "<prior>") 'ignore) ; Page Up
    (define-key map (kbd "<next>") 'ignore) ; Page Down
    (define-key map (kbd "C-v") 'ignore)
    (define-key map (kbd "M-v") 'ignore)
    (define-key map (kbd "<wheel-up>") 'ignore)
    (define-key map (kbd "<wheel-down>") 'ignore)
    ;; Disable horizontal scrolling from touchpads and tilt-wheels.
    (define-key map (kbd "<wheel-left>") 'ignore)
    (define-key map (kbd "<wheel-right>") 'ignore)
    ;; Disable go-to-beginning/end-of-buffer
    (define-key map (kbd "M-<") 'ignore)
    (define-key map (kbd "M->") 'ignore)
    map)
  "A keymap to disable all scrolling and movement for the dashboard.")

;; --- 2. The Minor Mode: A simple 'lockdown' mode ---
;; This packages our keymap into a toggleable mode.
(define-minor-mode dashboard-lock-mode
  "A minor mode to completely lock the dashboard view by disabling movement keys."
  :init-value nil
  :lighter " Lock" ; Text to show in the modeline (if not diminished)
  :keymap dashboard-lock-keymap) ; Use the keymap we just defined

;; --- 3. The Setup Function and Hook ---
;; This function runs when the dashboard opens to prepare the window.
(defun setup-dashboard-lock ()
  "Prepare the window and activate the lock mode."
  ;; Set static window properties
  (set-window-scroll-bars (selected-window) nil nil) ; Hide scroll bars
  (set-window-hscroll (selected-window) 0) ; Reset horizontal scroll
  (setq-local truncate-lines t) ; Don't wrap long lines
  ;; Turn on our new lockdown mode
  (dashboard-lock-mode 1))

;; --- 4. Activation ---
;; This `dolist` (do-list) loops through a list of old function
;; names and *removes* them from the hook. This is excellent
;; practice for "cleaning up" past attempts and ensuring
;; *only* our new, correct function runs.
(dolist (hook-func '(dashboard-lock-view-final
                     dashboard-force-reset-view
                     my-dashboard-absolutely-lock-window
                     my-dashboard-lock-window
                     my-dashboard-disable-scrolling
                     dashboard-lock-view-definitively
                     setup-dashboard-lock))
  (remove-hook 'dashboard-mode-hook hook-func))

;; Finally, add the one, correct setup function to the hook.
(add-hook 'dashboard-mode-hook #'setup-dashboard-lock)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --- Mode-Line and Other UI Packages ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A stylish and configurable mode-line (status bar).
(use-package telephone-line
  :ensure t
  :demand t ; Load this immediately, don't lazy-load.
  :custom
  ;; This defines the segments for the Left-Hand Side (lhs)
  (telephone-line-lhs
   '((evil    . (telephone-line-vc-segment)) ; Git branch
     (accent . (telephone-line-project-segment ; Project name
                telephone-line-process-segment)) ; e.g., LSP status
     (nil    . (telephone-line-minor-mode-segment ; Minor modes
                telephone-line-buffer-segment)))) ; File name
  ;; This defines the segments for the Right-Hand Side (rhs)
  (telephone-line-rhs
   '((nil    . (telephone-line-atom-encoding-segment)) ; e.g., UTF-8
     (accent . (telephone-line-flycheck-segment ; Error count
                telephone-line-major-mode-segment)) ; e.g., "Python"
     (evil   . (telephone-line-misc-info-segment)))) ; e.g., line/col
  (telephone-line-height 24)
  :config
  (telephone-line-mode 1)) ; Turn it on

;; `helpful` is a *much* better help system for Emacs.
;; It replaces the default help buffers with a new one that
;; shows the function's source code, keybindings, and properties.
(use-package helpful
  :ensure t
  :defer t) ; Defer loading until it's called

;; --- Rebind Help Keys ---
;; We override the default `C-h` keybindings to use `helpful`.
;; This is a "drop-in" replacement.

;; `C-h f` (describe-function) is replaced by `helpful-callable`
;; (which works for both functions and macros).
(global-set-key (kbd "C-h f") #'helpful-callable)
;; `C-c C-d` is a custom key to show help for the thing *at point*.
(global-set-key (kbd "C-c C-d") #'helpful-at-point)
(global-set-key (kbd "C-h F") #'helpful-function)
;; `C-h v` (describe-variable) is replaced by `helpful-variable`.
(global-set-key (kbd "C-h v") #'helpful-variable)
;; `C-h k` (describe-key) is replaced by `helpful-key`.
(global-set-key (kbd "C-h k") #'helpful-key)
;; `C-h x` (describe-command) is replaced by `helpful-command`.
(global-set-key (kbd "C-h x") #'helpful-command)

;; `dimmer` helps with focus by "dimming" the text of all
;; inactive windows (panes), making the *active* window brighter
;; and more prominent.
(use-package dimmer
  :ensure t
  :config
  ;; This variable is a bit of a misnomer; setting `dimmer-mode` to `t`
  ;; actually *enables* the global minor mode `dimmer-mode`.
  (setq dimmer-mode t))

;;; ----------------------------------------------------------------------
;;; Finalize
;;; ----------------------------------------------------------------------

;; This is the standard "magic line" for an Emacs Lisp module.
;; It tells Emacs that the file `interface.el` has been loaded successfully,
;; which allows `(require 'interface)` in your `init.el` to work.
(provide 'interface)

;;; interface.el ends here
