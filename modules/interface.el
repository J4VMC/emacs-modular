;;; interface.el -- User interface customizations -*- lexical-binding: t; -*-

;;; Commentary:
;; This file handles the main visual aspects of the Emacs interface.
;; It configures the theme (gruvbox), the startup screen (dashboard),
;; the mode-line (telephone-line), and other core UI elements like
;; the cursor, line numbers, and scrolling behavior.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --- Core UI Tweaks ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'default-frame-alist '(alpha-background . 70))

;; font size
(set-face-attribute 'default nil :height 150)

;; Use a horizontal bar for the cursor instead of the default block.
(setq-default cursor-type 'hbar)

;; Show column numbers in the mode line.
(column-number-mode 1)

;; Use the modern, built-in line numbering mode. This is preferred over the older `line-number-mode`.
(global-display-line-numbers-mode 1)

;; Use a more aesthetically pleasing underline style for links and highlighted text.
(setq-default x-underline-at-descent-line t)

;; Disable the audible bell and use a silent, visual flash instead.
(setq visible-bell t)

;; Use "y" or "n" for prompts instead of typing out "yes" or "no".
(setq use-short-answers t)

;; In Emacs 29+, enable pixel-perfect smooth scrolling.
;; We wrap this in a version check to prevent errors on older Emacs versions.
(when (>= emacs-major-version 29)
  (pixel-scroll-precision-mode 1))

;; Make right-click open a context menu in graphical Emacs.
(when (display-graphic-p)
  (context-menu-mode 1))

;; Enable the windmove commands globally
(global-set-key (kbd "s-<left>") 'windmove-left)
(global-set-key (kbd "s-<right>") 'windmove-right)
(global-set-key (kbd "s-<up>") 'windmove-up)
(global-set-key (kbd "s-<down>") 'windmove-down)

;; Enable horizontal scrolling
(setq mouse-wheel-tilt-scroll t)
(setq mouse-wheel-flip-direction t)

;; Show the tab-bar as soon as tab-bar functions are invoked
(setq tab-bar-mode 1)
(setq tab-bar-show 1)
(global-set-key (kbd "s-t") 'tab-new)
(global-set-key (kbd "s-l") 'tab-close)

;; Add the time to the tab-bar, if visible
(add-to-list 'tab-bar-format 'tab-bar-format-align-right 'append)
(add-to-list 'tab-bar-format 'tab-bar-format-global 'append)
(setq display-time-format "%a %F %T")
(setq display-time-interval 1)
(display-time-mode)

;; Highlight changed lines
(use-package line-reminder
  :ensure t
  :config
  (setq global-line-reminder-mode t))

;; Preview colors
(use-package colorful-mode
  :diminish
  :ensure t
  :custom
  (colorful-use-prefix t)
  (colorful-only-strings 'only-prog)
  (css-fontify-colors nil)
  :config
  (global-colorful-mode t)
  (add-to-list 'global-colorful-modes 'helpful-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --- Icons ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package all-the-icons
  :ensure t)

(use-package all-the-icons-completion
  :ensure t
  :after (marginalia)
  :config
  ;; Adds icons to minibuffer completions via Marginalia.
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup))

(use-package all-the-icons-dired
  :ensure t
  :hook
  ;; Adds file and directory icons to dired.
  (Dired-mode . all-the-icons-dired-mode))

(use-package kind-icon
  :ensure t
  :after corfu
  ;; Note: The actual integration is handled in the `corfu` config in tools.el
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --- Dashboard ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set up the startup screen with recent files and projects.
(use-package dashboard
  :ensure t
  :after project
  :custom
  (dashboard-startupify-list '(dashboard-insert-banner
                               dashboard-insert-newline
                               dashboard-insert-items))
  (dashboard-startup-banner 'logo)
  (dashboard-center-content t)
  (dashboard-vertically-center-content t)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-projects-backend 'project-el)
  (dashboard-projects-backend-switch-function 'project-switch-project)
  (dashboard-items '((recents . 5)
                     (projects . 5)))
  (dashboard-show-shortcuts nil)
  (dashboard-set-footer nil)
  :config
  :config
  (dashboard-setup-startup-hook)
  ;; Only show dashboard when starting Emacs without arguments
  (setq initial-buffer-choice
	(lambda ()
          (if (> (length command-line-args) 1)
              nil  ; Don't show dashboard if files were specified
            (dashboard-open))))
  (add-hook 'dashboard-after-initialize-hook
            (lambda ()
              (when (get-buffer "*scratch*")
		(kill-buffer "*scratch*")))))

(defun my-dashboard-hide-cursor ()
  "Hide the cursor in the dashboard buffer."
  ;; Set the cursor type to nil to make it disappear
  (setq-local cursor-type nil)
  ;; Also, disable cursor blinking locally
  (setq-local blink-cursor-mode nil))

;; Add the function to the hook that runs when dashboard loads
(add-hook 'dashboard-mode-hook #'my-dashboard-hide-cursor)

;; --- 1. The Keymap: Disable all movement and scrolling keys ---
;; Replace your old `dashboard-lock-keymap` variable with this one.
(defvar dashboard-lock-keymap
  (let ((map (make-sparse-keymap)))
    ;; Disable all common keyboard and mouse scrolling methods
    (define-key map (kbd "<up>") 'ignore)
    (define-key map (kbd "<down>") 'ignore)
    (define-key map (kbd "<left>") 'ignore)
    (define-key map (kbd "<right>") 'ignore)
    (define-key map (kbd "<prior>") 'ignore)
    (define-key map (kbd "<next>") 'ignore)
    (define-key map (kbd "C-v") 'ignore)
    (define-key map (kbd "M-v") 'ignore)
    (define-key map (kbd "<wheel-up>") 'ignore)
    (define-key map (kbd "<wheel-down>") 'ignore)
    
    ;; --- The Missing Lines ---
    ;; These disable horizontal scrolling from touchpads and tilt-wheels.
    (define-key map (kbd "<wheel-left>") 'ignore)
    (define-key map (kbd "<wheel-right>") 'ignore)
    
    (define-key map (kbd "M-<") 'ignore)
    (define-key map (kbd "M->") 'ignore)
    map)
  "A keymap to disable all scrolling and movement for the dashboard.")

;; --- 2. The Minor Mode: A simple 'lockdown' mode ---
;; This packages our keymap into a toggleable mode.
(define-minor-mode dashboard-lock-mode
  "A minor mode to completely lock the dashboard view by disabling movement keys."
  :init-value nil
  :lighter " Lock"
  :keymap dashboard-lock-keymap)

;; --- 3. The Setup Function and Hook ---
;; This function runs when the dashboard opens to prepare the window.
(defun setup-dashboard-lock ()
  "Prepare the window and activate the lock mode."
  ;; Set the static window properties
  (set-window-scroll-bars (selected-window) nil nil)
  (set-window-hscroll (selected-window) 0)
  (setq-local truncate-lines t)
  
  ;; Turn on our new lockdown mode
  (dashboard-lock-mode 1))

;; --- 4. Activation ---
;; This ensures a clean slate by removing any of our old, failed attempts.
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

;; A stylish and configurable mode-line.
(use-package telephone-line
  :ensure t
  :demand t
  :custom
  (telephone-line-lhs
   '((evil   . (telephone-line-vc-segment))
     (accent . (telephone-line-project-segment
		telephone-line-process-segment))
     (nil    . (telephone-line-minor-mode-segment
		telephone-line-buffer-segment))))
  (telephone-line-rhs
   '((nil    . (telephone-line-atom-encoding-segment))
     (accent . (telephone-line-flycheck-segment
		telephone-line-major-mode-segment))
     (evil   . (telephone-line-misc-info-segment))))
  (telephone-line-height 24)
  :config
  (telephone-line-mode 1))

(use-package helpful
  :ensure t
  :defer t)

;; Note that the built-in `describe-function' includes both functions
;; and macros. `helpful-function' is functions only, so we provide
;; `helpful-callable' as a drop-in replacement.
(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-c C-d") #'helpful-at-point)
(global-set-key (kbd "C-h F") #'helpful-function)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
(global-set-key (kbd "C-h x") #'helpful-command)

(provide 'interface)
;;; interface.el ends here
