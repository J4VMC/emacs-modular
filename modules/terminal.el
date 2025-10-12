;;; terminal.el -- Settings for the terminal -*- lexical-binding: t; -*-
;;; Commentary:
;; This file contains all settings relevant to the terminal
;;; Code:

(use-package fish-mode
  :ensure t)

(use-package vterm
  :ensure t
  :init
  (setq vterm-copy-exclude-prompt nil)
  (setq vterm-always-compile-module t)
  (setq vterm-max-scrollback 20000)
  (setq vterm-timer-delay 0.01)
  (setq vterm-tramp-shells
	'("/usr/bin/bash" "/bin/bash" "/bin/zsh" "docker" "/bin/sh"))
  :bind (:map vterm-mode-map
	      ("s-p" . vterm-yank))
  :config
  (setq vterm-shell (executable-find "fish"))
  (setq vterm-eval-cmds '(("find-file" find-file)
                          ("message" message)
                          ("vterm-clear-scrollback" vterm-clear-scrollback)
                          ("dired" dired)
                          ("ediff-files" ediff-files)))
  (setq vterm-kill-buffer-on-exit t)

  ;; --- Custom kill buffer function ---
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

  ;; --- Keybinding for M-k ---
  (define-key
   vterm-mode-map (kbd "M-k")
   (lambda ()
     (interactive)
     (let ((buffer (current-buffer)))
       (set-process-query-on-exit-flag (get-buffer-process buffer) nil)
       (kill-buffer-and-its-windows buffer))))

  ;; --- Other configurations ---
  (add-hook 'vterm-mode-hook 'goto-address-mode))

(use-package vterm-toggle
  :after vterm
  :ensure t
  :config
  (setq vterm-toggle-scope 'dedicated)
  (setq vterm-toggle-project-root t)
  (setq vterm-toggle-cd-auto-create-buffer nil)
  (setq vterm-toggle-reset-window-configration-after-exit t)
  (setq vterm-toggle-fullscreen-p nil)
  (setq vterm-toggle-hide-method 'bury-all-vterm-buffer)
  (add-to-list
   'display-buffer-alist
   '((lambda (buffer-or-name _)
       (let ((buffer (get-buffer buffer-or-name)))
         (with-current-buffer buffer
           (or (equal major-mode 'vterm-mode)
	       (string-prefix-p
                vterm-buffer-name (buffer-name buffer))))))
     (display-buffer-reuse-window display-buffer-in-direction)
     (direction . bottom)
     (dedicated . t)
     (reusable-frames . visible)
     (window-height . 0.3)
     (window-width . 0.3)))

  ;; Function to get project root without prompting
  (defun my/get-project-root ()
    (or (when (fboundp 'project-root)
          (when-let* ((project (project-current nil)))
            (project-root project)))
        (when (fboundp 'projectile-project-root)
          (projectile-project-root))
        ;; Fallback: look for nearest .git directory (handles submodules)
        (when-let ((git-dir (locate-dominating-file default-directory ".git")))
          (expand-file-name git-dir))
        default-directory))

  ;; Override vterm-toggle--new
  (defun vterm-toggle--new (&optional buffer-name)
    "New vterm buffer."
    (let* ((buffer-name (or buffer-name vterm-buffer-name))
           (default-directory
            (if vterm-toggle-project-root
                (my/get-project-root)
              default-directory)))
      (if vterm-toggle-fullscreen-p
          (vterm buffer-name)
        (if (eq major-mode 'vterm-mode)
            (let ((display-buffer-alist nil))
              (vterm buffer-name))
          (vterm-other-window buffer-name)))))

  ;; Override vterm-toggle--project-root
  (defun vterm-toggle--project-root ()
    (my/get-project-root)))


(global-set-key (kbd "s-9") 'vterm-toggle)

(provide 'terminal)
;;; terminal.el ends here
