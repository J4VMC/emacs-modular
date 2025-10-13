;;; projects.el -- project-related settings -*- lexical-binding: t; -*-

;;; Commentary:
;; This file configures packages for project management and navigation.
;; It sets up Projectile for project-level commands, Treemacs for a file
;; drawer, and the modern Tree-sitter parsing system for enhanced syntax
;; highlighting and code intelligence.

;;; Code:

(use-package project
  :demand t
  :ensure nil
  :bind-keymap ("C-c p" . project-prefix-map)
  :config
  (setq project-vc-include-untracked t)
  
  ;; Set compilation buffer naming
  (setq project-compilation-buffer-name-function
        (lambda (mode)
          (format "*%s-compilation*" (project-name (project-current)))))
  ;; =============================================================================
  ;; PROJECT DETECTION CONFIGURATION
  ;; =============================================================================
  ;; This configuration handles two scenarios:
  ;; 1. Git submodules (like all/.emacs.d) - should be treated as separate projects
  ;; 2. Monorepo subprojects (marked with .project files) - should be treated as separate projects
  ;;
  ;; Both use 'vc project type to respect .gitignore files and show only Git-tracked files.

  ;; Configure built-in project.el to recognize .project files as project boundaries
  ;; This handles monorepo subprojects where you want to limit scope within a larger Git repo
  (setq project-vc-extra-root-markers '(".project" "pyproject.toml" ".projectile" "package.json"))

  ;; Custom function to detect Git submodules specifically
  ;; Submodules have .git files (not directories), so they need special handling
  ;; This runs BEFORE the built-in project-try-vc to catch submodules first
  (defun my/project-try-submodule (dir)
    "Detect Git submodules by checking for .git files (not directories).

   This function:
   - Only triggers for actual Git submodules (where .git is a file, not directory)
   - Returns 'vc project type to respect .gitignore files
   - Prevents submodules from being detected as part of their parent repository

   Used for cases like all/.emacs.d submodule in the dotfiles repo."
    (when-let ((git-file (locate-dominating-file dir ".git")))
      (let ((git-path (expand-file-name ".git" git-file)))
	;; Only treat as project if .git is a file (submodule) not a directory
	(when (file-regular-p git-path)
          ;; Return the same format as project-try-vc: (vc backend root-dir)
          (list 'vc 'Git (expand-file-name git-file))))))

  ;; Add submodule detection to run before the built-in project-try-vc
  ;; Order matters: submodule detection must run first to catch submodules
  ;; before project-try-vc finds the parent repository
  (add-hook 'project-find-functions #'my/project-try-submodule)

  ;; =============================================================================
  ;; SUMMARY: How this works
  ;; =============================================================================
  ;; 1. my/project-try-submodule runs first, catches Git submodules (.git files)
  ;; 2. project-try-vc runs second, handles normal repos + .project file markers
  ;; 3. Both return 'vc projects that respect .gitignore files
  ;; 4. Result: Fast, correct project boundaries for both submodules and monorepos
  ;;
  ;; Examples:
  ;; - all/.emacs.d (submodule): Detected by .git file, scoped to submodule
  ;; - monorepo/subproject (with .project): Detected by .project file, scoped to subdir
  ;; =============================================================================

  ;; Set up project-vc-ignores with proper patterns
  (setq project-vc-ignores
	'("venv/"
          ".venv/"
          "env/"
          ".env/"
          "typings/"
          "node_modules/"
          ".mypy_cache/"
          ".pytest_cache/"
          ".cache/"
          ".dvc/cache/"
          ".dvc/tmp/"
          ".jekyll-cache/"
          "__pycache__/"
          "dist/"
          "build/"
          ".git/"
          "!*.org"
          "!/notes/"))

  ;; Function to add ignored directories to project-vc-ignores
  (defun my/add-project-ignore (dir)
    "Add DIR to the list of ignored directories in project-vc-ignores."
    (add-to-list 'project-vc-ignores dir))

  ;; Add additional common ignore patterns
  (dolist (pattern
           '("*/__pycache__/"
             "*/node_modules/"
             "*/.git/"
             "*/dist/"
             "*/build/"
             "*/.tox/"
             "*/.coverage"
             "*/htmlcov/"
             "*.pyc"
             "*.pyo"
             "*.egg-info/"))
    (my/add-project-ignore pattern)))

(use-package treemacs
  :ensure t
  :defer t
  :bind
  (("s-0" . treemacs-select-window)
   ("C-c t d" . treemacs-select-directory))
  :config
  ;; Configure single-click behavior
  (with-eval-after-load 'treemacs
    (define-key treemacs-mode-map [mouse-1] 'treemacs-single-click-expand-action))
  
  ;; Core settings
  (setq treemacs-collapse-dirs (if treemacs-python-executable 3 0)
        treemacs-deferred-git-apply-delay 0.5
        treemacs-display-in-side-window t
        treemacs-eldoc-display 'simple
        treemacs-file-event-delay 2000
        treemacs-file-follow-delay 0.2
        treemacs-follow-after-init t
        treemacs-git-command-pipe ""
        treemacs-goto-tag-strategy 'refetch-index
        treemacs-indentation 2
        treemacs-max-git-entries 5000
        treemacs-missing-project-action 'ask
        treemacs-no-delete-other-windows t  ;; Keep treemacs visible
        treemacs-persist-file (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
        treemacs-litter-directories '("/node_modules" "/.venv" "/.cask" "/vendor")
        treemacs-position 'left
        treemacs-show-hidden-files t
        treemacs-sorting 'alphabetic-asc
        treemacs-tag-follow-delay 1.5
        treemacs-width-is-initially-locked nil
        treemacs-width 50
	treemacs-width-increment 1
        treemacs-workspace-switch-cleanup nil)  ;; Don't close projects when switching workspaces

  ;; Enable useful modes
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)
  
  ;; Git integration (conditional based on available tools)
  (pcase (cons (not (null (executable-find "git")))
               (not (null treemacs-python-executable)))
    (`(t . t) (treemacs-git-mode 'deferred))
    (`(t . _) (treemacs-git-mode 'simple))))

;; Adds file icons to Dired, matching the icons used in Treemacs.
(use-package treemacs-icons-dired
  :ensure t
  :hook (dired-mode . treemacs-icons-dired-enable-once))

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-tab-bar ; treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))

;; Create a custom keymap for project/jump commands
(define-prefix-command 'my-jump-map)
(global-set-key (kbd "s-p") 'my-jump-map)

;; Project commands under s-p
(with-eval-after-load 'project
  (define-key my-jump-map (kbd "p") 'project-switch-project)
  (define-key my-jump-map (kbd "f") 'project-find-file)
  (define-key my-jump-map (kbd "b") 'project-switch-to-buffer)
  (define-key my-jump-map (kbd "d") 'project-dired)
  (define-key my-jump-map (kbd "g") 'project-find-regexp)
  (define-key my-jump-map (kbd "c") 'project-compile))

;; Treemacs commands under s-p
(with-eval-after-load 'treemacs
  (define-key my-jump-map (kbd "t") 'treemacs)
  (define-key my-jump-map (kbd "0") 'treemacs-select-window)
  (define-key my-jump-map (kbd "a") 'treemacs-add-and-display-current-project-exclusively))

;; Vterm under s-p (to complement your s-9 binding)
(with-eval-after-load 'vterm-toggle
  (define-key my-jump-map (kbd "v") 'vterm-toggle))

(provide 'projects)
;;; projects.el ends here
