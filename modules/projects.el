;;; projects.el -- project-related settings -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file configures everything related to "projects."
;; In Emacs, a project is typically a directory containing a
;; version control folder (like `.git`).
;;
;; This file sets up two key packages:
;;
;; 1.  **Projectile** 🚀: This is the "brains" of project management.
;;     It's a non-visual package that *knows* what your project is,
;;     finds its root directory, and provides commands to run
;;     *on* that project (e.g., "find file in project," "search in project").
;;
;; 2.  **Treemacs** 🌳: This is the "UI" of project management.
;;     It provides the visual "file drawer" or "sidebar" that
;;     you see in other editors, letting you browse your project's
;;     file tree with the mouse or keyboard.
;;
;; We also set up a custom keymap to group all our project-related
;; commands under a single prefix (`s-p` or Super-p).

;;; Code:

;;; ----------------------------------------------------------------------
;;; Projectile (Project Management "Brains")
;;; ----------------------------------------------------------------------

(use-package projectile
  :ensure t
  :init
  ;; This runs *before* Projectile is loaded.

  ;; Tell Projectile NOT to bind "C-c p"
  (setq projectile-enable-keymap nil)

  ;; Tell Projectile where to look for your projects.
  ;; It will scan these directories on startup.
  (setq projectile-project-search-path '("~/Projects")
        ;; **PERFORMANCE TWEAK**:
        ;; Use `rg` (ripgrep) to find all files in the project.
        ;; This is *much* faster than the default Emacs `find` command.
        projectile-generic-command "rg -0 --files --color=never --hidden --glob !.git/ --max-filesize 1M"
        ;; **PERFORMANCE TWEAK**:
        ;; Also use `rg` for project-wide *searching* (grepping).
        projectile-grep-command "rg -n --with-filename --no-heading --max-columns=150 --ignore-case --max-filesize 1M --glob !.git/"
        ;; Cache the list of project files for even more speed.
        projectile-enable-caching t)
  :config
  ;; This runs *after* Projectile is loaded.
  (projectile-mode 1)) ; Turn on Projectile globally.

;;; ----------------------------------------------------------------------
;;; Treemacs (File Tree "UI")
;;; ----------------------------------------------------------------------

(use-package treemacs
  :ensure t
  ;; `:defer t` is a performance optimization. It means
  ;; "don't load this package *until* it's actually called."
  :defer t
  :bind
  ;; `s-0` (Super-0) is a common key to focus the file tree.
  (("s-0" . treemacs-select-window)
   ;; `C-c t d` to add the current directory to Treemacs as a project.
   ("C-c t d" . treemacs-select-directory))
  :config
  ;; This makes Treemacs behave more like a standard file explorer:
  ;; *single-clicking* a folder will expand/collapse it.
  ;; (The default is double-click).
  (with-eval-after-load 'treemacs
    (define-key treemacs-mode-map [mouse-1] 'treemacs-single-click-expand-action))

  ;; --- Core Treemacs Settings ---
  (setq treemacs-collapse-dirs (if treemacs-python-executable 3 0)
        treemacs-deferred-git-apply-delay 0.5
        ;; Show the Treemacs window on the side (in a "side window").
        treemacs-display-in-side-window t
        treemacs-eldoc-display 'simple
        treemacs-file-event-delay 2000
        treemacs-file-follow-delay 0.2
        ;; Auto-highlight the currently active file in the tree.
        treemacs-follow-after-init t
        treemacs-git-command-pipe ""
        treemacs-goto-tag-strategy 'refetch-index
        treemacs-indentation 2 ; Indent folders by 2 spaces
        treemacs-max-git-entries 5000
        treemacs-missing-project-action 'ask
        ;; **IMPORTANT**: Keep Treemacs open!
        ;; This prevents the Treemacs window from closing when
        ;; you select a file.
        treemacs-no-delete-other-windows t
        ;; Where to save the Treemacs session (which folders are open).
        treemacs-persist-file (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
        ;; **IMPORTANT**: A list of "junk" directories to *hide*
        ;; from the file tree. This keeps it clean!
        treemacs-litter-directories '("/node_modules" "/.venv" "/.cask" "/vendor")
        ;; Show the tree on the 'left' side of the Emacs window.
        treemacs-position 'left
        treemacs-show-hidden-files t ; Show hidden files (like .gitignore)
        treemacs-sorting 'alphabetic-asc
        treemacs-tag-follow-delay 1.5
        treemacs-width-is-initially-locked nil
        treemacs-width 50 ; Set the sidebar width to 50 columns
        treemacs-width-increment 1
        ;; Don't close projects when switching workspaces.
        treemacs-workspace-switch-cleanup nil)

  ;; --- Enable Treemacs Modes ---

  ;; "Follow mode" - automatically highlights the current file in the tree.
  (treemacs-follow-mode t)
  ;; "Filewatch mode" - automatically updates the tree if files
  ;; are changed on disk (e.g., by `git pull`).
  (treemacs-filewatch-mode t)
  ;; Show the little triangle icons for expanding/collapsing folders.
  (treemacs-fringe-indicator-mode 'always)

  ;; This complex-looking block just intelligently enables Git
  ;; integration. It will show file statuses (new, modified,
  ;; untracked) as colors/icons in the file tree.
  (pcase (cons (not (null (executable-find "git")))
	       (not (null treemacs-python-executable)))
    (`(t . t) (treemacs-git-mode 'deferred))
    (`(t . _) (treemacs-git-mode 'simple))))

;;; ----------------------------------------------------------------------
;;; Treemacs Integrations (Add-ons)
;;; ----------------------------------------------------------------------

;; This makes Dired (the *other* Emacs file manager) use the
;; *same* pretty icons that Treemacs uses, for a consistent look.
(use-package treemacs-icons-dired
  :ensure t
  :hook (dired-mode . treemacs-icons-dired-enable-once))

;; "Glue" package to integrate Treemacs with Magit (our Git client).
(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

;; **CRITICAL**: This is the "glue" package that connects
;; Treemacs (the UI) with Projectile (the brains).
;; This lets Treemacs *know* what your current project is.
(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

;; Integrates Treemacs with Emacs's built-in `tab-bar-mode`.
;; This gives each "tab" its own Treemacs project scope.
(use-package treemacs-tab-bar ; treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))

;;; ----------------------------------------------------------------------
;;; Custom "Jump" Keymap (s-p)
;;; ----------------------------------------------------------------------
;;
;; This is a very clean setup. We are creating our *own* keymap
;; prefix, `s-p` (Super-p), to group *all* project-related
;; commands, whether they come from `project`, `treemacs`, or `vterm`.

;; 1. Define a new, empty "prefix" keymap called `my-jump-map`.
(define-prefix-command 'my-jump-map)
;; 2. Bind this new prefix map to the `s-p` keystroke.
(global-set-key (kbd "s-p") 'my-jump-map)

;; 3. Add keybindings to our new map...
;; ...but only *after* the `projectile` package has loaded.
(with-eval-after-load 'projectile ;; <-- Must be 'projectile
  ;; `s-p p` -> Switch project
  (define-key my-jump-map (kbd "p") 'projectile-switch-project)
  ;; `s-p f` -> Find file in project
  (define-key my-jump-map (kbd "f") 'projectile-find-file)
  ;; `s-p b` -> Switch to a buffer from this project
  (define-key my-jump-map (kbd "b") 'projectile-switch-to-buffer)
  ;; `s-p d` -> Open Dired (file manager) at project root
  (define-key my-jump-map (kbd "d") 'projectile-dired)
  ;; `s-p g` -> Grep (search) in project
  (define-key my-jump-map (kbd "g") 'projectile-grep)
  ;; `s-p c` -> Compile project
  (define-key my-jump-map (kbd "c") 'projectile-compile-project)
  ;; `s-p r` -> Run vterm at project root (replaces your old C-c p t)
  (define-key my-jump-map (kbd "r") 'projectile-run-vterm))

;; 4. Add Treemacs keybindings to the *same* `s-p` map...
(with-eval-after-load 'treemacs
  ;; `s-p t` -> Toggle Treemacs window
  (define-key my-jump-map (kbd "t") 'treemacs)
  ;; `s-p 0` -> Jump focus to the Treemacs window
  (define-key my-jump-map (kbd "0") 'treemacs-select-window)
  ;; `s-p a` -> Add current project to Treemacs and show it
  (define-key my-jump-map (kbd "d") 'treemacs-add-and-display-current-project-exclusively))

;; 5. Add our vterm toggle to the map as well.
(with-eval-after-load 'vterm-toggle
  ;; `s-p v` -> Toggle the pop-up terminal
  (define-key my-jump-map (kbd "v") 'vterm-toggle))


;;; ----------------------------------------------------------------------
;;; Finalize
;;; ----------------------------------------------------------------------

;; This is the standard "magic line" for an Emacs Lisp module.
;; It tells Emacs that the file `projects.el` has been loaded successfully,
;; which allows `(require 'projects)` in your `init.el` to work.
(provide 'projects)

;;; projects.el ends here
