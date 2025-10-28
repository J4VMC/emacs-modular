;;; tree.el -- Configuration related to Tree-sitter -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file configures Tree-sitter 🌳.
;;
;; What is Tree-sitter?
;; It's a modern system for parsing code. Instead of using complex regular
;; expressions (the "old way"), Tree-sitter builds a complete, accurate
;; "syntax tree" of your code.
;;
;; Why use it?
;; 1.  **Vastly better syntax highlighting**: It understands context, so it
;;     knows the difference between a variable, a function name, and a
;;     parameter, even if they look the same.
;; 2.  **Smarter code navigation**: Allows for commands like "select the
;;     current function" or "go to the next class."
;; 3.  **Better code folding**: Folds code based on its actual structure,
;;     not just indentation.
;;
;; This file will:
;; 1.  Configure Emacs's built-in Tree-sitter support (`treesit`).
;; 2.  Install the "grammars" (parsers) for all the languages we use.
;; 3.  Remap old major modes (e.g., `python-mode`) to their new Tree-sitter
;;     counterparts (e.g., `python-ts-mode`).
;; 4.  Configure add-on packages that use Tree-sitter, like `combobulate`
;;     and `treesit-fold`.

;;; Code:

;;; ----------------------------------------------------------------------
;;; Core Tree-sitter Setup (`treesit`)
;;; ----------------------------------------------------------------------

(use-package treesit
  ;; `:ensure nil` means "don't try to install this package."
  ;; We use this here because `treesit` support is *built-in* to
  ;; modern Emacs (Emacs 29+). We just need to configure it.
  :ensure nil

  ;; `:preface` runs this code *before* the package is configured.
  :preface
  ;; --- Grammar Installation Function ---
  ;; Define a helper function to install all our language grammars.
  ;; Emacs doesn't come with the parsers; we have to download them.
  (defun setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    ;; Loop through our list of desired languages.
    (dolist (grammar
             ;; This is our list of grammars.
             ;; Each entry is `(language-symbol . (github-url optional-sub-directory))`
             '((css . ("https://github.com/tree-sitter/tree-sitter-css"))
               (go . ("https://github.com/tree-sitter/tree-sitter-go"))
               (html . ("https://github.com/tree-sitter/tree-sitter-html"))
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "src"))
               (json . ("https://github.com/tree-sitter/tree-sitter-json"))
               (markdown . ("https://github.com/ikatyang/tree-sitter-markdown"))
               (python . ("https://github.com/tree-sitter/tree-sitter-python"))
               (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
               (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
               (scala . ("https://github.com/tree-sitter/tree-sitter-scala"))
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "tsx/src"))
               (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "typescript/src"))
               (php . ("https://github.com/tree-sitter/tree-sitter-php" "php/src"))
               (sql . ("https://github.com/m-novikov/tree-sitter-sql"))
               (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))))

      ;; 1. Tell Emacs *where* to find this grammar.
      (add-to-list 'treesit-language-source-alist grammar)

      ;; 2. Install the grammar *only if* it's not already installed.
      ;;    This check prevents re-downloading and re-compiling every
      ;;    time Emacs starts.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  ;; --- Major Mode Remapping ---
  ;;
  ;; This is a *key* part of switching to Tree-sitter.
  ;; We tell Emacs: "When you would normally open `python-mode`,
  ;; open `python-ts-mode` instead." This replaces the old modes
  ;; with the new, superior Tree-sitter-based modes.
  (dolist (mapping
           '((python-mode . python-ts-mode)
             (css-mode . css-ts-mode)
             (typescript-mode . typescript-ts-mode)
             ;; We use js-ts-mode instead of the old js2-mode
             (js2-mode . js-ts-mode)
             (bash-mode . bash-ts-mode)
             (conf-toml-mode . toml-ts-mode)
             (go-mode . go-ts-mode)
             (css-mode . css-ts-mode)
             (php-mode . php-ts-mode)
             (json-mode . json-ts-mode)
             (sql-mode . sql-ts-mode)
             (xml-mode . xml-ts-mode)
             (scala-mode . scala-ts-mode)
             (rust-mode . rust-ts-mode)
             (js-json-mode . json-ts-mode)))
    ;; Add our mapping to the global list.
    (add-to-list 'major-mode-remap-alist mapping))

  ;; `:config` runs this code *after* the package is configured.
  :config
  ;; Now that Emacs is set up, call the function we defined above
  ;; to actually check for and install the grammars.
  (setup-install-grammars)

  ;; Set the "font-lock level" (highlighting level) to 4.
  ;; This is the maximum level, giving us the most detailed
  ;; syntax highlighting that Tree-sitter can provide.
  (setq treesit-font-lock-level 4)

;;; ----------------------------------------------------------------------
;;; Combobulate (Advanced Tree-sitter Editing)
;;; ----------------------------------------------------------------------
  ;;
  ;; `combobulate` is a package that uses the Tree-sitter tree to
  ;; provide advanced code editing and navigation, like "expand selection
  ;; to the parent node" (structural selection).

  ;; This is nested inside the `treesit` :config block, which is a
  ;; common `use-package` pattern. It means "load `combobulate`
  ;; only *after* `treesit` has been configured."
  (use-package combobulate
    ;; `:ensure nil` and `:load-path` mean this package is *not*
    ;; being installed by Elpaca from a package archive. It's
    ;; being loaded from a local directory in your Emacs config.
    ;; You must have cloned it there manually.
    :ensure nil
    :load-path "~/.emacs.d/combobulate"

    ;; `:hook` automatically enables `combobulate-mode` whenever
    ;; we enter one of these specified Tree-sitter major modes.
    :hook ((python-ts-mode . combobulate-mode)
           (js-ts-mode . combobulate-mode)
           (tsx-ts-mode . combobulate-mode)
           (typescript-ts-mode . combobulate-mode))
    :config
    ;; Set the keyboard shortcut prefix for combobulate commands.
    ;; All commands will start with `C-c o` (e.g., `C-c o n` for next node).
    (setq combobulate-key-prefix "C-c o")))

;;; ----------------------------------------------------------------------
;;; Tree-sitter Code Folding
;;; ----------------------------------------------------------------------
;;
;; This section adds code folding (hiding/showing blocks like
;; functions or classes) using the Tree-sitter syntax tree.

(use-package treesit-fold
  ;; Install from GitHub. The syntax specifies the host and repo.
  :ensure (:host github :repo "emacs-tree-sitter/treesit-fold")
  :config
  ;; Bind a key to toggle the fold at the current point.
  ;; `s-<backspace>` is Super(Win/Cmd) + Backspace.
  (define-key treesit-fold-mode-map (kbd "s-<backspace>") 'treesit-fold-toggle))

;; This package *adds* visual indicators (like `+` or `...`) in the
;; margin (the "fringe") to show you where folded code is.
(use-package treesit-fold-indicators
  :ensure (:host github :repo "emacs-tree-sitter/treesit-fold")
  ;; `:after treesit-fold` ensures that `treesit-fold` is loaded *first*,
  ;; since this package depends on it.
  :after treesit-fold
  :config
  ;; Turn on the indicators globally.
  (global-treesit-fold-indicators-mode 1))


;;; ----------------------------------------------------------------------
;;; Finalize
;;; ----------------------------------------------------------------------

;; This is the standard "magic line" for an Emacs Lisp module.
;; It tells Emacs that the file `tree.el` has been loaded successfully,
;; which allows `(require 'tree)` in your `init.el` to work.
(provide 'tree)

;;; tree.el ends here
