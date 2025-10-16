;;; tree.el -- Configuration related to Tree-sitter -*- lexical-binding: t; -*-
;;; Commentary:
;; Collection of settings related to Tree-sitter
;;; Code:

(use-package treesit
  :ensure nil
  :mode (("\\.tsx\\'" . tsx-ts-mode))
  :preface
  (defun setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
	     ;; Note the version numbers. These are the versions that
	     ;; are known to work with Combobulate *and* Emacs.
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
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
	(treesit-install-language-grammar (car grammar)))))

  ;; Optional. Combobulate works in both xxxx-ts-modes and
  ;; non-ts-modes.

  ;; You can remap major modes with `major-mode-remap-alist'. Note
  ;; that this does *not* extend to hooks! Make sure you migrate them
  ;; also
  (dolist (mapping
	   '((python-mode . python-ts-mode)
	     (css-mode . css-ts-mode)
	     (typescript-mode . typescript-ts-mode)
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
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (setup-install-grammars)

  (setq treesit-font-lock-level 4)  ;; Maximum highlighting level
  
  ;; Do not forget to customize Combobulate to your liking:
  ;;
  ;;  M-x customize-group RET combobulate RET
  ;;
  (use-package combobulate
    :ensure nil
    :load-path "~/.emacs.d/combobulate"
    :hook ((python-ts-mode . combobulate-mode)
	   (js-ts-mode . combobulate-mode)
	   (tsx-ts-mode . combobulate-mode)
	   (typescript-ts-mode . combobulate-mode))
    :config
    (setq combobulate-key-prefix "C-c o")))

(use-package treesit-fold
  :ensure (:host github :repo "emacs-tree-sitter/treesit-fold")
  :config
  (define-key treesit-fold-mode-map (kbd "s-<backspace>") 'treesit-fold-toggle))

(use-package treesit-fold-indicators
  :ensure (:host github :repo "emacs-tree-sitter/treesit-fold")
  :after treesit-fold
  :config
  (global-treesit-fold-indicators-mode 1))

(provide 'tree)
;;; tree.el ends here

