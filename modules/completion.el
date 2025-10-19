;;; completion.el -- Additional tools for completion utilities -*- lexical-binding: t; -*-
;;; Commentary:
;; This file collects all the settings for tools like Consult, Embark, Vertico, etc.
;;; Code:

;; Core Emacs completion behavior tweaks.
(use-package emacs
  :ensure nil
  :init
  (setq enable-recursive-minibuffers t)
  :custom
  (completion-cycle-threshold 3)
  (tab-always-indent 'complete)
  (text-mode-ispell-word-completion nil) ; For Emacs 30+
  (read-extended-command-predicate #'command-completion-default-include-p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --- The Completion Suite (Vertico, Consult, Embark, etc.) ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Vertico: A modern, vertical completion UI.
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :custom (vertico-cycle t))

(use-package vertico-directory
  :ensure nil
  :after vertico
  :bind (:map vertico-map
              ("M-DEL" . vertico-directory-delete-word)))

;; Orderless: Advanced completion style for flexible, out-of-order matching.
(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless))
  (setq completion-category-defaults nil))

;; Marginalia: Rich annotations for completion candidates in the minibuffer.
(use-package marginalia
  :ensure t
  :after vertico
  :init (marginalia-mode))

;; Consult: A powerful set of search and navigation commands.
(use-package consult
  :ensure t
  :after vertico
  :bind (("C-x b" . consult-buffer)
         ("M-y" . consult-yank-pop)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s o" . consult-outline))
  :config
  (setq consult-ripgrep-args
        "rg --null --line-buffered --max-columns=150 --max-columns-preview --max-file-size 1M --ignore-case --path-separator /\\ --smart-case --no-heading --line-number --hidden --glob !.git/ .")
  (setq consult-narrow-key "/"))

;; Embark: Act on completion candidates with contextual actions.
(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil (window-parameters (mode-line-format . none)))))

;; embark-consult: Integrates Embark with Consult for previews.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --- Completion-at-Point (Corfu, Cape, Tempel) ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Corfu: A clean and fast Completion-in-Region Function.
(use-package corfu
  :ensure t
  :init (global-corfu-mode)
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  :bind (:map corfu-map
              ("TAB" . corfu-next)
              ([tab] . corfu-next)
              ("S-TAB" . corfu-previous)
              ([backtab] . corfu-previous)))

;; Part of corfu
(use-package corfu-popupinfo
  :after corfu
  :ensure nil
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.25 . 0.1))
  (corfu-popupinfo-hide nil)
  :config
  (corfu-popupinfo-mode))

;; Make corfu popup come up in terminal overlay
(use-package corfu-terminal
  :if (not (display-graphic-p))
  :ensure t
  :config
  (corfu-terminal-mode))

;; Cape: Provides additional completion-at-point sources (backends).
(use-package cape
  :ensure t
  :init
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  (add-hook 'completion-at-point-functions #'cape-keyword)
  (add-hook 'completion-at-point-functions #'cape-dict)
  :config
  (defun super-capf-cape ()
    (cape-wrap-super #'cape-dabbrev #'cape-dict #'cape-keyword))
  (setq-local completion-at-point-functions (list #'super-capf-cape)))

(use-package tempel
  :ensure t
  ;; By default, tempel looks at the file "templates" in
  ;; user-emacs-directory, but you can customize that with the
  ;; tempel-path variable:
  ;; :custom
  ;; (tempel-path (concat user-emacs-directory "custom_template_file"))
  :bind (("M-*" . tempel-insert)
         ("M-+" . tempel-complete)
         :map tempel-map
         ("C-c RET" . tempel-done)
         ("C-<down>" . tempel-next)
         ("C-<up>" . tempel-previous)
         ("M-<down>" . tempel-next)
         ("M-<up>" . tempel-previous))
  :init
  ;; Make a function that adds the tempel expansion function to the
  ;; list of completion-at-point-functions (capf).
  (defun tempel-setup-capf ()
    (add-hook 'completion-at-point-functions #'tempel-expand -1 'local))
  ;; Put tempel-expand on the list whenever you start programming or
  ;; writing prose.
  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)
  :config
  (setq global-tempel-abbrev-mode 1))

(use-package tempel-collection
  :ensure t
  :after tempel)

(provide 'completion)
;;; completion.el ends here
