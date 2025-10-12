;;; debug.el -- All necessary tools for debugging -*- lexical-binding: t; -*-
;;; Commentary:
;; This configuration is mainly based around dap-mode
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --- Other Tools & Core Settings ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DAP: Debug Adapter Protocol for in-editor debugging
(use-package dap-mode
  :ensure t
  :after lsp-mode
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode))

(provide 'debug)
;;; debug.el ends here.
