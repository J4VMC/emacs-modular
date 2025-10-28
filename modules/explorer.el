;;; explorer.el -- File-specific configurations -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file configures file *exploration* and *management* inside Emacs. 📂
;;
;; The core of file management in Emacs is `Dired` (Directory Editor),
;; which is a built-in mode that turns a directory listing into an
;; editable buffer.
;;
;; We configure Dired with some quality-of-life tweaks and install
;; modern packages like `dirvish` and `oil` to enhance its
;; functionality, making it feel more like a modern file explorer.
;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; --- File Management (Dired) ---
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package dired
  ;; `:ensure nil` means "don't install this package."
  ;; We use this because `dired` is *built-in* to Emacs.
  ;; This block is just for *configuring* it.
  :ensure nil
  ;; Lazy-load Dired until the `dired` command is called.
  :commands (dired)
  :hook
  (;; When Dired starts, automatically enable `dired-hide-details-mode`.
   ;; This hides the boring `.` and `..` (current/parent directory)
   ;; entries by default, cleaning up the view. You can toggle
   ;; it by pressing `(` in Dired.
   (dired-mode . dired-hide-details-mode)
   ;; Also, highlight the current line in Dired.
   (dired-mode . hl-line-mode))
  :config
  ;; --- Quality-of-Life Settings ---

  ;; When copying a directory, *always* copy it recursively (copy
  ;; all its contents) without asking for confirmation.
  (setq dired-recursive-copies 'always)
  ;; Same, but for deleting directories recursively.
  (setq dired-recursive-deletes 'always)

  ;; **CRITICAL**: Use the system trash (Recycle Bin / Trash)
  ;; when deleting files. The default is to *permanently delete* (`rm`).
  ;; This is much safer!
  (setq delete-by-moving-to-trash t)

  ;; Enable "Do What I Mean" (dwim) for targets.
  ;; If you have two Dired buffers open in split windows,
  ;; copying or moving a file in one will automatically
  ;; set the *other* window as the destination. Very handy.
  (setq dired-dwim-target t))

;; Dirvish is a package that provides a "prettier" and more modern-feeling
;; user interface for Dired. It replaces the default Dired buffer
;; with its own, adding features like a centered, "follow-me"
;; file listing.
(use-package dirvish
  :ensure t
  :after dired ; Make sure Dired is loaded first
  :config
  ;; This is the magic line. It tells Dirvish to *completely take over*
  ;; Dired's job. When you call `dired`, you will get Dirvish instead.
  (dirvish-override-dired-mode t))

;; Oil.el is another modern alternative to Dired, inspired by Vim's
;; `oil.nvim`. It lets you edit a directory listing *as a text buffer*.
;; For example, you can use standard Emacs commands to rename files
;; (`C-c C-r`), delete them (`C-c C-d`), etc., by editing the text.
(use-package oil
  :ensure (:host github :repo "yibie/Oil.el"))

;;; ----------------------------------------------------------------------
;;; Finalize
;;; ----------------------------------------------------------------------

;; This is the standard "magic line" for an Emacs Lisp module.
;; It tells Emacs that the file `explorer.el` has been loaded successfully,
;; which allows `(require 'explorer)` in your `init.el` to work.
(provide 'explorer)

;;; explorer.el ends here
