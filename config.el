;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Gustav Wikström"
      user-mail-address "gustav@whil.se")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font "iosevka-12"
      doom-variable-pitch-font "DejaVu sans-13")

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one-light)

;;;; Initialize the defaults as soon as possible
(setq defaults-pim-dir (expand-file-name "f:/OneDrive - Wikström/")
      defaults-notes-dir (expand-file-name "f:/OneDrive - Wikström/Notes")
      defaults-tasks-dir (expand-file-name "f:/OneDrive - Wikström/Notes")
      defaults-library-dir (expand-file-name "f:/OneDrive - Wikström/Library"))
(setq gw/index-file (expand-file-name defaults-pim-dir)
      gw/misc-file (expand-file-name "misc.org" defaults-notes-dir))
;;;; OS-specific settings maybe
(let* ((windows-file (expand-file-name "config.windows.el"))
       (linux-file (expand-file-name "config.linux.el")))
  (cond
   ((eq system-type 'windows-nt)
    (if (file-exists-p windows-file)
        (load! windows-file)))
   ((eq system-type 'gnu/linux)
    (if (file-exists-p linux-file)
        (load! linux-file)))))
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory defaults-notes-dir)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.
;;; Emacs configurations
(menu-bar-mode 1)
(scroll-bar-mode 1)
(setq calendar-week-start-day 1)
(setq confirm-kill-emacs nil)
(setq custom-file (concat doom-private-dir "custom.el"))

(defun gw/done ()
  "Exit server buffers and hide the main Emacs window."
  (interactive)
  (server-edit)
  (make-frame-invisible nil t))
(map! "C-x C-c" #'gw/done)
(map! "C-M-c" #'save-buffers-kill-emacs)
;;(set-language-environment 'utf-8)
;;(set-default-coding-systems 'utf-8-dos)
;;(set-keyboard-coding-system 'utf-8-dos)
;;(set-terminal-coding-system 'utf-8-dos)
;;(setq locale-coding-system 'utf-8-dos)
(prefer-coding-system 'utf-8-dos)
;; Mostly for Org mode for it to format timestrings in english.
(setq system-time-locale "C")
;;; Built in major modes setup
(setq calc-multiplication-has-precedence nil)

;;;; Dired
(defun open-in-external-app ()
  "Open the current file or dired marked files in external app.
  Works in Microsoft Windows, Mac OS X, Linux."
  (interactive)
  (let (doIt
        (myFileList
         (cond
          ((string-equal major-mode "dired-mode") (dired-get-marked-files))
          (t (list (buffer-file-name))))))
    (setq doIt (if (<= (length myFileList) 5)
                   t
                 (y-or-n-p "Open more than 5 files?")))
    (when doIt
      (cond
       ((string-equal system-type "windows-nt")
        (mapc (lambda (fPath) (w32-shell-execute
                               "open" (replace-regexp-in-string
                                       "/" "\\"  fPath t t)))
              myFileList))
       ((string-equal system-type "darwin")
        (mapc (lambda (fPath) (let ((process-connection-type nil))
                                (start-process "" nil "open" fPath)))
              myFileList))
       ((string-equal system-type "gnu/linux")
        (mapc (lambda (fPath) (let ((process-connection-type nil))
                                (start-process "" nil "xdg-open" fPath)))
              myFileList))))))
(map! :mode dired-mode "C-c o o" #'open-in-external-app)
;;; Package configurations
(use-package! deadgrep
  :if (executable-find "rg")
  :init
  (map! "M-s" #'deadgrep))
(after! dired
  (setq dired-listing-switches "-lagGh"))
(use-package! filetags
  :if (string-equal system-type "windows-nt")
  :bind (:map dired-mode-map
         ("M-t" . filetags-dired-update-tags)))
(use-package! ledger-mode
  :init
  (setq ledger-reconcile-default-commodity "SEK"))
(use-package! ol-library
  :after org
  :config
  (setq ol-library-article-dir (expand-file-name "Journal Article" defaults-library-dir)
        ol-library-book-dir (expand-file-name "Book" defaults-library-dir)
        ol-library-video-dir (expand-file-name "Video" defaults-library-dir)
        ol-library-webpage-dir (expand-file-name "Web Page" defaults-library-dir)))
(use-package! org
  :mode ("\\.org\\'" . org-mode)
  :init
  (setq org-todo-keyword (quote ((sequence "TODO(t!)"
                                           "|"
                                           "DONE(d!)" "CANCELLED(c!)")))
        org-log-done 'note
        org-enforce-todo-dependencies t
        org-todo-keyword-faces (quote (("DONE"      :foreground "forest green" :weight bold)
                                       ("CANCELLED" :foreground "orange"      :weight bold)))
        org-hierarchical-todo-statistics nil
        org-src-window-setup 'current-window
        org-archive-location "%s_archive::datetree/"
        ;; org-agenda
        org-agenda-files (list defaults-tasks-dir)
        org-default-notes-file defaults-tasks-dir
        org-agenda-file-regexp ".*tasks\\|meetings.*\\.org\\'"
        org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
                                   (timeline . "  % s")
                                   (todo . " %i %-12:c")
                                   (tags . " %i %-12:c%b")
                                   (search . " %i %-12:c%b"))

        ;; Det enda jag ändrar är att sortera To-do's enligt deras Todo-ordning.
        ;; I övrigt är allt default.
        org-agenda-sorting-strategy '((agenda habit-down time-up priority-down category-keep)
                                      (todo todo-state-up priority-down category-keep)
                                      (tags priority-down category-keep)
                                      (search category-keep))
        org-agenda-span 'week
        org-timeline-show-empty-dates nil
        org-agenda-todo-keyword-format "%-6s"
        org-agenda-compact-blocks t
        org-agenda-show-all-dates t
        org-agenda-text-search-extra-files '(agenda-archives)
        org-agenda-sticky t
        ;; org-id
        org-id-track-globally t
        org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id
        org-id-locations-file (expand-file-name ".org-id-locations" defaults-pim-dir)
        org-id-locations-file-relative t
        org-id-method 'ts
        org-attach-id-to-path-function-list '(org-attach-id-ts-folder-format
                                              org-attach-id-uuid-folder-format)
        ;; org-attach
        org-attach-id-dir ".attachments"
        org-attach-use-inheritance t
        org-attach-dir-relative t
        org-return-follows-link t
        ;; logging
        org-log-into-drawer t
        ;; source code
        org-babel-load-languages '((calc .t)
                                   (emacs-lisp . t)
                                   (python . t)
                                   (sqlite . t)
                                   (plantuml . t)
                                   (dot . t)
                                   (R . t))
        org-confirm-babel-evaluate nil
        ;; Misc
        org-adapt-indentation nil
        org-hide-leading-stars t
        org-agenda-tags-todo-honor-ignore-options t
        org-startup-with-inline-images t
        org-image-actual-width nil
        ;; Footnotes
        org-footnote-section nil

        org-catch-invisible-edits 'show
        org-structure-template-alist '(("a" . "export ascii")
                                       ("c" . "center")
                                       ("C" . "comment")
                                       ("e" . "example")
                                       ("E" . "export")
                                       ("h" . "export html")
                                       ("l" . "export latex")
                                       ("q" . "quote")
                                       ("s" . "src")
                                       ("v" . "verse")
                                       ("el" . "src emacs-lisp")
                                       ("d" . "definition")
                                       ("t" . "theorem"))))
(after! org
  (setq org-startup-indented nil)
  (add-to-list 'org-export-backends 'md))
(use-package! org-brain
  :bind ("C-c w" . gw/org-brain-visualize-open)
  :init
  (defun gw/org-brain-visualize-open ()
    (interactive)
    (if (get-buffer "*org-brain*")
        (switch-to-buffer "*org-brain*")
      (org-brain-visualize-random)))
  (setq org-brain-path defaults-notes-dir)
  :config
  (setq org-brain-visualize-default-choices 'all)
  (setq org-brain-title-max-length 35)
  ;;(setq org-brain-show-resources 'nil)
  (setq org-brain-include-file-entries nil
        org-brain-file-entries-use-title nil)
  (setq org-brain-file-from-input-function
        (lambda (file) (if (cdr file) (car file) "misc")))
  (setq org-brain-headline-entry-name-format-string "%2$s"))
(use-package! magit
  :init
  (map! "C-c m" #'magit-status))
(use-package! uniquify
  :init
  (setq uniquify-buffer-name-style 'forward))
(use-package! visual-basic
  :mode ("\\.\\(frm\\|bas\\|cls\\|vb\\)$" . visual-basic-mode)
  :init
  (setq visual-basic-mode-indent 4))
;;; Prepair some background stuff
(org-agenda-list)
(org-todo-list)
(org-brain-visualize-random)

;;; Custom functions
(defun gw/split ()
  "Switches between vertical or horizontal split if there are two active
windows in the buffer"
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun gw/toggle-utf8-latin1 ()
  (interactive)
  (if (or (equal buffer-file-coding-system 'utf-8-dos)
          (equal buffer-file-coding-system 'mule-utf-8-dos)
          (equal buffer-file-coding-system 'utf-8-unix))
      (progn
        (set-buffer-file-coding-system 'latin-1) (save-buffer)
        (message "buffer converted to latin-1"))
    (set-buffer-file-coding-system 'utf-8) (save-buffer)
    (message "buffer converted to utf-8")))

(load! "config.workarounds.el")

