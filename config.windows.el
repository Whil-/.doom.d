;;; ~/.doom.d/config.windows.el -*- lexical-binding: t; -*-

(require 'server)
(unless (server-running-p) (server-start))

(setq browse-url-browser-function 'browse-url-default-windows-browser)
(setq w32-get-true-file-attributes nil)
(let ((find "c:/tools/msys64/usr/bin/find.exe")
      (grep "c:/tools/msys64/usr/bin/grep.exe"))
  (when (file-exists-p find)
    (setq find-program find))
  (when (file-exists-p grep)
    (setq grep-program grep)))

(setenv "SSH_ASKPASS" "git-gui--askpass")
(magit-auto-revert-mode -1)
