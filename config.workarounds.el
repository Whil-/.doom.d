;;; ~/.doom.d/config-workarounds.el -*- lexical-binding: t; -*-

;; See https://github.com/hlissner/doom-emacs/issues/3185
(defadvice! no-errors/+org-inline-image-data-fn (_protocol link _description)
  :override #'+org-inline-image-data-fn
  "Interpret LINK as base64-encoded image data. Ignore all errors."
  (ignore-errors
    (base64-decode-string link)))

;; See https://github.com/abo-abo/swiper/issues/235
;; and https://github.com/hlissner/doom-emacs/issues/3215
;; This seems to be changed somewhere in Doom to "... --path-separator // ..."
(if (eq system-type 'windows-nt)
    (setq counsel-rg-base-command
          "rg -M 240 --with-filename --no-heading --line-number --color never %s --path-separator / ."))
