(setq inhibit-splash-screen 1)
(menu-bar-mode -1)

(ido-mode)
(custom-set-variables)
(custom-set-faces
 '(font-lock-function-name-face ((t (:foreground "green"))))
 '(minibuffer-prompt ((t (:foreground "blue")))))
(setq dired-listing-switches "-alh")
(defvar my-hostname (concat " " (system-name)))
(setq global-mode-string (append global-mode-string my-hostname))
(eshell)
(switch-to-buffer "*eshell*")
