(setq inhibit-splash-screen 1)
(menu-bar-mode -1)

(ido-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-function-name-face ((t (:foreground "green"))))
 '(minibuffer-prompt ((t (:foreground "blue")))))
(setq dired-listing-switches "-alh")
(defvar my-hostname (concat " " (system-name)))
(setq global-mode-string (append global-mode-string my-hostname))
(eshell)
(switch-to-buffer "*eshell*")
