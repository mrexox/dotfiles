;;; Emacs configuration file.
;;;
;;; This configuration is mainly used for regular txt files.
;;;


;; Settings
(set-frame-font "Monoid Nerd Font Mono Retina 15")

(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(menu-bar-mode 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(set-foreground-color "#222222")
(set-background-color "#fafafa")
(set-cursor-color "#bbbbbb") 
(prefer-coding-system 'utf-8)

;; Functions
(defun kbd/insert-emdash ()
  (interactive)
  (insert-char 8212))

(defun hooks/text-mode-hook ()
  (interactive)
  (setq buffer-face-mode-face '(:family "Helvetica" :height 200))
  (setq cursor-type 'bar)
  (buffer-face-mode)
  (visual-line-mode))

(defun hooks/diary-mode-hook ()
  (interactive)
  (hooks/text-mode-hook)
  (auto-save-visited-mode 1))

(defun focus ()
  (interactive)
  (darkroom-tentative-mode))

;; Bindings
(add-hook 'text-mode-hook 'hooks/text-mode-hook)
(add-hook 'diary-mode-hook 'hooks/diary-mode-hook)
(global-set-key (kbd "C-c -") 'kbd/insert-emdash)


;; Packages

(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(use-package smart-mode-line
  :init
  (setq sml/no-confirm-load-theme t)
  (sml/setup))

(use-package darkroom)

;; Call commands
(calendar)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default))
 '(package-selected-packages '(darkroom smart-mode-line)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
