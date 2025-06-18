;;; This configuration is mainly used for regular txt files.

;; Settings

;;; Code

(set-frame-font "Monoid Nerd Font Mono Retina 15" t)
(setq diary-file "~/Dropbox/My/diary")
(when (window-system)
  (set-frame-size (selected-frame) 100 40)
  (set-frame-position (selected-frame) 300 90))

(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(menu-bar-mode 0)
(scroll-bar-mode -1)
(tool-bar-mode -1)
;(fido-vertical-mode t)

;; In case no theme is used
(set-foreground-color "#222222")
(set-background-color "#f5f5f5")
(set-face-attribute 'region nil :background "#f5ebe0" :foreground "#d14d72")
(set-cursor-color "#bbbbbb")
(prefer-coding-system 'utf-8)
(setq exec-path (append exec-path '("/opt/homebrew/bin")))
(setenv "PATH" (concat (getenv "PATH") ":/opt/homebrew/bin"))
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;; Functions

(defun kbd/insert-emdash ()
  (interactive)
  (insert-char 8212))

(defun hooks/text-mode-hook ()
  (interactive)
  (setq cursor-type 'bar)
  (setq buffer-face-mode-face '(:family "Literata Book" :height 200))
  (buffer-face-mode)
  (visual-line-mode)
  (auto-save-visited-mode 1))

(defun hooks/ruby-mode-hook ()
  (interactive)
  (display-line-numbers-mode t))

(defun focus ()
  (interactive)
  (darkroom-tentative-mode))

;; Bindings

(add-hook 'text-mode-hook 'hooks/text-mode-hook)
(add-hook 'diary-mode-hook 'hooks/text-mode-hook)
(add-hook 'ruby-mode-hook 'hooks/ruby-mode-hook)
(global-set-key (kbd "C-c -") 'kbd/insert-emdash)

(global-set-key (kbd "C-у") 'move-end-of-line)
(global-set-key (kbd "C-ф") 'move-beginning-of-line)

;; Packages

(setq use-package-always-ensure t)
;; (require 'package)
;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;; (package-initialize)

;; (use-package mood-line
;;   ;; Enable mood-line
;;   :config
;;   (mood-line-mode)
;;   (setq mood-line-glyph-alist mood-line-glyphs-fira-code))

;; (use-package smartscan
;;   :init
;;   (smartscan-mode 1))

;; (use-package darkroom)

;; (use-package enh-ruby-mode)
;; (use-package typescript-mode)
;; (use-package markdown-mode)

;; (use-package magit
;;   :config
;;   (defalias 'b 'magit-blame-addition))

;; Startup windows

;; (calendar)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(term-keys markdown-mode typescript-mode mood-line enh-ruby-mode magit smartscan darkroom)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'downcase-region 'disabled nil)
