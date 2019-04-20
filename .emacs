;;; ================================================================================
;;; Averall Emacs settings:
;;; Config config config
;;; ================================================================================
(require 'ido)
(ido-mode 1)
(visual-line-mode 1)
(set-cursor-color "#666666")
(desktop-save-mode 1)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono Book 12" ))
(menu-bar-mode -1)
;; No scroll-bars and tool-bars
(when window-system
  (tool-bar-mode 0)
  (scroll-bar-mode 0)
  (custom-set-variables
   '(initial-frame-alist (quote ((fullscreen . maximized))))))
;; Startup
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)				; no startup message
(setq initial-scratch-message
      ";; Hello, Ian on Arch. Happy hacking!\n\n")
(setq frame-title-format "%b")					; top line
(setq-default make-backup-files nil)			; no file~ files
(display-time)									; time on status panel
(fset 'yes-or-no-p 'y-or-n-p)
(setq require-final-newline t)
(setq default-tab-width 4)



;;; ---------------------------
;;; Packages:
;;; ---------------------------
(require 'ls-lisp)
(setq ls-lisp-dirs-first t)
(setq ls-lisp-use-insert-directory-program nil)

(require 'package)
;; For package repositories
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)
(add-to-list 'load-path "~/.emacs.d/popup-el")

;; Web mode
(add-to-list 'load-path "~/.emacs.d/web-mode/")
(require 'web-mode)

(add-to-list 'load-path "~/.emacs.d/haml-mode")
(require 'haml-mode)

(add-to-list 'load-path "~/.emacs.d/slime")
(require 'slime-autoloads)
(setq inferior-lisp-program "/opt/sbcl/bin/sbcl")
(setq slime-contribs '(slime-fancy))
(setq-default lisp-body-indent 2)
(setq slime-net-coding-system 'utf-8-unix)
(setq-default lisp-indent-function 'common-lisp-indent-function)
(slime-setup '(slime-repl
               slime-fuzzy
               slime-fancy-inspector
               slime-indentation))
(add-to-list 'load-path "~/.emacs.d/neotree")
(require 'neotree)

(require 'multiple-cursors)

(add-to-list 'load-path "~/.emacs.d/disable-mouse")
(require 'disable-mouse)
(global-disable-mouse-mode)



;;; ---------------------------
;;; Modes and hooks:
;;; ---------------------------
(defun my-mode-hook ()
  (linum-mode 1)
  (setq indent-tabs-mode nil)
  (setq tab-width 2))
;; Set auto-fill-mode when text-node is active
(add-hook 'text-mode-hook '(lambda () (visual-line-mode 1)))
(add-hook 'text-mode-hook '(lambda () (font-lock-mode 0)))
(add-hook 'diary-mode-hook '(lambda () (auto-fill-mode 1)))

;; Useful hooks
(add-hook 'java-mode-hook   'my-mode-hook)
(add-hook 'perl-mode-hook   'my-mode-hook)
(add-hook 'python-mode-hook 'my-mode-hook)
(add-hook 'c-mode-hook      'my-mode-hook)
(add-hook 'lisp-mode-hook   'my-mode-hook)
(add-hook 'ruby-mode-hook   'my-mode-hook)
(add-hook 'css-mode-hook    'my-mode-hook)
(add-hook 'js-mode-hook     'my-mode-hook)

(setq js-indent-level 2)

(projectile-mode)
;;; ---------------------------
;;; Keybindings:
;;; ---------------------------

(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "C-c C-e") '(lambda ()
				  (interactive)
				  (find-file "~/.emacs")))
(global-set-key (kbd "C-x f") 'projectile-find-file)
(global-set-key [f8] 'neotree-toggle)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "<f5>") 'ian/deploy)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "M-g") 'goto-line)
(define-key mc/keymap (kbd "C-. M-C-f") 'mc/mark-next-sexps)
(define-key mc/keymap (kbd "C-. M-C-b") 'mc/mark-previous-sexps)
(define-key mc/keymap (kbd "C-. <") 'mc/mark-all-above)
(define-key mc/keymap (kbd "C-. >") 'mc/mark-all-below)

(define-key mc/keymap (kbd "C-. C-d") 'mc/remove-current-cursor)
(define-key mc/keymap (kbd "C-. C-k") 'mc/remove-cursors-at-eol)
(define-key mc/keymap (kbd "C-. d")   'mc/remove-duplicated-cursors)

(define-key mc/keymap (kbd "C-. .")   'mc/move-to-column)
(define-key mc/keymap (kbd "C-. =")   'mc/compare-chars)

;; ;; Go mode
(require 'go-mode)
(add-hook 'go-mode-hook
	  (lambda ()
	    (add-hook 'before-save-hook 'gofmt-before-save)
	    (my-mode-hook)))
;;; ---------------------------
;;; Other settings
;;; ---------------------------
(setq calendar-week-start-day 1
      calendar-day-name-array ["Вс" "Пн" "Вт" "Ср" "Чт" "Пт" "Сб"]
      calendar-month-name-array ["Январь" "Февраль" "Март" "Апрель" "Май"
					  "Июнь" "Июль" "Август" "Сентябрь"
					  "Октябрь" "Ноябрь" "Декабрь"])
(setq view-diary-entries-initially t)

(global-auto-revert-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("addfaf4c6f76ef957189d86b1515e9cf9fcd603ab6da795b82b79830eed0b284" "57fe2bf84d81baecc6d89ed97bdb19936a3052fc2551ca178667fc45feef2381" "1b1e54d9e0b607010937d697556cd5ea66ec9c01e555bb7acea776471da59055" default)))
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(package-selected-packages
   (quote
    (cyberpunk-theme yaml-mode markdown-mode go-mode sexy-monochrome-theme eyebrowse mc-extras multiple-cursors jsx-mode ## js2-mode slime magit))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:background "#141918")))))

;; Magit aliases
(defalias 'blame 'magit-blame)
(defalias 'b 'blame)
(defalias 'status 'magit-status)
(defalias 's 'status)

(server-start)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; TODO: make it deployc
(defun ian/deploy ()
  (interactive)
  (shell-command "~/Bin/deploy-emacs"))

(load-theme 'cyberpunk)
(when window-system
  (set-face-foreground 'fringe "#444")
  (set-face-background 'fringe "#000")
  (linum-mode t)
  (set-face-foreground 'linum "#444")
  (set-face-background 'linum "#000"))
(provide '.emacs)
