;;;  package --- Summary
;;; Commentary:
;;; -----------
;;; This is my (ian's) .emacs
;;; Feel free to take it
;;; --------------------
;;; Code:

;;; Averall Emacs settings:
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono Book 12" ))
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
(setq-default tab-width 4)

;;; ---------
;;; Packages:
;;; ---------
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
(require 'popup)

;; Auto-complete
(add-to-list 'load-path "~/.emacs.d/auto-complete")
(require 'auto-complete)

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

(add-to-list 'load-path "~/.emacs.d/log4e")
(require 'log4e)

(add-to-list 'load-path "~/.emacs.d/yaxception")
(require 'yaxception)

(add-to-list 'load-path "~/.emacs.d/powerline")
(require 'powerline)
(powerline-default-theme)



;;; ------
;;; Modes and hooks:
;;; ------
(show-paren-mode)
(menu-bar-mode -1)
;; Cursor cool wow
;;(setq-default cursor-type '(hbar . 2))
;;(blink-cursor-mode -1)

;; Set auto-fill-mode when text-node is active
(add-hook 'text-mode-hook '(lambda () (visual-line-mode 1)))
(add-hook 'text-mode-hook '(lambda () (font-lock-mode 0)))
(add-hook 'diary-mode-hook '(lambda () (auto-fill-mode 1)))

;; line counting
(add-hook 'java-mode-hook '(lambda () (interactive) (linum-mode 1)))
(add-hook 'perl-mode-hook '(lambda () (interactive) (linum-mode 1)))
(add-hook 'c-mode-hook '(lambda () (interactive) (linum-mode 1)))
(add-hook 'lisp-mode-hook '(lambda () (interactive) (linum-mode 1)))
(add-hook 'javascript-mode-hook '(lambda () (interactive) (linum-mode 1)))
(add-hook 'ruby-mode-hook '(lambda () (interactive) (linum-mode 1)))
(add-hook 'css-mode-hook '(lambda () (interactive) (linum-mode 1)))
(add-to-list 'auto-mode-alist '("\\.jsx\\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss?\\'" . css-mode))
(add-hook 'haskell-mode-hook #'hindent-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'web-mode-hook
		  (lambda ()
			(linum-mode t)
			(auto-complete-mode t)))
(add-hook 'haml-mode-hook
		  (lambda ()
			(setq indent-tabs-mode nil)
			(define-key haml-mode-map "\C-m" 'newline-and-indent)))
(add-hook 'lisp-mode-hook
		  (lambda ()
			(linum-mode t)
			(auto-complete-mode t)))

;; ;; F10 works as run for perl programs
;; (add-hook 'perl-mode-hook
;; 	  (lambda ()
;; 	    (define-key perl-mode-map [f10]
;; 	      `(lambda ()
;; 		 (interactive)
;; 		 (let ((args (read-string "Args: "))
;; 		       (scriptname (buffer-name)))
;; 		   (shell-command (concat "perl " scriptname " " args)))))))

;;; ------------
;;; Keybindings:
;;; ------------

(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "C-c C-e") '(lambda ()
								   (interactive)
								   (find-file "~/.emacs")))
(global-set-key [f8] 'neotree-toggle)
(global-set-key (kbd "C-x g") 'magit-status)

;; ;; Go mode
;; (add-to-list 'load-path "~/.emacs.d/go-mode.el/")
;; (require 'go-mode-autoloads)
;; (add-hook 'go-mode-hook
;;           (lambda ()
;;             (add-hook 'before-save-hook 'gofmt-before-save)
;;             (setq tab-width 2)
;; 	    (linum-mode t)
;;             (setq indent-tabs-mode 1)))





;; Popup lib


;;			(setq web-mode-markup-indent-offset 2)
;;			(setq web-mode-code-indent-offset 2)))



(defun turn-off-mouse (&optional frame)
  "FRAME - choose frame to disable mouse."
  (interactive)
  (let ((inhibit-message t) (default-directory "~"))
    (shell-command "synclient TouchpadOff=1")))

(defun turn-on-mouse (&optional frame)
  "FRAME - choose frame to disable mouse."
  (interactive)
  (let ((inhibit-message t) (default-directory "~"))
    (shell-command "synclient TouchpadOff=0")))

;; (load-theme 'dracula t)

;; (add-to-list 'load-path "~/.emacs.d/php-mode")
;; (require 'php-mode)



;; PACKAGES











(add-to-list 'load-path "~/.emacs.d/disable-mouse")
(require 'disable-mouse)
(global-disable-mouse-mode)

(add-to-list 'custom-theme-load-path "/home/ian/.emacs.d/themes")
(load-theme 'junio t)
(setq calendar-week-start-day 1
          calendar-day-name-array ["Вс" "Пн" "Вт" "Ср" "Чт" "Пт" "Сб"]
          calendar-month-name-array ["Январь" "Февраль" "Март" "Апрель" "Май"
                                     "Июнь" "Июль" "Август" "Сентябрь"
                                     "Октябрь" "Ноябрь" "Декабрь"])
(setq view-diary-entries-initially t)
;;(calendar)
;;(switch-to-buffer (get-buffer "*scratch*"))
;;(other-window)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
	("1b1e54d9e0b607010937d697556cd5ea66ec9c01e555bb7acea776471da59055" default)))
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(package-selected-packages
   (quote
	(jsx-mode ## js2-mode powerline projectile dark-mint-theme haskell-mode smartparens slime flycheck magit dash))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Magin aliases
(defalias 'blame 'magit-blame)
(defalias 'b 'blame)
(defalias 'status 'magit-status)
(defalias 's 'status)

(load (expand-file-name "~/.quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")
(server-start)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'python-mode-hook
			 (lambda ()
			   (linum-mode)))

(global-flycheck-mode)
(flycheck-add-mode 'javascript-eslint 'web-mode)


(provide '.emacs)
;;; .emacs ends here
