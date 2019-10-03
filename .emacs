;;; package --- My .emacs

;;; Code:

(package-initialize)
;; Requries
(require 'package)
(require 'evil)
(require 'ls-lisp)
(require 'magit)
(require 'neotree)
(require 'multiple-cursors)
(require 'dockerfile-mode)
(require 'docker-compose-mode)
(require 'projectile)

(set-default-font "DejaVu Sans Mono 11")
(setq clean-buffer-list-delay-general 1)
(setq inhibit-splash-screen t)
(setq tags-revert-without-query 1)
(setq auto-save-default nil)
(setq tab-width 4)
(setq-default tab-width 4)
(setq column-number-mode t)
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
            '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))
(setq backup-directory-alist '(("." . "~/emacs-backups")))

;; NO TABS IN INDENTATION
(setq-default indent-tabs-mode nil)

;; Ls in dired mode
(setq ls-lisp-dirs-first t)
(setq ls-lisp-use-insert-directory-program nil)

(fset 'yes-or-no-p 'y-or-n-p)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Commands
(toggle-frame-fullscreen)
(menu-bar-mode -1)
(ido-mode)
(display-time)

;; Hooks
(defun my-go-mode-hook ()
  (setq tab-width 4 indent-tabs-mode nil)
  (linum-mode 1)
  (add-hook 'before-save-hook 'gofmt-before-save))
(add-hook 'go-mode-hook 'my-go-mode-hook)

(add-hook 'text-mode-hook '(lambda () (visual-line-mode 1)))
(add-hook 'text-mode-hook '(lambda () (font-lock-mode 0)))
(add-hook 'diary-mode-hook '(lambda () (auto-fill-mode 1)))
(add-hook 'makefile-mode-hook '(lambda () (indent-tabs-mode t)))
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'yaml-mode-hook
          (lambda ()
            (flycheck-mode)
            (define-key yaml-mode-map "\C-m" 'newline-and-indent)))
(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup))

(use-package gitlab-ci-mode-flycheck
  :after flycheck gitlab-ci-mode
  :init
  (gitlab-ci-mode-flycheck-enable))


;; Magit
(defalias 'blame 'magit-blame)
(defalias 'b 'blame)
(defalias 'status 'magit-status)
(defalias 's 'status)
(add-hook 'magit-status-mode-hook
          (lambda () (visual-line-mode 1)))

(setq pretty '(sh c js python perl lisp))
(add-hook 'makefile-mode-hook (lambda () (linum-mode 1)))
;; Linum-mode for all files listed in `line-them'
(dolist (elt pretty)
  (add-hook (intern (concat (symbol-name elt) "-mode" "-hook"))
			(lambda()
			  (linum-mode 1)
              (setq-default indent-tabs-mode nil))))

;; Perl mode tab is not indenting
(defun perl-mode-start ()
  (setq tab-width 4)
  (highlight-regexp "TODO:\?" 'hi-yellow)
  (highlight-regexp "FIXME:\?" 'hi-pink)
  (yas-minor-mode)
  (setq indent-tabs-mode nil)
  (local-set-key (kbd "<tab>")
                 (lambda () (interactive) (insert "    "))))
(add-hook 'perl-mode-hook 'perl-mode-start)



(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; Docker-compose mode
(add-to-list 'auto-mode-alist '("docker-compose\\'" . docker-compose-mode))
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
(add-to-list 'auto-mode-alist '("\\.t\\'" . perl-mode))


;; Last
(set-face-bold-p 'bold nil)

(mapc
 (lambda (face)
   (set-face-attribute face nil :weight 'normal :underline nil))
 (face-list))

;; Slime
(setq inferior-lisp-program "sbcl")
(setq slime-contribs '(slime-fancy))

;; ORG
(setq org-log-done 'time)

;; HELM_AG
(defalias 'agr 'helm-do-ag-project-root)
(defalias 'ag 'helm-do-ag)

;; ESHELL ALIASES
(defun eshell/ff (file)
  (find-file-other-window file))

(defun eshell/f (file)
  (find-file file))

(defalias 'eshell/l 'eshell/ls)
(defalias 'eshell/ll 'eshell/ls)

(defun install ()
  (interactive)
  (if (call-process-shell-command
       "cd $(git rev-parse --show-toplevel)/contrib/rdpkg && sudo make install clean"
       nil "*Shell Command Output*" t)
      (message "Installed!")
    (message "Error")))

(defun eshell-new()
  "Open a new instance of eshell."
  (interactive)
  (eshell 'N))

(defun eshell/clear ()
  "Clear the eshell buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (delete-other-windows)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))


(defun etags ()
  "Create etags in project directory"
  (interactive)
  (tags-reset-tags-tables)
  (let* ((dir (substring
               (shell-command-to-string "git rev-parse --show-toplevel")
               0 -1))
         (isdir (file-directory-p dir)))
    (if isdir
        (shell-command (format
                        "(cd %s ; find . -regex '.*.[hplmycsxgo]+' -type f | etags -)"
                        dir))
      (message "You are not in a git project"))))

(projectile-mode +1)

;; Global Set Keys
(global-set-key (kbd "C-x f") 'projectile-find-file)
(global-set-key (kbd "C-x p") 'magit-pull-from-upstream)
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "C-S-<iso-lefttab>") 'other-frame)
(global-set-key (kbd "C-c C-e") '(lambda () (interactive)
                                   (find-file "~/.emacs")))
(global-set-key (kbd "M-e") 'yas-expand)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key [f8] 'neotree-toggle)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key "\M-g" 'goto-line)
(global-set-key [f1] 'manual-entry)
(global-set-key [f2] 'info)
(global-set-key [f3] 'repeat-complex-command)
(global-set-key [f4] 'advertised-undo)
(global-set-key [f5] 'helm-make-projectile)
(global-set-key [f6] 'kill-other-buffers)
(global-set-key [f7] 'find-file)
(global-set-key [f9] 'install)
(global-set-key [f12] 'next-buffer)
(global-set-key [f10] 'kill-buffer)

(when window-system
  (tool-bar-mode 0)
  (scroll-bar-mode 0)
  (custom-set-variables
   '(initial-frame-alist (quote ((fullscreen . maximized)))))
  ;;  (require 'color-theme-sanityinc-tomorrow)
  (when (display-graphic-p)
    (global-hl-line-mode -1)
    ;;(load-theme 'klere)
    ;;(color-theme-sanityinc-tomorrow-night)
    ;;(load-theme 'tango)
    ;;(load-theme 'bliss)
    ;;(load-theme 'melancholy)
    ;;(set-cursor-color "#666666"))
    ;;(set-foreground-color "#CCCCCC")
    ;;(set-face-foreground 'linum "#cccccc")
    ;;(set-border-color "#cccccc")
    )
  (server-start))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("7feeed063855b06836e0262f77f5c6d3f415159a98a9676d549bfeb6c49637c4" "77bd459212c0176bdf63c1904c4ba20fce015f730f0343776a1a14432de80990" "59e82a683db7129c0142b4b5a35dbbeaf8e01a4b81588f8c163bd255b76f4d21" "9527feeeec43970b1d725bdc04e97eb2b03b15be982ac50089ad223d3c6f2920" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(package-selected-packages
   (quote
    (flymake-python-pyflakes hemisu-theme excorporate plsense arc-dark-theme helm-make go-mode ac-etags gitlab-ci-mode-flycheck gitlab-ci-mode ecb use-package shell-pop yasnippet espresso-theme multifiles slime sexy-monochrome-theme ranger projectile powerline persistent-scratch neotree multiple-cursors magit klere-theme jedi-direx helm-ag flymake-perlcritic flycheck-yamllint flycheck-rust flycheck-pycheckers fiplr evil dockerfile-mode docker-compose-mode dired-ranger cyberpunk-theme color-theme cheat-sh bliss-theme bash-completion))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(yas-global-mode)
(global-hl-line-mode 1)

(load-theme 'cyberpunk)
;;(load-theme 'hemisu-dark)
(set-foreground-color "#ccc")
(linum-mode)
(set-face-foreground 'linum "#666")
(set-cursor-color "#cccccc")
;;(set-face-foreground 'dired-directory "#9B1")
(set-face-background 'fringe "#000")
(set-face-background 'linum "#000")
(setenv "PERL5LIB"
        (concat "/home/ian/src/r/core/src" ":"
                "/home/ian/perl5/lib/perl5"))
(require 'flymake-python-pyflakes)
(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)


(defun sync ()
  (interactive)
  (magit-stage-modified)
  (magit-commit)
  (magit-push-current-to-upstream))

(global-undo-tree-mode 1)
(add-hook 'eshell-mode-hook '(lambda () (global-hl-line-mode -1)))
