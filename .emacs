;;; package --- dot.emacs

(require 'package)

(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
;; defun
(defun req (package)
  "Require or install a package"
  (unless (require package nil t)
    (package-install package)))

;; requires ==================== Requires ==================== requires
;;(req 'evil)
(req 'ls-lisp)
(req 'magit)
(req 'neotree)
(req 'multiple-cursors)
(req 'dockerfile-mode)
(req 'docker-compose-mode)
(req 'projectile)
(req 'cyberpunk-theme)
(req 'yasnippet)
(req 'yafolding)
(req 'haml-mode)
(req 'dotenv-mode)
(req 'flymake-python-pyflakes)
(req 'helm-ag)
(req 'coffee-mode)

(set-default-font "Hack 11")
(set-face-bold-p 'bold nil)
(setq org-hide-emphasis-markers t)
(setq clean-buffer-list-delay-general 1)
(setq inhibit-splash-screen t)
(setq tags-revert-without-query 1)
(setq auto-save-default nil)
(setq tab-width 2)
(setq-default tab-width 2)
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

(menu-bar-mode -1)
(ido-mode)
(display-time)
(projectile-mode +1)
(windmove-default-keybindings)

;; hooks ==================== Hooks ==================== hooks
(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
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
(add-hook 'css-mode-hook '(lambda () (setq tab-width 2)))


;; Perl mode tab is not indenting
(defun my/perl-mode-hook ()
  (setq tab-width 4)
  (highlight-regexp "TODO:\?" 'hi-yellow)
  (highlight-regexp "FIXME:\?" 'hi-pink)
  (yas-minor-mode)
  (setq indent-tabs-mode nil)
  (local-set-key (kbd "<tab>")
                 (lambda () (interactive) (insert "    "))))
(add-hook 'perl-mode-hook 'my/perl-mode-hook)

(defun my/ruby-mode-hook ()
  (highlight-regexp "TODO:\?" 'hi-yellow)
  (highlight-regexp "FIXME:\?" 'hi-pink)
  (yas-minor-mode)
  (linum-mode 1)
  (yafolding-mode 1))
(add-hook 'ruby-mode-hook 'my/ruby-mode-hook)

(defun my/go-mode-hook ()
  (setq tab-width 2)
  (linum-mode 1)
  (add-hook 'before-save-hook 'gofmt-before-save))
(add-hook 'go-mode-hook 'my/go-mode-hook)


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

;; Docker-compose mode
(add-to-list 'auto-mode-alist '("docker-compose\\'" . docker-compose-mode))
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;; Perl mode for test files
(add-to-list 'auto-mode-alist '("\\.t\\'" . perl-mode))

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
(add-to-list 'exec-path "/usr/local/bin")

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

;; defuns ==================== Defuns ==================== defuns
(defun etags ()
  "Create etags in project directory"
  (interactive)
;;  (tags-reset-tags-tables)
  (let* ((dir (substring
               (shell-command-to-string "git rev-parse --show-toplevel")
               0 -1))
         (isdir (file-directory-p dir)))
    (if isdir
        (shell-command (format
                        "(cd %s ; find . -type f | etags -) &"
                        dir))
      (message "You are not in a git project"))))

(defun now ()
  "Current date"
  (format-time-string "%Y-%m-%d"))

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
(global-set-key (kbd "M-<tab>") 'yafolding-toggle-element)
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
(global-set-key (kbd "C-x v") 'view-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("6bc387a588201caf31151205e4e468f382ecc0b888bac98b2b525006f7cb3307" "addfaf4c6f76ef957189d86b1515e9cf9fcd603ab6da795b82b79830eed0b284" "7feeed063855b06836e0262f77f5c6d3f415159a98a9676d549bfeb6c49637c4" "77bd459212c0176bdf63c1904c4ba20fce015f730f0343776a1a14432de80990" "59e82a683db7129c0142b4b5a35dbbeaf8e01a4b81588f8c163bd255b76f4d21" "9527feeeec43970b1d725bdc04e97eb2b03b15be982ac50089ad223d3c6f2920" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(package-selected-packages
   (quote
    (ztree crystal-mode coffee env-mode company coffee-mode haml-mode evil-visual-mark-mode company-tabnine helm-projectile ag helm-ag dotenv-mode 0blayout flymake-python-pyflakes hemisu-theme excorporate plsense arc-dark-theme helm-make go-mode ac-etags gitlab-ci-mode-flycheck gitlab-ci-mode ecb use-package shell-pop yasnippet espresso-theme multifiles slime sexy-monochrome-theme ranger projectile powerline persistent-scratch neotree multiple-cursors magit klere-theme jedi-direx flymake-perlcritic flycheck-yamllint flycheck-rust flycheck-pycheckers fiplr evil dockerfile-mode docker-compose-mode dired-ranger cyberpunk-theme color-theme cheat-sh bliss-theme bash-completion))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
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
    (load-theme 'cyberpunk)
    (linum-mode)
    (set-foreground-color "#ccc")
    (set-face-foreground 'linum "#666")
    (set-cursor-color "#cccccc")
    ;;(set-face-foreground 'dired-directory "#9B1")
    (set-face-background 'fringe "#000")
    (set-face-background 'linum "#000")
    )
  (server-start))

(yas-global-mode)
(global-hl-line-mode 1)


(defun sync ()
  (interactive)
  (magit-stage-modified)
  (magit-commit)
  (magit-push-current-to-upstream))

(req 'company)
(add-to-list 'company-backends #'company-tabnine)
(setq company-idle-delay 0.6)
(setq company-show-numbers t)

(req 'undo-tree)
(global-undo-tree-mode 1)
(add-hook 'eshell-mode-hook '(lambda () (global-hl-line-mode -1)))
(add-hook 'ruby-mode-hook '(lambda () (linum-mode 1) (yafolding-mode 1)))
(add-hook 'haml-mode-hook '(lambda () (linum-mode 1) (company-mode 1)))
(add-hook 'sql-mode-hook '(lambda () (linum-mode 1)))
(add-hook 'js-mode-hook '(lambda () (linum-mode 1)))
(add-hook 'coffee-mode-hook
          (lambda ()
            (set (make-local-variable 'tab-width) 2)
            (set (make-local-variable 'indent-tabs-mode) t)))

;; For evil-mode
(global-set-key (kbd "§") (kbd "<escape>"))
