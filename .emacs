;;; DOTEMACS

;; Use C-c C-e for fast editing .emacs
(global-set-key (kbd "C-c C-e") '(lambda () (interactive)
                                   (find-file "~/.emacs")))

(require 'package)

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; Temporarily removed non-stable repository

(add-to-list 'package-archives
             '("melpa" ."https://melpa.org/packages/") t)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

;;;
;;; .emacs helpers
;;;

(defun req (package)
  "Require or install a package"
  (unless (require package nil t)
    (package-install package)))

(defmacro require-all (&rest packages)
  "Require all given packages"
  `(dolist (pkg ',packages)
     (req pkg)))

;;;
;;; Requires
;;;

(require-all

 ;;
 ;; Functionality
 ;;

 ls-lisp
 magit
 multiple-cursors
 projectile
 helm-projectile
 helm-xref
 yasnippet
 rbenv
 undo-tree

 ;;
 ;; Look and feel
 ;;

 smart-mode-line
 cyberpunk-theme

 ;;
 ;; File modes
 ;;

 editorconfig
 dockerfile-mode
 dotenv-mode
 yaml-mode
 web-mode
 helm-ag)

;;;
;;; Settings
;;;

(setq sml/no-confirm-load-theme t)
(setq sml/theme 'dark)
(sml/setup)

(setq custom-file "~/.emacs-custom.el")
(load custom-file)
(set-frame-font "Monoid 13")
(setq clean-buffer-list-delay-general 1) ; make buffer eligible for killing in 1 day
(setq inhibit-splash-screen t)
(setq tags-revert-without-query 1)
(setq auto-save-default nil)
(setq-default tab-width 2)
(setq column-number-mode t)
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
            '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))
(setq backup-directory-alist '(("." . "~/.emacs-backups")))

(setq-default indent-tabs-mode nil)

;; Ls in dired mode
(setq ls-lisp-dirs-first t)
(setq ls-lisp-use-insert-directory-program nil)
(set-face-bold-p 'bold nil)
(fset 'yes-or-no-p 'y-or-n-p)

;; Never keep current tags table. Always open without asking.
(setq tags-add-tables nil)
(setq large-file-warning-threshold nil)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;;;
;;; File-modes
;;;

(menu-bar-mode -1)
(global-prettify-symbols-mode +1)
(ido-mode 1)
;; (helm-mode) ; Not so useful
(display-time)
(projectile-mode +1)
(windmove-default-keybindings)
;;(global-rbenv-mode)
(global-undo-tree-mode 1)
(yas-global-mode)
(global-hl-line-mode 1)
(editorconfig-mode 1)

;;;
;;; Hooks
;;;

(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
(add-hook 'text-mode-hook '(lambda () (visual-line-mode 1)))
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
(add-hook 'dired-mode-hook
          (lambda () (dired-hide-details-mode)))
(add-hook 'magit-status-mode-hook
          (lambda () (visual-line-mode 1)))
(add-hook 'makefile-mode-hook (lambda () (linum-mode 1)))
(add-hook 'eshell-mode-hook '(lambda () (global-hl-line-mode -1)))
(add-hook 'haml-mode-hook '(lambda () (linum-mode 1) (company-mode 1)))
(add-hook 'sql-mode-hook '(lambda () (linum-mode 1)))
(add-hook 'js-mode-hook '(lambda () (linum-mode 1)))
(add-hook 'coffee-mode-hook
          (lambda ()
            (set (make-local-variable 'tab-width) 2)
            (set (make-local-variable 'indent-tabs-mode) t)))

(add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))

(defun my/perl-mode-hook ()
  (setq tab-width 4)
  (highlight-regexp "#\s\*TODO:\?" 'hi-yellow)
  (highlight-regexp "#\s\*FIXME:\?" 'hi-pink)
  (highlight-regexp "#\s\*NOTE:\?" 'hi-yellow)
  (yas-minor-mode)
  (setq indent-tabs-mode nil)
  (local-set-key (kbd "<tab>")
                 (lambda () (interactive) (insert "    "))))
(add-hook 'perl-mode-hook 'my/perl-mode-hook)

(defun my/ruby-mode-hook ()
  (highlight-regexp "#\s\*TODO:\?" 'hi-yellow)
  (highlight-regexp "#\s\*FIXME:\?" 'hi-pink)
  (highlight-regexp "#\s\*NOTE:\?" 'hi-yellow)
  (yas-minor-mode)
  (linum-mode 1)
  (flymake-mode)
  (flymake-ruby-load)
  (rbenv-use-corresponding))
(add-hook 'ruby-mode-hook 'my/ruby-mode-hook)

(defun my/go-mode-hook ()
  (setq tab-width 2)
  (linum-mode 1)
  (add-hook 'before-save-hook 'gofmt-before-save))
(add-hook 'go-mode-hook 'my/go-mode-hook)

(defun my/c-mode-hook ()
  (setq tab-width 4)
  (linum-mode 1))
(add-hook 'c-mode-hook 'my/c-mode-hook)

;;;
;;; Magit
;;;

(defalias 'blame 'magit-blame-addition)
(defalias 'b 'blame)
(defalias 'status 'magit-status)
(defalias 's 'status)

(setq pretty '(sh c js python perl lisp))
;; Linum-mode for all files listed in `line-them'
(dolist (elt pretty)
  (add-hook (intern (concat (symbol-name elt) "-mode" "-hook"))
			(lambda()
			  (linum-mode 1)
        (setq-default indent-tabs-mode nil))))


;; Dockerfile mode
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;; Perl mode for test files
(add-to-list 'auto-mode-alist '("\\.t\\'" . perl-mode))


;; Slime
(setq inferior-lisp-program "sbcl")
(setq slime-contribs '(slime-fancy))


;; PATH to ag command
(add-to-list 'exec-path "/usr/local/bin")


;; The silver searcher aliases
(defalias 'agr 'helm-do-ag-project-root)
(defalias 'ag 'helm-do-ag)

;;;
;;; Eshell aliases
;;;

(defalias 'eshell/l 'eshell/ls)
(defalias 'eshell/ll 'eshell/ls)

(defun eshell/ff (file)
  (find-file-other-window file))

(defun eshell/f (file)
  (find-file file))

(defun eshell/clear ()
  "Clear the eshell buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

;;;
;;; Custom functions that can be used via M-x comman
;;; More elisp examples can be found here:
;;;              http://ergoemacs.org/emacs/elisp_examples.html
;;;

(defun etags ()
  "Create etags in a git project"
  (interactive)
  (let* ((dir (substring
               (shell-command-to-string "git rev-parse --show-toplevel")
               0 -1))
         (isdir (file-directory-p dir))
         (display-buffer-alist
          (list
           (cons
            "\\*Async Shell Command\\*.*"
            (cons #'display-buffer-no-window nil)))))
    (if isdir
        (async-shell-command
         (format "(cd %s ; find . -type f -not -path './node_modules/*' | etags -) &"
                 dir))
      (message "You are not in a git project"))))

(defun eshell-new()
  "Open a new instance of eshell."
  (interactive)
  (eshell 'N))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (delete-other-windows)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defalias 'killo 'kill-other-buffers)

(defun today ()
  "Insert current date under cursor"
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun now ()
  "Insert current date and time under cursor"
  (interactive)
  (insert (format-time-string "%Y-%m-%d-%H%M%S")))

(defun sync ()
  "Stage all, commit and push to upstream"
  (interactive)
  (magit-stage-modified)
  (magit-commit)
  (magit-push-current-to-upstream))

(defun replace-all (directory from to)
  "Query replace all occuries in found files"
  (interactive "DDirectory: \nsReplace regexp: \nsTo: ")
  (find-grep-dired directory from)
  (when (yes-or-no-p "Start query replace?")
    (dired-toggle-marks)
    (dired-do-query-replace-regexp from to)))

;;;
;;; Keybindings
;;;

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x f") 'helm-projectile-find-file)
(global-set-key (kbd "C-x p") 'magit-pull-from-upstream)
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "C-S-<iso-lefttab>") 'other-frame)
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
(global-set-key (kbd "C-x v") 'view-mode)


;;;
;;; X-settings for window-system
;;;

(when window-system
  (tool-bar-mode 0)
  (scroll-bar-mode 0)
  (custom-set-variables
   '(initial-frame-alist (quote ((fullscreen . maximized)))))
  (when (display-graphic-p)
    (global-hl-line-mode -1)
    (load-theme 'cyberpunk)
    (linum-mode)
    (set-foreground-color "#ccc")
    (set-face-foreground 'linum "#666")
    (set-cursor-color "#cccccc")
    (set-face-background 'fringe "#000")
    (set-face-background 'linum "#000")
    )
  (server-start))
