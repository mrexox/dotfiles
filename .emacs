;;; .emacs --- My Emacs configuration

;; 2022 (C) mrexox

;;; Code:

;;
;; Global variables
;;
(defvar my/font "Monoid 13")
(defvar my/title-format
  (list
   (format "%s %%S: %%j " (system-name))
   '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;;
;; Window system settings
;;
(when window-system
  (tool-bar-mode 0)
  (scroll-bar-mode 0)
  (tooltip-mode 0)
  (custom-set-variables '(initial-frame-alist (quote ((fullscreen . maximized)))))
  (server-start))

;; Use C-c C-e for fast editing .emacs
(global-set-key (kbd "C-c C-e") '(lambda () (interactive) (find-file "~/.emacs")))
(global-set-key (kbd "C-c C-v") 'view-mode)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "C-S-<iso-lefttab>") 'other-frame)
(global-set-key "\M-g" 'goto-line)
(global-set-key [f9] 'kill-other-buffers)

;;
;; Set the preferred preferences
;;
(setq-default
 tab-width 2
 indent-tabs-mode nil
 gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 800000)))
(add-hook 'focus-out-hook 'garbage-collect)

(setq
 auto-save-default nil
 backup-directory-alist '(("." . "~/.emacs.d/backups"))
 clean-buffer-list-delay-general 1 ; make buffer eligible for killing in 1 day
 column-number-mode t
 frame-title-format my/title-format
 inhibit-splash-screen t
 js-indent-level 2
 large-file-warning-threshold nil
 ruby-insert-encoding-magic-comment nil
 tags-add-tables nil ; never keep current tags table; always open without asking.
 tags-revert-without-query 1
 visible-cursor 1)

;;
;; Modes
;;
(display-battery-mode 1)
(display-time-mode 0)
(global-hl-line-mode 1)
(global-prettify-symbols-mode 0) ; enable if you need ligatures and lambda
(global-subword-mode 1)
(ido-mode 0) ; Not very useful
(line-number-mode 1)
(menu-bar-mode 0)
(windmove-default-keybindings)

;;
;; Goodies
;;
(add-to-list 'exec-path "/usr/local/bin")
(fset 'yes-or-no-p 'y-or-n-p)
(mouse-avoidance-mode 'banish)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(set-face-bold-p 'bold nil)
(set-frame-font my/font)
(mapc (lambda (face) (set-face-attribute face nil :weight 'normal :underline nil))
      (face-list))

;;
;; Load custom file
;;
(setq-default custom-file (expand-file-name ".custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;
;; Load secret file
;;
(let ((secret.el (expand-file-name ".secret.el" user-emacs-directory)))
  (when (file-exists-p secret.el)
  (load secret.el)))

;;
;; Packages
;;
(require 'package)
(add-to-list 'package-archives '("melpa" ."https://melpa.org/packages/") t)
(package-initialize)

;;
;; 'use-package' configuration
;;
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;;
;; crystal-mode
;;
(use-package crystal-mode)


;;
;; js2-mode
;;
(use-package js2-mode)
;;             :hook (js-mode . js2-minor-mode))

(use-package lsp-mode
  :config
  (with-eval-after-load 'js
    (add-hook 'js-mode-hook #'lsp-deferred)
    (define-key js-mode-map (kbd "M-.") nil)))
;;
;; cyberpunk-theme
;;
(use-package cyberpunk-theme
             :config
             (when (display-graphic-p)
               (load-theme 'cyberpunk t)
               (set-foreground-color "#ccc")
               (set-face-background hl-line-face "gray4")
               (linum-mode 1)
               (set-face-foreground 'linum "#666")
               (set-face-background 'linum "#000")
               (set-cursor-color "#cccccc")
               (set-face-background 'fringe "#000")))

;;
;; darkroom
;;
(use-package darkroom)

;;
;; disable-mouse
;;
(use-package disable-mouse
             :config (global-disable-mouse-mode 1))

;;
;; dockerfile-mode
;;
(use-package dockerfile-mode
             :mode "Dockerfile\\'")

;;
;; dotenv-mode
;;
(use-package dotenv-mode)

;;
;; editorconfig
;;
(use-package editorconfig
             :config (editorconfig-mode 1))

;;
;; evil
;;
(use-package evil)

;;
;; flylisp
;;
(use-package flylisp)

;;
;; go-mode
;;
(defun my/go-mode-hook ()
  (setq tab-width 2)
  (hl-line-mode 0)
  (linum-mode 1)
  (local-set-key (kbd "C-c C-r") #'go-remove-unused-imports)
  (local-set-key (kbd "C-c i") #'go-goto-imports)
  (local-set-key (kbd "M-.") #'godef-jump)
  (add-hook 'before-save-hook #'gofmt-before-save))

(use-package go-mode
             :hook ((go-mode . my/go-mode-hook)))
;; (add-hook 'go-mode-hook #'my/go-mode-hook)

;;
;; helm-ag
;;
(use-package helm-ag
             :config
             (helm-mode 1)
             (defalias 'agr 'helm-do-ag-project-root)
             (defalias 'ag 'helm-do-ag))

(use-package helm-projectile)
(use-package helm-xref)

;;
;; ls-lisp
;;
(require 'ls-lisp)
(setq
 ls-lisp-dirs-first t
 ls-lisp-use-insert-directory-program nil)

;;
;; magit
;;

(defun my/magit-status-mode-hook ()
  (visual-line-mode 1))

(use-package magit
             :hook ((magit-status-mode-hook . my/magit-status-mode-hook))
             :bind (("C-x p" . magit-pull-from-upstream)
                    ("C-x g" . magit-status))
             :config
             (defalias 'blame 'magit-blame-addition)
             (defalias 'b 'blame)
             (defalias 'status 'magit-status)
             (defalias 's 'status))

;;
;; multiple-cursors
;;
;; (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;; (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))
(use-package multiple-cursors
            :bind
            (("C-S-c C-S-c" . mc/edit-lines)
             ("C->" . mc/mark-next-like-this)
             ("C-<" . mc/mark-previous-like-this)
             ("C-c C-<" . mc/mark-all-like-this)))

;;
;; org-tempo
;;
(require 'org-tempo)

;;
;; projectile
;;
;; (global-set-key (kbd "C-x f") 'projectile-find-file)
(use-package projectile
             :config (projectile-mode 1)
             :bind ("C-x f" . projectile-find-file))

;;
;; rbenv
;;
(use-package rbenv)

;;
;; slime
;;
(use-package slime
             :config
             (setq inferior-lisp-program "sbcl")
             (setq slime-contribs '(slime-fancy)))

;;
;; smart-mode-line
;;
(use-package smart-mode-line
             :config
             (when (display-graphic-p)
               (setq sml/no-confirm-load-theme t)
               (setq sml/theme 'dark)
               (sml/setup)))

;;
;; undo-tree
;;
(use-package undo-tree
             :config (global-undo-tree-mode 1))

;;
;; web-mode
;;

;; (add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))

(use-package web-mode
             :mode "\\.vue\\'"
             :mode "\\.css?\\'"
             :mode "\\.html?\\'"
             :mode "\\.ts\\'")

;;
;; yaml-mode
;;
;; (add-hook 'yaml-mode-hook
;;           (lambda ()
;;             (flycheck-mode)
;;             (linum-mode-hook)
;;             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))
(defun my/yaml-mode-hook ()
  (flycheck-mode)
  (linum-mode 1))

(use-package yaml-mode
             :hook ((yaml-mode . my/yaml-mode-hook))
             :bind (:map yaml-mode-map
                         ("C-m" . newline-and-indent)))

;;
;; yasnippet
;;
(use-package yasnippet
             :config (yas-global-mode 1)
             :bind ("M-e" . yas-expand))

;;;
;;; Hooks
;;;

;;(add-hook 'lisp-mode #'linum-mode-hook)
;;(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
(add-hook 'text-mode-hook '(lambda () (visual-line-mode 1)))
(add-hook 'diary-mode-hook '(lambda () (auto-fill-mode 1)))
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'css-mode-hook '(lambda () (setq tab-width 2)))
(add-hook 'dired-mode-hook
          (lambda ()
            (dired-hide-details-mode)
            (define-key dired-mode-map "/" 'dired-narrow)
            (define-key dired-mode-map "k" 'previous-line)
            (define-key dired-mode-map "j" 'next-line)
            (define-key dired-mode-map "h" 'dired-up-directory)
            (define-key dired-mode-map "l" 'dired-view-file)))
(add-hook 'makefile-mode-hook #'linum-mode-hook)
(add-hook 'makefile-mode-hook #'(lambda () (setq indent-tabs-mode t)))
(add-hook 'eshell-mode-hook '(lambda () (global-hl-line-mode -1)))
;;(add-hook 'haml-mode-hook '(lambda () (linum-mode-hook) (company-mode 1)))

(add-hook 'web-mode-hook #'linum-mode-hook)
(add-hook 'coffee-mode-hook
          (lambda ()
            (set (make-local-variable 'tab-width) 2)
            (set (make-local-variable 'indent-tabs-mode) t)))

(defun my/perl-mode-hook ()
  (setq tab-width 4)
  (highlight-regexp "#\s\*TODO:\?" 'hi-yellow)
  (highlight-regexp "#\s\*FIXME:\?" 'hi-pink)
  (highlight-regexp "#\s\*NOTE:\?" 'hi-yellow)
  (setq indent-tabs-mode nil))
(add-hook 'perl-mode-hook 'my/perl-mode-hook)

(defun find-ruby-reference ()
  "Search for the full class Ruby reference in tags"
  (interactive)
  (push-mark (progn
               (re-search-backward "[^[:alnum:]:_!?]")
               (forward-char)
               (while (string-equal (thing-at-point 'char) ":")
                 (forward-char))
               (point)))
  (re-search-forward "\\([^[:alnum:]:_!?]\\|$\\)")
  (if (not (null (thing-at-point 'char)))
      (backward-char))
  (search-ruby-reference (buffer-substring (mark) (point))))

;; Look, how beautiful this code is ;)
(defun search-ruby-reference (phrase)
  (condition-case err
      (xref-find-definitions phrase)
    (user-error
     (if (string-match "::" phrase)
         (search-ruby-reference
          (string-join (cdr (split-string phrase "::")) "::"))
       (message ">> %s" (error-message-string err))))))

(defun my/ruby-mode-hook ()
  (highlight-regexp "#\s\*TODO:\?\.\*\$" 'hi-yellow)
  (highlight-regexp "#\s\*FIXME:\?\.\*\$" 'hi-pink)
  (highlight-regexp "#\s\*NOTE:\?\.\*\$" 'hi-yellow)
  (hl-line-mode 0) ;; temporarily off
  (linum-mode 1)
  (flymake-mode)
  (rbenv-use-corresponding)
  (local-set-key (kbd "M-.") 'find-ruby-reference))
(add-hook 'ruby-mode-hook 'my/ruby-mode-hook)

(defun my/c-mode-hook ()
  (setq tab-width 4)
  (linum-mode 1))
(add-hook 'c-mode-hook 'my/c-mode-hook)

(add-hook 'view-mode-hook
          (lambda ()
            (define-key view-mode-map "j" 'next-line)
            (define-key view-mode-map "k" 'previous-line)
            (define-key view-mode-map "l" 'forward-char)
            (define-key view-mode-map "h" 'backward-char)
            (define-key view-mode-map "H" 'previous-buffer)))
;;;
;;; Magit
;;;

(setq pretty '(sh c js python perl lisp))
;; Linum-mode for all files listed in `line-them'
(dolist (elt pretty)
  (add-hook (intern (concat (symbol-name elt) "-mode" "-hook"))
			(lambda()
			  (linum-mode 1)
        (setq-default indent-tabs-mode nil))))



;; Perl mode for test files
(add-to-list 'auto-mode-alist '("\\.t\\'" . perl-mode))

;; PATH to ag command



;; The silver searcher aliases

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
         (format "(cd %s ; find . -type f -name '*.rb' -o -name '*.js' -not -path './*/node_modules/*' | etags -) &"
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
  (mapc 'kill-buffer (delq (get-buffer "*scratch*")
                           (delq (current-buffer) (buffer-list)))))

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

(defun focus-mode ()
  "Enable focus-mode for current buffer"
  (interactive)
  (darkroom-tentative-mode))

(defun msg (&optional b e)
  "Git branch to commit message"
  (interactive "r")
  (replace-string "-" " " nil b e)
  (capitalize-region b (1+ b)))

(defun camelize (begin end)
  "Convert under_score string S to CamelCase string."
  (interactive "r")
  (let ((str (buffer-substring begin end)))
    (replace-string str
                    (mapconcat 'identity
                               (mapcar
                                '(lambda (word) (capitalize (downcase word)))
                                (split-string str "_")) "")
                    nil
                    begin
                    end)))

(cd "~/")
