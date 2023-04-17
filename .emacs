;;; .emacs --- My Emacs configuration

;; 2022 (C) mrexox

;;; Code:

;; Global variables
(defvar my/font "Monoid 14")
(defvar my/title-format
  (list
   (format "%s %%S: %%j " (system-name))
   '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;; Window system settings
(when window-system
  (tool-bar-mode 0)
  (scroll-bar-mode 0)
  (tooltip-mode 0)
  (custom-set-variables '(initial-frame-alist (quote ((fullscreen . maximized)))))
  (server-start))

;; Use C-c C-e for fast editing .emacs
(global-set-key (kbd "C-c C-e") '(lambda () (interactive) (find-file "~/.emacs")))
;; Use C-c C-o to open organizational folder with org files
(global-set-key (kbd "C-c C-o") '(lambda () (interactive) (find-file "~/Documents/organizational")))
(global-set-key (kbd "C-c C-v") 'view-mode)
(global-set-key (kbd "C-c C-r") 'revert-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "C-S-<iso-lefttab>") 'other-frame)
(global-set-key "\M-g" 'goto-line)
(global-set-key [f9] 'kill-other-buffers)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Set the preferred preferences
(setq-default
 tab-width 2
 indent-tabs-mode nil
 gc-cons-threshold most-positive-fixnum
 gc-cons-percentage 0.6)
(add-hook 'focus-out-hook 'garbage-collect)

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

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
 visible-cursor 1
 calendar-week-start-day 1
 initial-scratch-message "\n\n\n")

;; Modes
(display-battery-mode 0)
(display-time-mode 0)
(global-hl-line-mode 0)
(global-prettify-symbols-mode 0) ; enable if you need ligatures and lambda
(global-subword-mode 1)
(ido-mode 0) ; Not very useful
(line-number-mode 1)
(menu-bar-mode 0)
(windmove-default-keybindings)

;; Goodies
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

;; Load custom file
(setq-default custom-file (expand-file-name ".custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Load secret file
(let ((secret.el (expand-file-name ".secret.el" user-emacs-directory)))
  (when (file-exists-p secret.el)
    (load secret.el)))

;; Packages
(require 'package)
(add-to-list 'package-archives '("melpa" ."https://melpa.org/packages/") t)
(package-initialize)

;; 'use-package' configuration
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(use-package crystal-mode
  :hook ((crystal-mode . linum-mode))
  :config
  (define-key crystal-mode-map (kbd "M-.") #'crystal-def-jump))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :mode "\\.js\\'"
  :hook ((typescript-mode . linum-mode))
  :config
  (setq typescript-indent-level 2))

(use-package lsp-mode
  :after js
  :hook (
         (js-mode . lsp-deferred)
         (typescript-mode . lsp-deferred)
         )
  :config
  (define-key js-mode-map (kbd "M-.") nil)) ;; don't prompt when jumping

(use-package lsp-ivy
  :after lsp
  :after ivy)

(use-package anzu
  :config
  (global-anzu-mode 1)
  (global-set-key (kbd "M-r") 'anzu-replace-at-cursor-thing)
  (defalias 'replace 'anzu-query-replace-regexp))

;; Color scheme
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

(use-package diminish
  :config (diminish 'projectile-mode))

(use-package ivy
  :config
  (ivy-mode 1)
  ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  ;; number of result lines to display
  (setq ivy-height 8)
  ;; does not count candidates
  (setq ivy-count-format "(%d/%d) ")
  ;; no regexp by default
  (setq ivy-initial-inputs-alist nil)
  ;; configure regexp engine.
  (setq ivy-re-builders-alist
	      ;; allow input not in order
        '((t   . ivy--regex-ignore-order))))

(use-package ivy-rich
  :after ivy
  :custom
  (ivy-virtual-abbreviate 'full
                          ivy-rich-switch-buffer-align-virtual-buffer t
                          ivy-rich-path-style 'abbrev)
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer)
  (setq ivy-initial-inputs-alist nil)
  (ivy-rich-mode 1)) ;; this gets us descriptions in M-x.

;; Might be useful, but not for now
;; (use-package ivy-posframe
;;   :init
;;   (setq ivy-posframe-display-functions-alist
;;     '((swiper                     . ivy-posframe-display-at-point)
;;       (complete-symbol            . ivy-posframe-display-at-point)
;;       (counsel-M-x                . ivy-display-function-fallback)
;;       (counsel-esh-history        . ivy-posframe-display-at-window-center)
;;       (counsel-describe-function  . ivy-display-function-fallback)
;;       (counsel-describe-variable  . ivy-display-function-fallback)
;;       (counsel-find-file          . ivy-display-function-fallback)
;;       (counsel-recentf            . ivy-display-function-fallback)
;;       (counsel-register           . ivy-posframe-display-at-frame-bottom-window-center)
;;       (dmenu                      . ivy-posframe-display-at-frame-top-center)
;;       (nil                        . ivy-posframe-display)))
;;     ;; ivy-posframe-height-alist
;;     ;; '((swiper . 20)
;;     ;;   (dmenu . 20)
;;     ;;   (t . 10)))
;;   :config
;;   (ivy-posframe-mode 1)) ; 1 enables posframe-mode, 0 disables it.

;; M-x history
(use-package smex
  :config
  (smex-initialize))

(use-package counsel
  :after ivy
  :config
  (counsel-mode)
  (global-set-key (kbd "C-c C-g") 'counsel-imenu))

(use-package counsel-etags
  :ensure t
  :bind (("M-." . counsel-etags-find-tag-at-point))
  :config
  (push "build" counsel-etags-ignore-directories)
  (push "log" counsel-etags-ignore-directories))

(use-package counsel-ag-popup
  :after counsel
  :config
  (defalias 'ag 'counsel-ag-popup-search-here)
  (defun agr (&optional string)
    "Search for a string in a git project"
    (interactive)
    (let ((dir (substring
                (shell-command-to-string "git rev-parse --show-toplevel")
                0 -1)))
      (counsel-ag-popup-search dir string)))
  (defun agregion (&optional start end)
    "Search for selected string in a git project"
    (interactive "r")
    (let ((dir (substring
                (shell-command-to-string "git rev-parse --show-toplevel")
                0 -1)))
          (if (use-region-p)
              (let ((phrase (buffer-substring start end)))
                (counsel-ag-popup-search dir phrase))
            (let ((word (thing-at-point 'symbol)))
              (counsel-ag-popup-search dir word)))))
  (global-set-key (kbd "C-c C-s") 'agregion))

(use-package wgrep) ;; for global changes in ivy-occur buffer

(use-package swiper
  :after ivy
  :config
  (global-set-key (kbd "M-s") 'swiper-thing-at-point))

;; No distraction modes
(use-package darkroom)
(use-package writeroom-mode)

(use-package disable-mouse
  :config (global-disable-mouse-mode 1))

(use-package dockerfile-mode
  :mode "Dockerfile\\'")
(use-package dotenv-mode)
(use-package markdown-mode)

(use-package editorconfig
  :config (editorconfig-mode 1))

(use-package evil
  :init      ;; tweak evil's configuration before loading it
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (evil-mode 0))

(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-mode-list '(dashboard dired ibuffer))
  (evil-collection-init))

(use-package evil-tutor)

(use-package flylisp)

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

(require 'ls-lisp)
(setq
 ls-lisp-dirs-first t
 ls-lisp-use-insert-directory-program nil)

(use-package magit
  :hook ((magit-status-mode-hook . visual-line-mode))
  :config
  (defalias 'blame 'magit-blame-addition)
  (defalias 'b 'blame)
  (defalias 'status 'magit-status)
  (defalias 's 'status))

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

(require 'org-tempo)

;; (global-set-key (kbd "C-x f") 'projectile-find-file)
(use-package projectile
  :config (projectile-mode 1)
  :bind ("C-x f" . projectile-find-file))


(use-package rbenv)

(use-package slime
  :config
  (setq inferior-lisp-program "sbcl")
  (setq slime-contribs '(slime-fancy)))

;; (use-package smart-mode-line
;;   :config
;;   (when (display-graphic-p)
;;     (setq sml/no-confirm-load-theme t)
;;     (setq sml/theme 'dark)
;;     (sml/setup)))

(use-package all-the-icons)
;; I don't like it
;; (use-package all-the-icons-dired
;;   :hook ((dired-mode . all-the-icons-dired-mode)))
(use-package doom-modeline
  :config
  (setq doom-modeline-env-version t)
  (setq doom-modeline-buffer-file-name-style 'relative-from-project)
  (doom-modeline-mode 1))

(use-package undo-tree
  :config (global-undo-tree-mode 1))

;; (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
(use-package web-mode
  :mode "\\.vue\\'"
  :mode "\\.css?\\'"
  :mode "\\.html?\\'")

(use-package haml-mode)
(use-package coffee-mode)

(use-package yaml-mode
  :hook ((yaml-mode . flycheck-mode)
         (yaml-mode . linum-mode))
  :bind (:map yaml-mode-map
              ("C-m" . newline-and-indent)))

(use-package yasnippet
  :config (yas-global-mode 1)
  :bind ("M-]" . yas-expand))

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

(add-hook 'web-mode-hook (lambda ()
                           (linum-mode-hook)
                           (set (make-local-variable 'tab-width) 2)))

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
  ;;(flymake-mode)
  (rbenv-use-corresponding))
  ;;(local-set-key (kbd "M-.") 'find-ruby-reference))
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
         (format "(cd %s ; find . -type f -name '*.rb' -o -name '*.cr' -o -name '*.js' -not -path './*/node_modules/*' | etags -) &"
                 dir))
      (message "You are not in a git project"))))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (delete-other-windows)
  (mapc 'kill-buffer (delq (get-buffer "*scratch*")
                           (delq (current-buffer) (buffer-list)))))

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
