(defvar my-org-root-dir)

(let ((path "~/.emacs_local_pre_custom.el"))
  (if (file-exists-p path)
    (load-file path)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms (backquote ((".*" "~/.emacs.d/auto-save/" t))))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backup"))))
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
    ("41c8c11f649ba2832347fe16fe85cf66dafe5213ff4d659182e25378f9cfc183" default)))
 '(debug-on-error t)
 '(default-frame-alist (quote ((fullscreen . maximized))))
 ; '(desktop-save t)
 ; '(desktop-save-mode t)
 '(enable-remote-dir-locals t)
 '(exec-path-from-shell-check-startup-files nil)
 '(global-hl-line-mode t)
 '(helm-command-prefix-key "C-x h")
 '(helm-swoop-move-to-line-cycle t)
 '(helm-swoop-speed-or-color t)
 '(helm-swoop-split-direction 'split-window-vertically)
 '(helm-swoop-split-with-multiple-windows nil)
 '(helm-swoop-use-fuzzy-match t)
 '(helm-swoop-use-line-number-face t)
 '(hl-line-face (quote my-hl-line))
 '(indent-tabs-mode nil)
 '(js-indent-level 2)
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p t)
 ;;Used for Beancount: '(ledger-post-amount-alignment-column 80)
 '(nrepl-log-messages t)
 '(projectile-completion-system (quote helm))
 '(projectile-keymap-prefix (kbd "C-c M-c M-p"))
 '(savehist-mode t)
 '(show-trailing-whitespace t)
 '(truncate-lines t)
 '(visible-bell nil)
 '(web-mode-markup-indent-offset 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(defface my-hl-line '((t (:inherit hl-line :background "grey25"))) "my-hl-line face")

(defalias 'yes-or-no-p 'y-or-n-p)
; (make-directory (expand-file-name "desktop/" user-emacs-directory) :parents)
(make-directory (expand-file-name "auto-save/" user-emacs-directory) :parents)

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (setq package-archives '())
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")) t)
  (setq package-archive-priorities '((melpa . 10)
                                     (gnu . 5))))
(package-initialize)

(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(defmacro use-package-customize-load-path (name load-path-fn &rest args)
  `(use-package ,name
     ,@(let ((load-path load-path-fn))
         (if load-path
             '(:load-path load-path)
           '()))
     ,@args))

(use-package benchmark-init
  :ensure t
  :demand t
  :hook (after-init . benchmark-init/deactivate))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(add-hook 'dired-load-hook
          (lambda ()
            (load "dired-x")))
(add-hook 'dired-mode-hook
          (lambda ()))

(use-package exec-path-from-shell
  :ensure t
  :demand t
  :config (when (memq window-system '(mac ns))
            (exec-path-from-shell-initialize)
            (exec-path-from-shell-copy-env "MANPATH")
            (exec-path-from-shell-copy-env "GOPATH")))

(use-package visual-fill-column
  :ensure t
  :hook (visual-line-mode))

(use-package woman
  :ensure t
  :config (setq woman-path (woman-parse-colon-path (getenv "MANPATH"))))

(use-package async
  :ensure t
  :config (dired-async-mode 1))

(use-package request
  :ensure t)

(use-package projectile
  :ensure t
  :demand t
  :config
  (projectile-mode 1))

;; Company
(use-package company
  :ensure t
  :demand t
  :commands (company-complete)
  :config (global-company-mode)
  :bind (("M-<tab>" . company-complete)))

;; Helm
(use-package helm
  :ensure t
  :demand t
  :commands (helm-M-x helm-show-kill-ring helm-mini helm-find-files helm-occur helm-all-mark-rings)
  :config (helm-mode 1)
  :bind (("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("C-c h o" . helm-occur)
         ("C-h <spc>" . helm-all-mark-rings)))

(use-package helm-projectile
  :ensure t
  :demand t
  :after (helm)
  :config (helm-projectile-on))

(use-package helm-company
  :ensure t
  :demand t
  :after (helm company))

(use-package helm-swoop
  :ensure t
  :demand t
  :after (helm)
  :config
  (setq helm-multi-swoop-edit-save t)
  (define-key isearch-mode-map (kbd "C-h") 'helm-swoop-from-isearch)
  (define-key helm-swoop-map (kbd "M-S") 'helm-multi-swoop-all-from-helm-swoop)
  (define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
  (define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
  (define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)
  (define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
  :bind (("C-c C-s" . helm-swoop)
         ("C-c C-r" . helm-show-back-to-last-point)
         ("C-c M-s" . helm-multi-swoop)
         ("C-c M-S" . helm-multi-swoop-all)))

;; Flycheck
(use-package flycheck
  :ensure t
  :demand t
  :config (global-flycheck-mode))

;; Magit
(use-package magit
  :ensure t
  :commands (magit-status magit-dispatch-popup)
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup)))

(use-package lsp-mode
  :ensure t
  :demand t)
(use-package lsp-ui
  :ensure t
  :demand t
  :after (lsp-mode))
(use-package company-lsp
  :ensure t
  :demand t
  :after (lsp-mode company))

;; Clojure
;; Cider
(use-package clojure-mode
  :ensure t)
(use-package cider
  :ensure t
  :after (clojure-mode company-mode)
  :hook ((cider-repl-mode-hook . company-mode)
         (cider-mode-hook . company-mode))
  :config
  (define-key cider-mode-map (kbd "C-c M-c") nil)
  (cider-register-cljs-repl-type
   'figwheel--chestnut
   "(do (user/go) (user/cljs-repl))"
   'cider-check-figwheel-requirements))
(use-package midje-mode
  :ensure t
  :after (clojure-mode)
  :hook (clojure-mode)
  :config
  (let ((prefix-map (lookup-key midje-mode-map (kbd "C-c"))))
    (define-key midje-mode-map (kbd "C-c") nil)
    (define-key midje-mode-map (kbd "C-c C-m") prefix-map)))

;; Paredit
(use-package paredit
  :ensure t
  :commands (paredit-mode)
  :hook ((eval-expression-minibuffer-setup
          ielm-mode
          lisp-mode
          lisp-interaction-mode
          scheme-mode
          clojure-mode
          cider-repl-mode
          emacs-lisp-mode)
         . paredit-mode))

;; Haskell
(use-package haskell-mode
  :ensure t)
(use-package intero
  :ensure t
  :hook (haskell-mode . intero-mode))

;; Go
(use-package go-mode
  :ensure t
  :bind (:map
         go-mode-map
         ("M-." . godef-jump)
         ("M-*" . pop-tag-mark))
  :hook (go-mode-hook . (lambda ()
                          (make-local-variable 'before-save-hook)
                          (add-hook 'before-save-hook #'gofmt-before-save)))
  :config
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet")))
(use-package company-go
  :ensure t
  :after (go-mode)
  :hook (go-mode . (lambda ()
                     (set (make-local-variable 'company-backends) '(company-go))))
  :commands (company-go))

;; Python
(use-package pyenv-mode-auto
  :ensure t)
;; (use-package lsp-python
;;   :ensure t
;;   :commands (lsp-python-enable)
;;   :hook (python-mode . lsp-python-enable))

;; Java
(use-package lsp-java
  :ensure t
  :commands (lsp-java-enable)
  :hook (java-mode . lsp-java-enable))

;; JavaScript
(use-package js2-mode
  :ensure t
  :mode ("\\.js\\'"))
(use-package company-tern
  :ensure t
  :hook (js2-mode . (lambda ()
                      (set (make-local-variable 'company-backends) '(company-tern))))
  :commands (company-tern))

;; Web
(use-package web-mode
  :mode ("\\.phtml\\'"
         "\\.tpl\\.php\\'"
         "\\.[agj]sp\\'"
         "\\.as[cp]x\\'"
         "\\.erb\\'"
         "\\.mustache\\'"
         "\\.djhtml\\'"
         "\\.html?\\'"))

;; Ruby
(use-package groovy-mode
  :ensure t)
(use-package rhtml-mode
  :ensure t
  :mode ("\\.html\\.erb\\'"))
(use-package rinari
  :hook (rhtml-mode . rinari-launch)
  :commands (rinari-launch))

(use-package yaml-mode
  :ensure t)

;; TeX
(use-package tex
  :ensure auctex
  :init
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil))
(use-package company-auctex
  :ensure t
  :after (tex company)
  :hook (TeX . company-auctex))

(use-package vagrant-tramp
  :ensure t)

;; Beancount
(use-package beancount
  :load-path "~/.beancount/editors/emacs"
  :mode
  ("\\.beancount\\'" . beancount-mode)
  ("\\.beancount\\.org\\'" . beancount-mode)
  :config (setq beancount-install-dir "~/.pyenv/versions/3.7.0"))

(use-package darcula-theme
  :ensure t)

(let ((path "~/.emacs_local.el"))
  (if (file-exists-p path)
    (load-file path)))

(let ((path "~/.emacs_custom_set.el"))
  (if (file-exists-p path)
    (delete-file path))
  (setq custom-file path)
  (customize-save-customized))
(load custom-file)
