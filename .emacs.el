(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backup"))))
 '(column-number-mode t)
 '(debug-on-error t)
 '(default-frame-alist (quote ((fullscreen . maximized))))
 '(enable-remote-dir-locals t)
 '(exec-path-from-shell-check-startup-files nil)
 '(helm-command-prefix-key "C-x h")
 '(indent-tabs-mode nil)
 '(js-indent-level 2)
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-deadline-is-shown (quote repeated-after-deadline))
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-sorting-strategy
   (quote
    ((agenda deadline-up scheduled-up)
     (todo priority-down category-keep)
     (tags priority-down category-keep)
     (search category-keep))))
 '(org-catch-invisible-edits (quote show-and-error))
 '(org-goto-auto-isearch nil)
 '(org-id-link-to-org-use-id (quote create-if-interactive-and-no-custom-id))
 '(org-log-done (quote time))
 '(org-log-into-drawer t)
 '(projectile-completion-system (quote helm))
 '(projectile-keymap-prefix (kbd "C-c C-p"))
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

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (setq package-archives '())
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(use-package benchmark-init
  :ensure t
  :demand t
  :hook (after-init . benchmark-init/deactivate))

(require 'cl)

;; Ensure installed

(defvar packages
  '(use-package
    pyenv-mode-auto
    lsp-python
    lsp-java
    js2-mode
    company-tern
    web-mode
    groovy-mode
    rhtml-mode
    rinari
    yaml-mode))

(defun packages-installed-p ()
  (if (remove-if 'package-installed-p packages)
      nil
    t))

(defun packages-install ()
  (dolist (package packages)
    (when (not (package-installed-p package))
      (package-install package))))

(defun bootstrap ()
  (interactive)
  (when (not (packages-installed-p))
    (package-refresh-contents)
    (packages-install)))

(eval-when-compile
  (require 'use-package))

(use-package exec-path-from-shell
  :ensure t
  :demand t
  :config (when (memq window-system '(mac ns))
            (exec-path-from-shell-initialize)
            (exec-path-from-shell-copy-env "GOPATH")))

(use-package projectile
  :ensure t
  :demand t
  :config (projectile-mode 1))

;; Org
(use-package org
  :ensure t
  :commands (org-store-link org-agenda org-capture org-switchb)
  :config (require 'org-id)
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c b" . org-switchb)
         ("C-c o r" . org-revert-all-org-buffers)))

;; Edit Server
(use-package edit-server
  :ensure t
  :demand t
  :config (edit-server-start))

(use-package gmail-message-mode
  :ensure t
  :demand t
  :after (edit-server))

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
  :after (clojure-mode))
(use-package midje-mode
  :ensure t
  :after (clojure-mode)
  :config (let ((prefix-map (lookup-key midje-mode-map (kbd "C-c"))))
            (define-key midje-mode-map (kbd "C-c") nil)
            (define-key midje-mode-map (kbd "C-c C-m") prefix-map))
  :hook (clojure-mode))

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
          emacs-lisp-mode) . paredit-mode))

;; Haskell
(use-package haskell-mode
  :ensure t)
(use-package intero
  :ensure t
  :hook (haskell-mode . intero-mode))

;; Go
(use-package go-mode
  :ensure t
  :config (if (not (string-match "go" compile-command))
              (set (make-local-variable 'compile-command)
                   "go build -v && go test -v && go vet"))
          (add-hook 'before-save-hook #'gofmt-before-save)
          (set (make-local-variable 'company-backends) '(company-go))
          (local-set-key (kbd "M-.") 'godef-jump)
          (local-set-key (kbd "M-*") 'pop-tag-mark))
(use-package company-go
  :ensure t
  :after (go-mode)
  :commands (company-go))

;; Python
(add-hook 'python-mode-hook (lambda ()
                              (require 'lsp-python)
                              (lsp-python-enable)))

;; Java
(add-hook 'java-mode-hook #'lsp-java-enable)

;; JavaScript
(add-to-list 'auto-mode-alist `(,(rx ".js" string-end) . js2-mode))
(add-hook 'js2-mode-hook (lambda ()
                           (set (make-local-variable 'company-backends) '(company-tern))))

;; Web
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; Ruby
(add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . rhtml-mode))
(add-hook 'rhtml-mode-hook
     	  (lambda () (rinari-launch)))

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

(let ((path "~/.emacs_local.el"))
  (setq custom-file path)
  (if (file-exists-p path)
    (load-file path)))
