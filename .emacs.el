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
 '(projectile-keymap-prefix (kbd "C-M-p"))
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
  ;;(add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;; (require 'benchmark-init)
;; (add-hook 'after-init-hook #'benchmark-init/deactivate)

(require 'cl)

;; Ensure installed

(defvar packages
  '(exec-path-from-shell
    use-package
    projectile
    org
    benchmark-init
    company
    helm
    helm-projectile
    helm-company
    flycheck
    magit
    lsp-mode
    lsp-ui
    company-lsp
    clojure-mode
    cider
    midje-mode
    paredit
    intero
    go-mode
    company-go
    pyenv-mode-auto
    lsp-python
    lsp-java
    js2-mode
    company-tern
    web-mode
    groovy-mode
    rhtml-mode
    rinari
    auctex
    yaml-mode
    edit-server
    gmail-message-mode))

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

;; Org
(use-package org-id
  :commands (org-store-link org-agenda org-capture org-switchb)
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c b" . org-switchb)))

;; Edit Server
(require 'edit-server)
(edit-server-start)

;; Company
(use-package company
  :commands (company-complete)
  :bind (("M-<tab>" . company-complete)))

;; Helm
(use-package helm
  :commands (helm-M-x helm-show-kill-ring helm-mini helm-find-files helm-occur helm-all-mark-rings)
  :bind (("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("C-c h o" . helm-occur)
         ("C-h <spc>" . helm-all-mark-rings)))

;; Magit
(use-package magit
  :commands (magit-status magit-dispatch-popup)
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup)))

;; Cider

;; Midje
(with-eval-after-load "midje-mode"
  (let ((prefix-map (lookup-key midje-mode-map (kbd "C-c"))))
    (define-key midje-mode-map (kbd "C-c") nil)
    (define-key midje-mode-map (kbd "C-c C-m") prefix-map)))
(add-hook 'clojure-mode-hook 'midje-mode)

;; Paredit
(add-hook 'eval-expression-minibuffer-setup-hook 'paredit-mode)
(add-hook 'ielm-mode-hook                        'paredit-mode)
(add-hook 'lisp-mode-hook                        'paredit-mode)
(add-hook 'lisp-interaction-mode-hook            'paredit-mode)
(add-hook 'scheme-mode-hook                      'paredit-mode)
(add-hook 'clojure-mode-hook                     'paredit-mode)
(add-hook 'cider-repl-mode-hook                  'paredit-mode)
(add-hook 'emacs-lisp-mode-hook                  'paredit-mode)

;; Haskell
(add-hook 'haskell-mode-hook 'intero-mode)

;; Go
(add-hook 'go-mode-hook (lambda ()
                          (if (not (string-match "go" compile-command))
                              (set (make-local-variable 'compile-command)
                                   "go build -v && go test -v && go vet"))
                          (add-hook 'before-save-hook #'gofmt-before-save)
                          (set (make-local-variable 'company-backends) '(company-go))
                          (local-set-key (kbd "M-.") 'godef-jump)
                          (local-set-key (kbd "M-*") 'pop-tag-mark)))

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

(let ((path "~/.emacs_local.el"))
  (setq custom-file path)
  (if (file-exists-p path)
    (load-file path)))

(defun after-init ()
  ;; exec-path-from-shell
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "GOPATH"))
  ;; Projectile
  (projectile-mode 1)
  ;; Company
  (global-company-mode)
  ;; Flycheck
  (global-flycheck-mode)
  ;; Helm
  (helm-mode 1)
  (helm-projectile-on)
  ;; Paredit
  (autoload 'paredit-mode "paredit" nil t))
(add-hook 'after-init-hook #'after-init)
