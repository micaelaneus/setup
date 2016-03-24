(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backup"))))
 '(column-number-mode t)
 '(indent-tabs-mode nil)
 '(js-indent-level 2)
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "http://melpa.milkbox.net/packages/"))))
 '(savehist-mode t)
 '(truncate-lines t)
 '(visible-bell t)
 '(web-mode-markup-indent-offset 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Ensure installed

(defvar packages
  '(projectile
    company
    helm
    helm-projectile
    helm-company
    flycheck
    clojure-mode
    magit
    midje-mode
    cider
    paredit
    go-mode
    groovy-mode
    rhtml-mode
    rinari))

(defun packages-installed-p ()
  (if (remove-if 'package-installed-p packages)
      nil
    t))

(defun packages-install ()
  (dolist (package packages)
    (when (not (package-installed-p package))
      (package-install package))))

;; Cider
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

;; Midje
(add-hook 'clojure-mode-hook 'midje-mode)

;; Paredit
(add-hook 'eval-expression-minibuffer-setup-hook 'paredit-mode)
(add-hook 'ielm-mode-hook                        'paredit-mode)
(add-hook 'lisp-mode-hook                        'paredit-mode)
(add-hook 'lisp-interaction-mode-hook            'paredit-mode)
(add-hook 'scheme-mode-hook                      'paredit-mode)
(add-hook 'clojure-mode-hook                     'paredit-mode)
(add-hook 'cider-repl-mode-hook                  'paredit-mode)

;; Go
(add-hook 'go-mode-hook (lambda ()
                            (if (not (string-match "go" compile-command))
                                (set (make-local-variable 'compile-command)
                                     "go build -v && go test -v && go vet"))))
(add-hook 'before-save-hook #'gofmt-before-save)

;; Ruby
(add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . rhtml-mode))
(add-hook 'rhtml-mode-hook
     	  (lambda () (rinari-launch)))

;;; After init
(defun after-init ()
  ;; Ensure installed
  (package-initialize)
  (require 'cl)
  (when (not (packages-installed-p))
    (package-refresh-contents)
    (packages-install))
  ;; Projectile
  (projectile-global-mode)
  ;; Company
  (global-company-mode)
  ;; Paredit
  (autoload 'paredit-mode "paredit" nil t)
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode))
(add-hook 'after-init-hook 'after-init)
