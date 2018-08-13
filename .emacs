(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backup"))))
 '(column-number-mode t)
 '(enable-remote-dir-locals t)
 '(exec-path-from-shell-check-startup-files nil)
 '(helm-command-prefix-key "C-x h")
 '(indent-tabs-mode nil)
 '(js-indent-level 2)
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p t)
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "http://melpa.milkbox.net/packages/"))))
 '(projectile-completion-system (quote helm))
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

;; Ensure installed

(defvar packages
  '(exec-path-from-shell
    projectile
    org
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
    yaml-mode))

(defun packages-installed-p ()
  (if (remove-if 'package-installed-p packages)
      nil
    t))

(defun packages-install ()
  (dolist (package packages)
    (when (not (package-installed-p package))
      (package-install package))))

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
(add-hook 'python-mode-hook #'lsp-python-enable)

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

;;; After init
(defun after-init ()
  ;; Ensure installed
  (package-initialize)
  (require 'cl)
  (when (not (packages-installed-p))
    (package-refresh-contents)
    (packages-install))
  ;; exec-path-from-shell
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "GOPATH"))
  ;; Projectile
  (projectile-mode 1)
  ;; Company
  (global-company-mode)
  (global-set-key (kbd "M-TAB") 'company-complete)
  ;; Flycheck
  (global-flycheck-mode)
  ;; Helm
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "C-x b") 'helm-mini)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-c h o") 'helm-occur)
  (global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
  (helm-mode 1)
  (helm-projectile-on)
  ;; Magit
  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)
  ;; Paredit
  (autoload 'paredit-mode "paredit" nil t)
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  ;; Python
  (require 'lsp-python))
(add-hook 'after-init-hook 'after-init)

(let ((path "~/.emacs_local"))
  (setq custom-file path)
  (if (file-exists-p path)
    (load-file path)))
