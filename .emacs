(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backup"))))
 '(column-number-mode t)
 '(global-company-mode t)
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
  '(flycheck
    clojure-mode
    ; magit
    midje-mode
    cider
    paredit
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

;; Ruby
(add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . rhtml-mode))
(add-hook 'rhtml-mode-hook
     	  (lambda () (rinari-launch)))

;;; After init
(defun after-init ()
  ; Ensure installed
  (package-initialize)
  (require 'cl)
  (when (not (packages-installed-p))
    (package-refresh-contents)
    (packages-install))
  ; Paredit
  (autoload 'paredit-mode "paredit" nil t)
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode))
(add-hook 'after-init-hook 'after-init)
