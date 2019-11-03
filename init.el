;; My Emacs config
;;
;; No Spacesmacs but we imitate its keybindings just enough
;; to keep our muscle memory alive.
;;
;; TODO
;;
;; - cider

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; package configuration

(require 'package)

;; List the packages you want
(setq package-list '(evil
                     evil-leader
                     solarized-theme
                     aggressive-indent
                     clojure-mode
                     cider
                     markdown-mode
                     use-package
                     exec-path-from-shell
                     ws-butler
                     dumb-jump
                     helm
                     helm-projectile
                     helm-ag
                     mode-line-bell
                     evil-lisp-state
                     evil-smartparens
                     expand-region
                     projectile
                     visual-fill-column
                     smooth-scrolling
                     smartparens
                     simpleclip))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load packages

;; FIXME: do we need this?
(setq package-check-signature nil)

;; using http here instead of https, because of
;; perennial SSL problems

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://stable.melpa.org/packages/")))

; Activate all the packages (in particular autoloads)
(package-initialize)

; Update your local package index
(unless package-archive-contents
  (package-refresh-contents))

; Install all missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load extra files

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(add-to-list 'load-path (expand-file-name "~/emacs.d/vendor"))
(add-to-list 'load-path (expand-file-name "~/emacs.d/elisp"))
(load-file (expand-file-name "~/emacs.d/elisp/my-functions.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (cider aggressive-indent dumb-jump lsp-mode mode-line-bell helm-projectile markdown-mode helm-ag evil-lisp-state ws-butler evil-smartparens use-package smartparens evil-leader evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq helm-ag-base-command "rg --no-heading")
(setq helm-M-x-fuzzy-match t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; startup

(load "server")
(unless (server-running-p) (server-start))
(setq inhibit-startup-screen t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; appearance

(load-theme 'solarized-light t)
(setq default-frame-alist '((font . "Menlo-15")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; behavior

;; backups

(setq backup-directory-alist '(("." . (expand-file-name "~/.emacs.d/backup"))))

;; misc

(global-auto-revert-mode 1)
(require 'better-defaults)
(require 'simpleclip)
(simpleclip-mode 1)
(mode-line-bell-mode)

;; scrolling

(require 'smooth-scrolling)
(smooth-scrolling-mode 1)
(setq smooth-scroll-margin 5)

;; whitespace

(require 'ws-butler)
(add-hook 'prog-mode-hook #'ws-butler-mode)

;; expand-region

(setq expand-region-contract-fast-key "V")

;; aggressive-indent

(mapc (lambda (hook)
        (add-hook hook 'aggressive-indent-mode))
      '(clojure-mode-hook))

;; lisp

(use-package smartparens-config
  :ensure smartparens
  :config (progn (show-smartparens-global-mode t)))

;; FIXME: should we turn this on only for lisp modes?
(add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
(add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)

(require 'evil-lisp-state)

(require 'cider)
(setq cider-repl-pop-to-buffer-on-connect nil)

;; javascript

(setq-default js2-basic-offset 2)
(setq-default js-indent-level 2)

;; text

(add-hook 'visual-line-mode-hook 'visual-fill-column-mode)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; lsp

(use-package lsp-mode
  :ensure t
  :commands lsp
  :config
  (add-to-list 'lsp-language-id-configuration '(clojure-mode . "clojure-mode"))
  :init
  (setq lsp-enable-indentation nil)
  (setq lsp-enable-snippet nil)
  (add-hook 'clojure-mode-hook #'lsp)
  (add-hook 'clojurec-mode-hook #'lsp)
  (add-hook 'clojurescript-mode-hook #'lsp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keyboard

(require 'evil)
(evil-mode t)

(require 'evil-leader)
(global-evil-leader-mode)

(evil-leader/set-leader ",")
(evil-leader/set-key
  "e" 'eval-region
  "d" 'eval-defun)

(mapc (lambda (state)
        (evil-global-set-key state (kbd "SPC SPC") 'helm-M-x)
        (evil-global-set-key state (kbd "SPC b b") 'helm-mini)
        (evil-global-set-key state (kbd "SPC b d") 'kill-current-buffer)
        (evil-global-set-key state (kbd "SPC w m") 'delete-other-windows)
        (evil-global-set-key state (kbd "SPC f f") 'helm-find-files)
        (evil-global-set-key state (kbd "SPC /") 'helm-do-ag-project-root)
        (evil-global-set-key state "U" 'backward-up-list)
        (evil-global-set-key state "R" 'down-list)
        (evil-global-set-key state "L" 'sp-forward-sexp)
        (evil-global-set-key state "H" 'sp-backward-sexp)
        (evil-global-set-key state ";" 'mark-sexp)
        (evil-global-set-key state (kbd "SPC o f") 'fill-paragraph)
        (evil-global-set-key state (kbd "SPC o o") 'find-primary-proj)
        (evil-global-set-key state (kbd "SPC j j") 'dumb-jump-go)
        (evil-global-set-key state (kbd "SPC j t") 'dumb-jump-go-prompt)
        (evil-global-set-key state (kbd "SPC r l") 'helm-resume)
        (evil-global-set-key state (kbd "SPC r y") 'helm-show-kill-ring)
        (evil-global-set-key state (kbd "SPC p f") 'helm-projectile)
        (evil-global-set-key state (kbd "SPC f e d") 'find-init-el)
        (evil-global-set-key state (kbd "SPC m c") 'cider-force-connect)
        (evil-global-set-key state (kbd "SPC m k") 'cider-force-eval-buffer)
        (evil-global-set-key state (kbd "SPC s c") 'evil-ex-nohighlight)
        (evil-global-set-key state (kbd "SPC c l") 'comment-line))
      '(normal visual))

(mapc (lambda (state)
        (evil-global-set-key state (kbd "SPC v") 'er/expand-region)
        (evil-global-set-key state (kbd "SPC k s") 'evil-lisp-state-sp-forward-slurp-sexp))
      '(normal))

(evil-define-minor-mode-key 'motion 'visual-line-mode "j" 'evil-next-visual-line)
(evil-define-minor-mode-key 'motion 'visual-line-mode (kbd "<down>") 'evil-next-visual-line)
(evil-define-minor-mode-key 'motion 'visual-line-mode "k" 'evil-previous-visual-line)
(evil-define-minor-mode-key 'motion 'visual-line-mode (kbd "<up>") 'evil-previous-visual-line)
(evil-define-minor-mode-key 'motion 'visual-line-mode "0" 'evil-beginning-of-visual-line)
(evil-define-minor-mode-key 'motion 'visual-line-mode "$" 'evil-end-of-visual-line)
