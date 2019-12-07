;;; init.el --- Initialization file for Emacs
;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs
;;;
;;; No Spacesmacs but we imitate its keybindings just enough
;;; to keep our muscle memory alive.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; package configuration

(require 'package)

(let ((package-list '(evil
                      evil-leader
                      evil-lisp-state ;; do we still need this?
                      evil-visualstar
                      evil-smartparens
                      diff-hl
                      solarized-theme
                      package-lint
                      ;; zprint-mode
                      tide
                      prettier-js
                      typescript-mode
                      aggressive-indent
                      clojure-mode
                      flycheck-joker
                      git-link
                      cider
                      markdown-mode
                      use-package
                      exec-path-from-shell
                      git-timemachine
                      js2-mode
                      ws-butler
                      dumb-jump
                      helm
                      helm-projectile
                      helm-ag
                      smart-mode-line
                      mode-line-bell
                      expand-region
                      projectile
                      visual-fill-column
                      smooth-scrolling
                      smartparens
                      simpleclip)))
  (setq package-check-signature nil) ;; FIXME: do we need this?

  (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                           ("melpa" . "http://melpa.org/packages/")))

  ;; Activate all the packages (in particular autoloads)
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents))
  (dolist (package package-list)
    (unless (package-installed-p package)
      (package-install package))))

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
    (tide prettier prettier-js typescript-mode zprint-mode package-lint helm-unicode helm-chrome-control git-timemachine git-link diff-hl evil-visualstar js2-mode deadgrep smart-mode-line flycheck-jokeryy flycheck-joker cider aggressive-indent dumb-jump lsp-mode mode-line-bell helm-projectile markdown-mode helm-ag evil-lisp-state ws-butler evil-smartparens use-package smartparens evil-leader evil)))
 '(safe-local-variable-values
   (quote
    ((eval when
           (and
            (buffer-file-name)
            (not
             (file-directory-p
              (buffer-file-name)))
            (string-match-p "^[^.]"
                            (buffer-file-name)))
           (unless
               (featurep
                (quote package-build))
             (let
                 ((load-path
                   (cons "../package-build" load-path)))
               (require
                (quote package-build))))
           (unless
               (derived-mode-p
                (quote emacs-lisp-mode))
             (emacs-lisp-mode))
           (package-build-minor-mode)
           (setq-local flycheck-checkers nil)
           (set
            (make-local-variable
             (quote package-build-working-dir))
            (expand-file-name "../working/"))
           (set
            (make-local-variable
             (quote package-build-archive-dir))
            (expand-file-name "../packages/"))
           (set
            (make-local-variable
             (quote package-build-recipes-dir))
            default-directory))
     (eval define-clojure-indent
           (reg-cofx :defn)
           (reg-event-db :defn)
           (reg-event-fx :defn)
           (reg-fx :defn)
           (reg-sub :defn)
           (reg-event-domain :defn)
           (reg-block-event-fx :defn)
           (reg-event-domain-fx :defn)
           (reg-event-persistent-db :defn)
           (this-as 0))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'helm-ag)
(require 'helm-command)
(setq helm-ag-base-command "rg --no-heading --smart-case -M100")
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
;; maximize vertically
(add-to-list 'initial-frame-alist '(fullscreen . fullheight))
;; modeline
(sml/setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; behavior

;; remember how things used to be

(desktop-save-mode 1)
(save-place-mode 1)

;; chrome

(require 'helm-chrome-control)

;; git

(global-diff-hl-mode)
(diff-hl-flydiff-mode)
(require 'git-link)
(setq git-link-open-in-browser t)

(defadvice git-timemachine-mode (after git-timemachine-change-to-emacs-state activate compile)
  "Enable time traveling in git."
  (if (evil-normal-state-p)
      (evil-emacs-state)
    (evil-normal-state)))

(ad-activate 'git-timemachine-mode)

;; backups

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; misc

(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode 1)
(require 'better-defaults)
(require 'simpleclip)
(simpleclip-mode 1)
(mode-line-bell-mode)
(setq vc-follow-symlinks t) ;; avoid annoying prompt

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
      '(clojure-mode-hook
        emacs-lisp-mode-hook))

;; lisp

(require 'flycheck)
(setq flycheck-emacs-lisp-load-path 'inherit)

(require 'smartparens-config)

(dolist (hook '(clojure-mode-hook emacs-lisp-mode-hook))
  (add-hook hook 'turn-on-smartparens-strict-mode))
(add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)

(require 'evil-lisp-state)

(require 'cider)
(setq cider-repl-pop-to-buffer-on-connect nil)

;; lispy word characters

(defun clojure-word-chars ()
  "Add more special charcaters for Clojure."
  (modify-syntax-entry ?+ "w")
  (modify-syntax-entry ?- "w")
  (modify-syntax-entry ?< "w")
  (modify-syntax-entry ?> "w"))

(add-hook 'clojure-mode-hook 'clojure-word-chars)
(add-hook 'emacs-lisp-mode-hook #'(lambda () (modify-syntax-entry ?- "w")))

(require 'flycheck-joker)
(global-flycheck-mode)

;; javascript

(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(setq-default js2-basic-offset 2)
(setq-default js-indent-level 2)
(setq-default typescript-indent-level 2)

(defun setup-tide-mode ()
  (interactive)

  (prettier-js-mode +1)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

;; text

(add-hook 'visual-line-mode-hook 'visual-fill-column-mode)
(add-hook 'visual-line-mode-hook (lambda () (setq fill-column 90)))
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; lsp

;; (use-package lsp-mode
;;   :ensure t
;;   :commands lsp
;;   :config

;;   (add-to-list 'lsp-language-id-configuration '(js2-mode . "js2-mode"))

;;   ;; (add-to-list 'lsp-language-id-configuration '(clojure-mode . "clojure-mode"))
;;   ;; (add-to-list 'lsp-language-id-configuration '(clojurec-mode . "clojurec-mode"))
;;   ;; (add-to-list 'lsp-language-id-configuration '(clojurescript-mode . "clojurescript-mode"))

;;   :init
;;   (setq lsp-enable-indentation nil)
;;   (setq lsp-enable-snippet nil)
;;   (add-hook 'clojure-mode-hook #'lsp)
;;   (add-hook 'clojurec-mode-hook #'lsp)
;;   (add-hook 'clojurescript-mode-hook #'lsp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keyboard

(require 'evil)
(evil-mode t)
(global-evil-visualstar-mode)

(require 'evil-leader)
(global-evil-leader-mode)

(evil-leader/set-leader ",")
(evil-leader/set-key
  "e" 'eval-region
  "d" 'eval-defun)

(mapc (lambda (state)
        (evil-global-set-key state (kbd "SPC SPC") 'helm-M-x)
        (evil-global-set-key state (kbd "SPC b b") 'helm-mini)
        (evil-global-set-key state (kbd "SPC b d") 'kill-this-buffer)
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
        (evil-global-set-key state (kbd "SPC o c") 'find-compose)
        (evil-global-set-key state (kbd "SPC o z") 'zprint)
        (evil-global-set-key state (kbd "SPC t o") 'iterm-open)
        (evil-global-set-key state (kbd "SPC t t") 'iterm-open-new-tab)
        (evil-global-set-key state (kbd "SPC t r") 'iterm-repeat-last-command)
        (evil-global-set-key state (kbd "SPC j i") 'helm-imenu)
        (evil-global-set-key state (kbd "SPC j I") 'helm-imenu-in-all-buffers)
        (evil-global-set-key state (kbd "SPC j j") 'my-jump-to-definition)
        (evil-global-set-key state (kbd "SPC j t") 'dumb-jump-go-prompt)
        (evil-global-set-key state (kbd "SPC r l") 'helm-resume)
        (evil-global-set-key state (kbd "SPC r y") 'helm-show-kill-ring)
        (evil-global-set-key state (kbd "SPC p f") 'helm-projectile)
        (evil-global-set-key state (kbd "SPC f e d") 'find-init-el)
        (evil-global-set-key state (kbd "SPC m c") 'cider-force-connect)
        (evil-global-set-key state (kbd "SPC m k") 'cider-force-eval-buffer)
        (evil-global-set-key state (kbd "SPC s c") 'evil-ex-nohighlight)
        (evil-global-set-key state (kbd "SPC i e") 'em-dash)

        (evil-global-set-key state (kbd "SPC k t") 'sp-transpose-sexp)
        (evil-global-set-key state (kbd "SPC k s") 'sp-forward-slurp-sexp)
        (evil-global-set-key state (kbd "SPC k r") 'raise-sexp)
        (evil-global-set-key state (kbd "SPC k w") 'insert-parentheses)
        (evil-global-set-key state (kbd "SPC k W") 'sp-unwrap-sexp)
        (evil-global-set-key state (kbd "SPC o b") 'sp-splice-sexp-killing-backward)
        (evil-global-set-key state (kbd "SPC o d") 'sp-splice-sexp-killing-forward)

        (evil-global-set-key state (kbd "SPC g l l") 'git-link)
        (evil-global-set-key state (kbd "SPC g t m") 'git-timemachine)
        (evil-global-set-key state (kbd "SPC g ]") 'diff-hl-next-hunk)
        (evil-global-set-key state (kbd "SPC g [") 'diff-hl-prev-hunk)
        (evil-global-set-key state (kbd "SPC c l") 'comment-line))
      '(normal visual))

(mapc (lambda (state)
        (evil-global-set-key state (kbd "SPC v") 'er/expand-region))
      '(normal))

(evil-define-minor-mode-key 'motion 'visual-line-mode "j" 'evil-next-visual-line)
(evil-define-minor-mode-key 'motion 'visual-line-mode (kbd "<down>") 'evil-next-visual-line)
(evil-define-minor-mode-key 'motion 'visual-line-mode "k" 'evil-previous-visual-line)
(evil-define-minor-mode-key 'motion 'visual-line-mode (kbd "<up>") 'evil-previous-visual-line)
(evil-define-minor-mode-key 'motion 'visual-line-mode "0" 'evil-beginning-of-visual-line)
(evil-define-minor-mode-key 'motion 'visual-line-mode "$" 'evil-end-of-visual-line)

(provide 'init)
;;; init.el ends here
