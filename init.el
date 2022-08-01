;;; init.el --- Initialization file for Emacs
;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs
;;;
;;; No Spacesmacs but we imitate its keybindings just enough
;;; to keep our muscle memory alive.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; preliminaries

(setq gc-cons-threshold 200000000)
(setq read-process-output-max (* 1024 1024))
(setq create-lockfiles nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; package configuration

(require 'package)

(let ((package-list '(evil
                      evil-leader
                      evil-visualstar
                      eglot
                      diff-hl
                      solarized-theme
                      package-lint
                      prettier-js
                      typescript-mode
                      which-key
                      aggressive-indent
                      clojure-mode
                      prettier-js
                      typescript-mode
                      yasnippet
                      git-link
                      cider
                      yaml-mode
                      markdown-mode
                      exec-path-from-shell
                      git-timemachine
                      js2-mode
                      ws-butler
                      helm
                      helm-projectile
                      helm-ag
                      smart-mode-line
                      mode-line-bell
                      expand-region
                      projectile
                      visual-fill-column
                      simpleclip
                      smooth-scrolling
                      ;; parenthesis management
                      smartparens
                      evil-smartparens)))
  (setq package-check-signature nil) ;; FIXME: do we need this?

  (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                           ("melpa" . "https://melpa.org/packages/")))

  ;; Activate all the packages (in particular autoloads)
  (package-initialize

   )
  (unless package-archive-contents
    (package-refresh-contents))
  (dolist (package package-list)
    (unless (package-installed-p package)
      (package-install package))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load extra files

(when (memq window-system '(mac ns x))
  (setenv "RIPGREP_CONFIG_PATH"
          (expand-file-name "~/.rgconfig"))
  (exec-path-from-shell-initialize))

(add-to-list 'load-path (expand-file-name "~/emacs.d/vendor"))
(add-to-list 'load-path (expand-file-name "~/emacs.d/elisp"))
(add-to-list 'load-path (expand-file-name "~/prg/zprint-mode.el"))
(load-file (expand-file-name "~/emacs.d/elisp/my-functions.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; zprint-mode

(require 'zprint-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(eglot yasnippet csv-mode sqlformat bm company-mode which-key helm-lsp lsp-ui highlight-indentation-mode yaml-mode company tide prettier-js typescript-mode package-lint helm-unicode helm-chrome-control git-timemachine git-link diff-hl evil-visualstar js2-mode deadgrep smart-mode-line flycheck-jokeryy flycheck-joker cider aggressive-indent lsp-mode mode-line-bell helm-projectile markdown-mode helm-ag evil-lisp-state ws-butler evil-smartparens use-package smartparens evil-leader evil))
 '(safe-local-variable-values
   '((eval when
           (and
            (buffer-file-name)
            (not
             (file-directory-p
              (buffer-file-name)))
            (string-match-p "^[^.]"
                            (buffer-file-name)))
           (unless
               (featurep 'package-build)
             (let
                 ((load-path
                   (cons "../package-build" load-path)))
               (require 'package-build)))
           (unless
               (derived-mode-p 'emacs-lisp-mode)
             (emacs-lisp-mode))
           (package-build-minor-mode)
           (setq-local flycheck-checkers nil)
           (set
            (make-local-variable 'package-build-working-dir)
            (expand-file-name "../working/"))
           (set
            (make-local-variable 'package-build-archive-dir)
            (expand-file-name "../packages/"))
           (set
            (make-local-variable 'package-build-recipes-dir)
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
           (this-as 0)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq helm-mode-fuzzy-match t)

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
(setq which-key-idle-delay 3.0)
(which-key-mode)
(setq default-frame-alist '((font . "Menlo-15")))
;; maximize vertically
(add-to-list 'initial-frame-alist '(fullscreen . fullheight))
;; modeline
(sml/setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; behavior

(save-place-mode 1)

;; git

(global-diff-hl-mode)
(diff-hl-flydiff-mode)
(require 'git-link)
(setq git-link-open-in-browser t)

(with-eval-after-load 'git-timemachine
  (evil-make-overriding-map git-timemachine-mode-map 'normal)
  ;; force update evil keymaps after git-timemachine-mode loaded
  (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps))

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
      '(clojure-mode-hook))

(mapc (lambda (hook))
      '(typescript-mode-hook))

;; see https://github.com/clojure-emacs/clojure-mode/issues/516
(add-hook 'clojure-mode-hook
          (lambda ()
            (setq comment-indent-function 'comment-indent-default)
            (setq comment-add 0)
            (comment-normalize-vars)
            (setq-local comment-column 0)))

(require 'clojure-comment-dwim)

;; lisp


(require 'smartparens-config)

(dolist (hook '(clojure-mode-hook emacs-lisp-mode-hook))
  (add-hook hook 'turn-on-smartparens-strict-mode))
(add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)

(require 'cider)
(setq cider-repl-pop-to-buffer-on-connect nil)

;; lispy word characters

;; C-q: insert literal character
;; C-q C-j -> newline
;; delete a quote character without regard for balancing: C-u 0 BACKSPACE
;; C-h k <KEY> -> help for key or key sequence (describe-key)
;; C-x C-h -> show all prefix commands that with C-x; works with any prefix commmand
;; C-c -> prefix command for user-defined keybindings; package authors shouldn't use it


(defun clojure-word-chars ()
  "Add more special characters " "for Clojure."
  (modify-syntax-entry ?+ "w")
  (modify-syntax-entry ?- "w")
  (modify-syntax-entry ?< "w")
  (modify-syntax-entry ?> "w"))

(add-hook 'clojure-mode-hook 'clojure-word-chars)
(add-hook 'emacs-lisp-mode-hook #'(lambda () (modify-syntax-entry ?- "w")))

;; javascript

(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.mjs\\'" . js2-mode))
(setq-default js2-basic-offset 2)
(setq-default js-indent-level 2)
(setq-default typescript-indent-level 2)

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)
(setq-default typescript-indent-level 2)

(add-hook 'typescript-mode-hook #'prettier-js-mode)


;; text

(add-hook 'visual-line-mode-hook 'visual-fill-column-mode)
(add-hook 'visual-line-mode-hook (lambda () (setq fill-column 90)))
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;;;; yaml

(require 'highlight-indentation)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keyboard

(require 'evil)
(evil-mode t)
(global-evil-visualstar-mode)

(require 'evil-leader)
(global-evil-leader-mode)

;; Use MacOS key-bindings (Option-hyhpen, Option-Shift-hyphen) for en-dash and em-dash
;; (define-key undo-tree-map (kbd "M-_") nil) ;; remove override
(global-set-key (kbd "M-_") (lambda () (interactive) (insert "—")))
(global-set-key (kbd "M--") (lambda () (interactive) (insert "–")))
(global-set-key (kbd "M-8") (lambda () (interactive) (insert "•")))
(global-set-key (kbd "M-*") (lambda () (interactive) (insert "°")))

(evil-leader/set-leader ",")
(evil-leader/set-key
  "e" 'eval-region
  "d" 'eval-defun)

(mapc (lambda (state)
        (evil-global-set-key state (kbd "s-=") (lambda () (interactive) (text-scale-increase 1)))
        (evil-global-set-key state (kbd "s-+") (lambda () (interactive) (text-scale-increase 1)))
        (evil-global-set-key state (kbd "s--") (lambda () (interactive) (text-scale-increase -1)))
        (evil-global-set-key state (kbd "SPC SPC") 'helm-M-x)
        (evil-global-set-key state (kbd "SPC b b") 'helm-mini)
        (evil-global-set-key state (kbd "SPC b d") 'kill-this-buffer)
        (evil-global-set-key state (kbd "SPC b n") 'bm-next)
        (evil-global-set-key state (kbd "SPC b t") 'bm-toggle)
        (evil-global-set-key state (kbd "SPC w m") 'delete-other-windows)
        (evil-global-set-key state (kbd "SPC f f") 'helm-find-files)
        (evil-global-set-key state (kbd "SPC /") 'helm-do-ag-project-root)
        (evil-global-set-key state (kbd "SPC ?") 'helm-do-ag-buffers)
        (evil-global-set-key state "U" 'backward-up-list)
        (evil-global-set-key state "R" 'down-list)
        (evil-global-set-key state "L" 'sp-forward-sexp)
        (evil-global-set-key state "H" 'sp-backward-sexp)
        (evil-global-set-key state ";" 'mark-sexp)
        (evil-global-set-key state (kbd "SPC o f") 'fill-paragraph)
        (evil-global-set-key state (kbd "SPC o o") 'find-primary-proj)
        (evil-global-set-key state (kbd "SPC o c") 'find-compose)
        (evil-global-set-key state (kbd "SPC o C") 'find-compose2)
        (evil-global-set-key state (kbd "SPC o 1") 'find-1x1)
        (evil-global-set-key state (kbd "SPC o r") 'find-reading)
        (evil-global-set-key state (kbd "SPC o w") 'find-writing)
        (evil-global-set-key state (kbd "SPC o s") 'find-scraps)
        (evil-global-set-key state (kbd "SPC o i") 'find-it)
        (evil-global-set-key state (kbd "SPC o d") 'find-diary)
        (evil-global-set-key state (kbd "SPC o z") 'zprint)
        (evil-global-set-key state (kbd "SPC t o") 'iterm-open)
        (evil-global-set-key state (kbd "SPC t t") 'iterm-open-new-tab)
        (evil-global-set-key state (kbd "SPC t r") 'iterm-repeat-last-command)

        (evil-global-set-key state (kbd "SPC j i") 'helm-imenu)
        (evil-global-set-key state (kbd "SPC j I") 'helm-imenu-in-all-buffers)
        (evil-global-set-key state (kbd "SPC j j") 'xref-find-definitions)
        (evil-global-set-key state (kbd "SPC j r") 'xref-find-references)
        (evil-global-set-key state (kbd "SPC j a") 'eglot-code-actions)
        (evil-global-set-key state (kbd "SPC j R") 'eglot-rename)
        (evil-global-set-key state (kbd "SPC j h") 'eldoc-doc-buffer)
        (evil-global-set-key state (kbd "SPC r l") 'helm-resume)
        (evil-global-set-key state (kbd "SPC r y") 'helm-show-kill-ring)
        (evil-global-set-key state (kbd "SPC p f") 'helm-projectile)
        (evil-global-set-key state (kbd "SPC f e d") 'find-init-el)
        (evil-global-set-key state (kbd "SPC f e m") 'find-my-functions)
        (evil-global-set-key state (kbd "SPC m c") 'cider-force-connect)
        (evil-global-set-key state (kbd "SPC m k") 'cider-force-eval-buffer)
        (evil-global-set-key state (kbd "SPC s c") 'evil-ex-nohighlight)
        (evil-global-set-key state (kbd "SPC i e") 'em-dash)

        (evil-global-set-key state (kbd "SPC s k") 'smerge-keep-current)

        (evil-global-set-key state (kbd "SPC m m") 'markdown-shifttab)

        (evil-global-set-key state (kbd "SPC s j") 'sp-split-sexp)
        (evil-global-set-key state (kbd "SPC k t") 'sp-transpose-sexp)
        (evil-global-set-key state (kbd "SPC k s") 'sp-forward-slurp-sexp)
        (evil-global-set-key state (kbd "SPC k b") 'sp-forward-barf-sexp)
        (evil-global-set-key state (kbd "SPC k r") 'raise-sexp)
        (evil-global-set-key state (kbd "SPC k w") 'insert-parentheses)
        (evil-global-set-key state (kbd "SPC k W") 'sp-unwrap-sexp)
        (evil-global-set-key state (kbd "SPC k c") 'sp-clone-sexp)
        (evil-global-set-key state (kbd "SPC o b") 'sp-splice-sexp-killing-backward)
        (evil-global-set-key state (kbd "SPC o t") 'touch)
        (evil-global-set-key state (kbd "SPC c c") 'clojure-comment-dwim)

        (evil-global-set-key state (kbd "SPC i b") 'insert-bar)

        (evil-global-set-key state (kbd "SPC t t") 'projectile-toggle-between-implementation-and-test)

        (evil-global-set-key state (kbd "SPC g l l") 'git-link)
        (evil-global-set-key state (kbd "SPC g t m") 'git-timemachine)
        (evil-global-set-key state (kbd "SPC g ]") 'diff-hl-next-hunk)
        (evil-global-set-key state (kbd "SPC g [") 'diff-hl-prev-hunk))
      '(normal visual))

(mapc (lambda (state)
        (evil-global-set-key state (kbd "SPC c l") 'comment-line)
        (evil-global-set-key state (kbd "SPC v") 'er/expand-region))
      '(normal))

(mapc (lambda (state)
        (evil-global-set-key state (kbd "SPC c l") 'comment-dwim)
        )
      '(visual))

(evil-define-minor-mode-key 'motion 'visual-line-mode "j" 'evil-next-visual-line)
(evil-define-minor-mode-key 'motion 'visual-line-mode (kbd "<down>") 'evil-next-visual-line)
(evil-define-minor-mode-key 'motion 'visual-line-mode "k" 'evil-previous-visual-line)
(evil-define-minor-mode-key 'motion 'visual-line-mode (kbd "<up>") 'evil-previous-visual-line)
(evil-define-minor-mode-key 'motion 'visual-line-mode "0" 'evil-beginning-of-visual-line)
(evil-define-minor-mode-key 'motion 'visual-line-mode "$" 'evil-end-of-visual-line)

;; tgt-toggle

(setq tgt-open-in-new-window 'nil)

(add-to-list 'tgt-projects '((:root-dir "~/pitch/shadow-jest")
                             (:src-dirs "src/main")
                             (:test-dirs "src/test")
                             (:test-suffixes "_spec")))

(require 'yasnippet)
(yas-global-mode 1)

(setq tab-always-indent 'complete)
(helm-mode 1)

;; eglot

(add-hook 'eglot-managed-mode-hook
	  ;; This displays full docs for clojure functions.
	  ;; See https://github.com/joaotavora/eglot/discussions/894
	  #'(lambda ()
	      (setq-local eldoc-documentation-strategy
			  #'eldoc-documentation-compose

			  eldoc-echo-area-use-multiline-p
			  5)))

(provide 'init)
;;; init.el ends here
