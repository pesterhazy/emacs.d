(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eglot-confirm-server-initiated-edits nil)
 '(eglot-connect-timeout 90)
 '(eglot-sync-connect 0)
 '(evil-undo-system 'undo-redo)
 '(helm-minibuffer-history-key "M-p")
 '(ignored-local-variable-values '((checkdoc-minor-mode . t)))
 '(package-selected-packages
   '(lazy-ruff corfu dotenv-mode prettier-js json-mode company evil-collection go-mode git-timemachine helm-projectile yaml-mode vundo flyspell-correct evil evil-leader evil-visualstar apheleia wgrep eglot jarchive python-mode projectile dockerfile-mode diff-hl csv-mode toggle-test solarized-theme package-lint which-key aggressive-indent clojure-mode yasnippet git-link cider markdown-mode exec-path-from-shell js2-mode ws-butler helm helm-ag smart-mode-line mode-line-bell expand-region visual-fill-column simpleclip smooth-scrolling smartparens evil-smartparens))
 '(recentf-max-saved-items 200)
 '(safe-local-variable-values
   '((projectile-project-type . go)
     (eval when
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
