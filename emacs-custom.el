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
   '(add-node-modules-path aggressive-indent apheleia cider clojure-mode
                           company corfu csv-mode diff-hl
                           dockerfile-mode dotenv-mode eglot evil
                           evil-collection evil-leader
                           evil-smartparens evil-visualstar
                           exec-path-from-shell expand-region
                           flyspell-correct git-link git-timemachine
                           go-mode helm helm-ag helm-projectile
                           jarchive js2-mode json-mode lazy-ruff
                           markdown-mode mode-line-bell package-lint
                           prettier-js projectile python-mode
                           simpleclip smart-mode-line smartparens
                           smooth-scrolling solarized-theme
                           toggle-test visual-fill-column vundo wgrep
                           which-key ws-butler yaml-mode yasnippet))
 '(recentf-max-saved-items 200)
 '(safe-local-variable-values
   '((projectile-project-type . go)
     (eval when
           (and (buffer-file-name)
                (not (file-directory-p (buffer-file-name)))
                (string-match-p "^[^.]" (buffer-file-name)))
           (unless (featurep 'package-build)
             (let ((load-path (cons "../package-build" load-path)))
               (require 'package-build)))
           (unless (derived-mode-p 'emacs-lisp-mode)
             (emacs-lisp-mode))
           (package-build-minor-mode)
           (setq-local flycheck-checkers nil)
           (set (make-local-variable 'package-build-working-dir)
                (expand-file-name "../working/"))
           (set (make-local-variable 'package-build-archive-dir)
                (expand-file-name "../packages/"))
           (set (make-local-variable 'package-build-recipes-dir)
                default-directory))
     (eval define-clojure-indent (reg-cofx :defn) (reg-event-db :defn)
           (reg-event-fx :defn) (reg-fx :defn) (reg-sub :defn)
           (reg-event-domain :defn) (reg-block-event-fx :defn)
           (reg-event-domain-fx :defn) (reg-event-persistent-db :defn)
           (this-as 0)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
