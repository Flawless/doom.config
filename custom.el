(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(safe-local-variable-values
   '((eval progn
           (defun fix-cider-lein-profile
               (orig-fun &rest args)
             (let
                 ((repl
                   (ido-completing-read "Select ClojureScript REPL:"
                                        '("dev" "debug" "test"))))
               (setq cider-figwheel-main-default-options
                     (format ":%s" repl))
               (setq cider-lein-parameters
                     (format "with-profile +%s repl" repl))
               (let
                   ((res
                     (apply orig-fun args)))
                 res)))
           (advice-add 'cider-jack-in-cljs :around #'fix-cider-lein-profile)
           (advice-add 'cider-jack-in-clj&cljs :around #'fix-cider-lein-profile))
     (eval cl-flet
           ((fix-cider-lein-profile
             (orig-fun &rest args)
             (let
                 ((repl
                   (ido-completing-read "Select ClojureScript REPL:"
                                        '("dev" "debug" "test"))))
               (setq cider-figwheel-main-default-options
                     (format ":%s" repl))
               (setq cider-lein-parameters
                     (format "with-profile +%s repl" repl))
               (let
                   ((res
                     (apply orig-fun args)))
                 res))))
           (advice-add 'cider-jack-in-cljs :around #'fix-cider-lein-profile)
           (advice-add 'cider-jack-in-clj&cljs :around #'fix-cider-lein-profile))
     (eval cl-labels
           ((fix-cider-lein-profile
             (orig-fun &rest args)
             (let
                 ((repl
                   (ido-completing-read "Select ClojureScript REPL:"
                                        '("dev" "debug" "test"))))
               (setq cider-figwheel-main-default-options
                     (format ":%s" repl))
               (setq cider-lein-parameters
                     (format "with-profile +%s repl" repl))
               (let
                   ((res
                     (apply orig-fun args)))
                 res))))
           (advice-add 'cider-jack-in-cljs :around #'fix-cider-lein-profile)
           (advice-add 'cider-jack-in-clj&cljs :around #'fix-cider-lein-profile))
     (eval cl-flet
           ((fix-cider-lein-profile
             (orig-fun &rest args)
             ((let
                  ((repl
                    (ido-completing-read "Select ClojureScript REPL:"
                                         '("dev" "debug" "test"))))
                (setq cider-figwheel-main-default-options
                      (format ":%s" repl))
                (setq cider-lein-parameters
                      (format "with-profile +%s repl" repl))
                (let
                    ((res
                      (apply orig-fun args)))
                  res)))))
           (advice-add 'cider-jack-in-cljs :around #'fix-cider-lein-profile)
           (advice-add 'cider-jack-in-clj&cljs :around #'fix-cider-lein-profile))
     (eval let
           ((fix-cider-lein-profile
             (lambda
               (orig-fun &rest args)
               (let
                   ((repl
                     (ido-completing-read "Select ClojureScript REPL:"
                                          '("dev" "debug" "test"))))
                 (setq cider-figwheel-main-default-options
                       (format ":%s" repl))
                 (setq cider-lein-parameters
                       (format "with-profile +%s repl" repl))
                 (let
                     ((res
                       (apply orig-fun args)))
                   res)))))
           (advice-add 'cider-jack-in-cljs :around #'fix-cider-lein-profile)
           (advice-add 'cider-jack-in-clj&cljs :around #'fix-cider-lein-profile))
     (cider-clojure-cli-global-options . "-A:env/dev")
     (cider-clojurescript-cli-global-options . "-A:dev")
     (clojurec-mode
      (cider-clojure-cli-global-options . "-A:fig"))
     (cider-default-cljs-repl . shadow)
     (clojurescript-mode
      (cider-clojure-cli-global-options . "-A:fig"))
     (cider-shadow-cljs-default-options . "app"))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(defun my-reload-dir-locals-for-current-buffer ()
  "reload dir locals for the current buffer"
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

(defun my-reload-dir-locals-for-all-buffer-in-this-directory ()
  "For every buffer with the same `default-directory` as the
current buffer's, reload dir-locals."
  (interactive)
  (let ((dir default-directory))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (equal default-directory dir))
        (my-reload-dir-locals-for-current-buffer)))))

(add-hook 'emacs-lisp-mode-hook
          (defun enable-autoreload-for-dir-locals ()
            (when (and (buffer-file-name)
                       (equal dir-locals-file
                              (file-name-nondirectory (buffer-file-name))))
              (add-hook (make-variable-buffer-local 'after-save-hook)
                        'my-reload-dir-locals-for-all-buffer-in-this-directory))))
(put 'scroll-left 'disabled nil)
(put 'magit-clean 'disabled nil)
