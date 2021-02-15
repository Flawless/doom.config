;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Alexander Ushanov"
      user-mail-address "alushanov92@gmail.com")

(setq-default uniquify-buffer-name-style 'forward              ; Uniquify buffer names
              window-combination-resize t                      ; take new window space from all other windows (not just current)
              x-stretch-cursor t)                              ; Stretch cursor to the glyph width

(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t                         ; Nobody likes to loose work, I certainly don't
      inhibit-compacting-font-caches t)           ; When there are lots of glyphs, keep them in memory

(delete-selection-mode 1)                         ; Replace selection when inserting text


;; (global-subword-mode 1)                           ; Iterate through CamelCase words
;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-gruvbox)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
(setq org-agenda-files (directory-files-recursively "~/org/" "\\.org$"))
(setq org-agenda-start-on-weekday 1)
(setq calendar-week-start-day 1)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(setq-default fill-column 100)

(after! eshell-mode
  (eshell/addpath "home/flawless/tools/arcanist/bin"))

(defun clojure-styles ()
  (put-clojure-indent 're-frame.core/reg-event-fx 1)
  (put-clojure-indent 're-frame.core/reg-fx 1)
  (put-clojure-indent 'rf/reg-event-fx 1)
  (put-clojure-indent 'rf/reg-fx 1)
  (put-clojure-indent 're-frame.core/reg-event-db 1)
  (put-clojure-indent 're-frame.core/reg-db 1)
  (put-clojure-indent 'rf/reg-event-db 1)
  (put-clojure-indent 'rf/reg-db 1)
  (put-clojure-indent 're-frame.core/reg-sub 1)
  (put-clojure-indent 'rf/reg-sub 1)
  (put-clojure-indent 'component-style-def 1)
  (put-clojure-indent 'reg-view 1)
  (put-clojure-indent 'reg-modal 1)

  (put-clojure-indent 'attempt-all 1)
  (put-clojure-indent 'try-all 1))

(add-hook! 'clojure-mode-hook (clojure-styles))
(add-hook! 'clojurescript-mode-hook (clojure-styles))

(add-hook! 'clojure-mode-hook (paredit-mode))
(add-hook! 'clojurescript-mode-hook (paredit-mode))

(use-package! beancount
  :defer t
  :mode
  ("\\.bean\\(?:count\\)?\\'" . beancount-mode)
  :config
  (setq beancount-accounts-files
        (directory-files "~/finances/"
                         'full
                         (rx ".bean" eos))))

(add-hook! beancount-mode
  (outline-minor-mode))

(map! :after outline-minor-mode
      :n "g n" #'outline-next-heading
      :n "g p" #'outline-previous-heading
      :n "z m" #'outline-hide-leaves
      :n "z r" #'outline-show-leaves)

(after! dap-python
  (setq dap-auto-show-output nil)

  (setq dap-auto-configure-features '(locals))

  (setq dap-ui-buffer-configurations
        `((,"*dap-ui-locals*"  . ((side . right) (slot . 1) (window-width . 0.50))) ;; changed this to 0.50
          (,"*dap-ui-expressions*" . ((side . right) (slot . 2) (window-width . 0.20)))
          (,"*dap-ui-sessions*" . ((side . right) (slot . 3) (window-width . 0.20)))
          (,"*dap-ui-breakpoints*" . ((side . left) (slot . 2) (window-width . , 0.20)))
          (,"*debug-window*" . ((side . bottom) (slot . 3) (window-width . 0.20)))))


  (defun my/window-visible (b-name)
    "Return whether B-NAME is visible."
    (-> (-compose 'buffer-name 'window-buffer)
        (-map (window-list))
        (-contains? b-name)))

  (defun my/show-debug-windows (session)
    "Show debug windows."
    (let ((lsp--cur-workspace (dap--debug-session-workspace session)))
      (save-excursion
        (unless (my/window-visible dap-ui--locals-buffer)
          (dap-ui-locals)))))

  (add-hook 'dap-stopped-hook 'my/show-debug-windows)

  (defun my/hide-debug-windows (session)
    "Hide debug windows when all debug sessions are dead."
    (unless (-filter 'dap--session-running (dap--get-sessions))
      (and (get-buffer dap-ui--locals-buffer)
           (kill-buffer dap-ui--locals-buffer))))

  (add-hook 'dap-terminated-hook 'my/hide-debug-windows)

  )

(after! dap-python
  (dap-register-debug-template "dap-debug-script"
                               (list :type "python"
                                     :args "-i"
                                     :cwd (lsp-workspace-root)
                                     ;; :justMyCode :json-false
                                     ;; :debugOptions ["DebugStdLib" "ShowReturnValue" "RedirectOutput"]
                                     :program nil ; (expand-file-name "~/git/blabla")
                                     :request "launch"
                                     :name "dap-debug-script"))

  (dap-register-debug-template "dap-debug-test"
                               (list :type "python"
                                     :cwd (lsp-workspace-root)
                                     :module "pytest"
                                     :request "launch"
                                     :name "dap-debug-test-file"))

  (dap-register-debug-template "dap-debug-bokeh"
                               (list :type "python"
                                     :args "--show crewrelief --log-level info"
                                     :cwd (expand-file-name "~/git/crewrelief/src")
                                     :program "serve"
                                     :module "bokeh"
                                     :request "launch"
                                     :name "dap-debug-bokeh"))


  )

(after! dap-python
  (defun dap-python-script ()
    (interactive
     (dap-debug
      (list :type "python"
            :args "-i"
            :cwd (lsp-workspace-root)
            :program nil
            :request "launch"
            :name "dap-debug-script")))))

(after! dap-python
  (require 'python-pytest)

  (defun dap-python-test-method-at-point ()
    (interactive
       (dap-debug
        (list :type "python"
              :args ""
              :cwd (lsp-workspace-root)
              :program (concat (buffer-file-name) ":" ":" (python-pytest--current-defun))
              :module "pytest"
              :request "launch"
              :name "dap-debug-test-function")))))

(defadvice! +dap-python-poetry-executable-find-a (orig-fn &rest args)
  "Use the Python binary from the current virtual environment."
  :around #'dap-python--pyenv-executable-find
  (if (getenv "VIRTUAL_ENV")
      (executable-find (car args))
    (apply orig-fn args)))
;; (after! dap-python
;;   (defun dap-python--pyenv-executable-find (command)
;;     (concat (getenv "VIRTUAL_ENV") "/bin/python")))

(map! :localleader
        :map +dap-running-session-mode-map
      "d" nil)

;; (map! :after dap-mode
;;     :map dap-mode-map
;;     :localleader "d" nil)

;; (map! :after dap-mode
;;     :map python-mode-map
;;     :localleader
;;     ;; "d" nil
;;     (:desc "debug" :prefix "d"
;;       :desc "Hydra" :n "h" #'dap-hydra
;;       :desc "Run debug configuration" :n "d" #'dap-debug
;;       :desc "dap-ui REPL" :n "r" #'dap-ui-repl
;;       :desc "Debug test function" :n "t" #'dap-python-test-method-at-point
;;       :desc "Run last debug configuration" :n "l" #'dap-debug-last
;;       :desc "Toggle breakpoint" :n "b" #'dap-breakpoint-toggle
;;       :desc "dap continue" :n "c" #'dap-continue
;;       :desc "dap next" :n "n" #'dap-next
;;       :desc "Debug script" :n "s" #'dap-python-script
;;       :desc "dap step in" :n "i" #'dap-step-in
;;       :desc "dap eval at point" :n "e" #'dap-eval-thing-at-point
;;       :desc "Disconnect" :n "q" #'dap-disconnect ))
(use-package lsp-mode
  :config
  (lsp-register-custom-settings
   '(("pyls.plugins.pyls_mypy.enabled" t t)
     ("pyls.plugins.pyls_mypy.live_mode" nil t)
     ("pyls.plugins.pyls_black.enabled" t t)
     ("pyls.plugins.pyls_isort.enabled" t t)))
  :hook
  ((python-mode . lsp)))

(after! cider-mode
  (setq cider-auto-test-mode t)
  (setq cider-save-file-on-load t))

;; (use-package! magit-arcanist
;;   :config
;;   (progn
;;     (setq magit-arcanist-key (kbd "^"))
;;     (magit-arcanist-enable)))

(use-package! flycheck-clj-kondo
  :after (clojure-mode clojurescript-mode))

(defun make-eww-link (url)
  (eww-browse-url url))

(after! org
  (org-add-link-type "eww" #'make-eww-link)
  (setq org-superstar-headline-bullets-list '("⁖" "◉" "○" "✸" "✿")
        org-todo-keywords '((sequence "WAIT" "TODO" "STRT" "CTRL" "HOLD" "|" "DONE" "KILL"))))

(setq org-journal-file-type 'weekly
      org-journal-date-prefix "* "
      org-journal-date-format "%a, %Y-%m-%d.org"
      org-journal-file-format "%V.org")
