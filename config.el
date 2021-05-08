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
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t)) ;; Prevent flickering issues

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
(setq org-books-file (concat org-directory "books.org"))
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

(defun clojure-init ()
  (clojure-styles)
  (rainbow-delimiters-mode)
  (rainbow-identifiers-mode)
  (paredit-mode)
  (centered-cursor-mode))

(add-hook! 'clojure-mode-hook (clojure-init))
(add-hook! 'clojurescript-mode-hook (clojure-init))

(setq gc-cons-threshold (* 1024 1024 1024)
      read-process-output-max (* 50 1024 1024)
      lsp-file-watch-threshold 10000
      lsp-idle-delay .500
      company-minimum-prefix-length 1
      lsp-log-io nil
      lsp-lens-enable nil
      lsp-ui-sideline-enable t
      lsp-ui-sideline-show-hover nil
      lsp-eldoc-enable-hover nil
      lsp-signature-auto-activate nil
      lsp-enable-indentation nil)

(use-package! beancount
  :defer t
  :mode
  ("\\.bean\\(?:count\\)?\\'" . beancount-mode)
  :config
  (setq beancount-accounts-files
        (directory-files "~/finances/"
                         'full
                         (rx ".bean" eos))))

(add-hook! beancount```


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

(setq magit-save-repository-buffers t)

;; (use-package! magit-arcanist
;;   :config
;;   (progn
;;     (setq magit-arcanist-key (kbd "^"))
;;     (magit-arcanist-enable)))

(use-package! flycheck-clj-kondo
  :after (clojure-mode clojurescript-mode))

(after! org
  (auto-fill-mode)
  (setq org-superstar-headline-bullets-list '("⁖" "◉" "○" "✸" "✿")
        org-todo-keywords '((sequence "TODO(t)" "WAIT(w)" "STRT(s)" "CTRL(c)" "HOLD(h)" "|" "DONE(d)" "KILL(k)"))
        org-tag-alist '(("important" . ?i)
                        ("urgent"    . ?u)
                        ("arvl"      . ?a))
        org-agenda-custom-commands '(("1" "Q1" tags-todo "+important+urgent")
                                     ("2" "Q2" tags-todo "+important-urgent")
                                     ("3" "Q3" tags-todo "-important+urgent")
                                     ("4" "Q4" tags-todo "-important-urgent")))
  (setq org-capture-templates
        '(("b" "Book" entry (file org-books-file)
           "* %^{TITLE}\n:PROPERTIES:\n:ADDED: %<[%Y-%02m-%02d]>\n:END:%^{AUTHOR}p\n%?" :empty-lines 1)
          ("t" "Todo" entry (file+headline "~/org/gtd.org" "Tasks")
           "* TODO %?\n %i\n %a")))

  (setq org-journal-file-type 'weekly
        org-journal-date-prefix "* "
        org-journal-date-format "%a, %Y-%m-%d"
        org-journal-file-format "%V.org")

  (defun my-org-clocktable-formatter (ipos tables params)
    "Custom formatter for org-mode clocktables which groups by category rather than file.
It uses `org-clock-clocktable-formatter' for the insertion of the
table after sorting the items into tables based on an items
category property. Thus all parameters supported by
`org-clock-clocktable-formatter' are supported. To use this to
sort a clocktable add `:properties (\"CATEGORY\") :formatter
my-org-clocktable-formatter' to that clocktable's arguments."
    (let* ((tt (-flatten-n 1 (-map #'-last-item tables)))
           (formatter (or org-clock-clocktable-formatter
                          'org-clocktable-write-default))
           (newprops (remove "CATEGORY" (plist-get params :properties)))
           (newparams (plist-put (plist-put params :multifile t) :properties newprops))
           newtables)

      ;; Compute net clocked time for each item
      (setq tt
            (--map-indexed
             (let* ((it-level (car it))
                    (it-time (nth 4 it))
                    (it-subtree (--take-while (< it-level (car it))
                                              (-drop (1+ it-index) tt)))
                    (it-children (--filter (= (1+ it-level) (car it))
                                           it-subtree)))
               (-replace-at 4 (- it-time (-sum (--map (nth 4 it) it-children)))
                            it))
             tt))

      ;; Add index (ie id) and indexes of parents (these are needed in the
      ;; sorting step). This can probably be written more functionally using --reduce?
      ;; At least without having to modify hist.
      (setq tt
            (let (hist)
              (--map-indexed (let* ((it-level (car it))
                                    (it-hist (-drop (- (length hist)
                                                       it-level -1)
                                                    hist)))
                               (setq hist (cons it-index it-hist))
                               (cons it-index (cons it-hist it)))
                             tt)))

      ;; Now comes the important phase: sorting, where we copy items with >0 net time
      ;; into newtables based on their category, and we copy their parents when
      ;; appropriate.
      (--each tt (let* ((it-hist (nth 1 it))
                        (it-time (nth 6 it))
                        (it-prop (-last-item it))
                        (it-cat (alist-get "CATEGORY" it-prop nil nil #'string=))
                        ;; Find the index of the table for category: it-cat or if
                        ;; it doesn't yet exist add it to the start of newtables.
                        (cat-pos (or
                                  (--find-index (string= (car it) it-cat) newtables)
                                  (progn (push (list it-cat nil) newtables) 0)))
                        (cat-members (-map #'car (-last-item (nth cat-pos newtables))))
                        (it-parent
                         (or (--find-index (member it
                                                   cat-members)
                                           it-hist)
                             (length it-hist)))
                        (hist-to-add
                         ;; replace the time of copied parents with 0 since if a
                         ;; parents is being copied and has time >0 then it has
                         ;; already been placed in the table for a different
                         ;; category. ie. We don't want time double counted.
                         (--map (-replace-at 6 0 (nth it tt))
                                (-take it-parent it-hist))))

                   (when (not (= 0 it-time))
                     (setf (-last-item (nth cat-pos newtables))
                           (append (cons it hist-to-add)
                                   (-last-item (nth cat-pos newtables)))))))

      (--each newtables (setf (-last-item it) (reverse (-last-item it))))
      ;; Cleanup, remove ids and list of parents, as they are no longer needed.
      (setq newtables
            (--map (list (car it) 0 (--map (-drop 2 it) (-last-item it))) newtables))

      ;; Recompute the total times for each node.
      ;; (replace this with --each and setf?)
      (setq newtables
            (--map (let* ((it-children (sum-direct-children-org 1 (-last-item it)))
                          (it-total-time (-sum
                                          (--map (nth 4 it)
                                                 (--filter (= 1 (car it))
                                                           it-children)))))
                     (list (car it) it-total-time it-children))
                   newtables))
      ;; Actually insert the clocktable now.
      (funcall formatter ipos newtables newparams)
      ;; Replace "File" with "Category" in the "file" column and "*File time*" with "*
      ;; Category time*" in the table.
      (org-table-goto-line 1)
      (org-table-blank-field)
      (insert "Category")
      (org-table-align)
      (let ((n 2))
        (while (org-table-goto-line n)
          (org-table-next-field)
          ;; This won't work if there are addition columns eg. Property column.
          ;; Instead look forward along each line to see if that regexp is matched?
          (when (looking-at "\\*File time\\* .*\| *\\*.*[0-9]:[0-9][0-9]\\*")
            (org-table-blank-field)
            (insert "*Category time*")
            (org-table-align))
          (incf n))))))

(defun sum-direct-children-org (level children)
  "Update the time LEVEL nodes recursively to be the sum of the times of its children.
Used in `my-org-clocktable-formatter' to go from net times back to tatal times."
  (let ((subtrees (-partition-before-pred (lambda (it) (= level (car it))) children)))
    (-flatten-n 1
                (--map (let ((it-children (sum-direct-children-org (1+ level)
                                                                   (cdr it))))
                         (cons (--update-at
                                4 (+ it
                                     (-sum
                                      (--map (nth 4 it)
                                             (--filter (= (1+ level)
                                                          (car it))
                                                       it-children))))
                                (car it))
                               it-children))
                       subtrees))))
