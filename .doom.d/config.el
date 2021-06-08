(setq user-full-name "Victor Rodriguez"
      user-mail-address "vrodriguez@confluent.io")
(setq ns-auto-hide-menu-bar t)

;; Makes sure that weekdays in the timestamps of org-mode files and the agenda appear in English
(setq system-time-locale "C")

(setq vic/org-dir "~/source/BrainForest/")

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)

(setenv "PATH" (concat
                "/usr/local/go/bin" ";"
                (getenv "PATH")
                )
        )
(setq org-latex-pdf-process
      '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(setq dired-listing-switches "--group-directories-first -al")

(use-package deft
  :init
  (setq deft-directory vic/org-dir)
  (setq deft-extensions '("org"))
  (setq deft-recursive t))

(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 19 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "JetBrainsMono Nerd Font" :size 17))
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
;; (custom-set-faces!
;;   '(font-lock-comment-face :slant italic)
;;   '(font-lock-keyword-face :slant italic))

(use-package all-the-icons)

(setq doom-theme 'doom-material)
(map! :leader
      :desc "Load new theme"
      "h t" #'counsel-load-theme)
;transparent adjustment
 (set-frame-parameter (selected-frame)'alpha '(95 . 95))
 (add-to-list 'default-frame-alist'(alpha . (95 . 95)))

(setq org-directory vic/org-dir)
(after! org
  ;; (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (setq org-hide-emphasis-markers t)
  (setq org-startup-folded 'content
        org-startup-with-inline-images t)
  (setq org-export-with-section-numbers nil)
  (setq org-ellipsis " ▼ "
        org-log-done 'time))
(setq display-line-numbers-type t)
(add-hook 'org-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'org-appear-mode)

(after! org
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
  ;; (setq org-superstar-remove-leading-stars t
    (setq org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●")))

(after! org
  (require 'org-download)
  (setq-default org-download-image-dir (concat org-directory "_attachments/"))
  (setq
        ;org-download-screenshot-method "screencapture -i %s"
        ;org-download-screenshot-method "scrot -s %s"
        ;org-download-screenshot-method "import %s"
        org-download-screenshot-method "flameshot gui --raw > %s"
        org-download-heading-lvl nil
        org-download-method 'directory)
  (org-download-enable))

(add-hook 'dired-mode-hook 'org-download-enable)

(after! org
  (setq
        org-journal-dir (format "%s/journal/%s" org-directory (format-time-string "%Y/%b") )
        ;; org-journal-date-format "%B %d, %Y (%A) "
        ;; org-journal-file-format "W%V_%Y-%m-%d.org"
        org-journal-file-format "%Y-%m-%d.org"
        ;; org-journal-created-property-timestamp-format "%Y%m%V%d"
        org-journal-file-header "#+title: Journal %B %d, %Y\n#+startup: folded\n#+category: Journal"))

(after! org
  ;;(custom-set-variables '(org-agenda-files (directory-files-recursively "~/vaults/org/agenda" "\\.org$")))
  (setq org-agenda-files `(,(concat org-directory "/agenda")))
  (setq org-agenda-prefix-format '(
        (agenda . " • %i %-12:c\t%?-12t% s")
        (todo . " • %i %-12:c\t")
        (tags . " %i %-12:c")
        (search . " %i %-12:c")))
  (setq org-agenda-custom-commands '(("z" "Day view"
                                      ((agenda "" ((org-agenda-span 'day)
                                                   (org-agenda-start-day "+0d")
                                                   (org-agenda-overriding-header "")
                                                   (org-super-agenda-groups
                                                    '((:name ""
                                                       :time-grid t
                                                       :date today
                                                       :order 1
                                                       :scheduled today
                                                       :todo "TODAY")
                                                      (:name "Overdue" :deadline past :scheduled past :order 2)))))
                                       (todo "" ((org-agenda-overriding-header "")
                                                 (org-super-agenda-groups
                                                  '((:name "Stuck projects"
                                                     :and (:todo "PROJ"
                                                           :not (:children ("NEXT" "READING"))))
                                                    (:name "Projects" :todo "PROJ")
                                                    (:name "with Subtasks"
                                                     :and (:todo "TODO" :children todo))
                                                    (:discard (:anything t))))))
                                       (alltodo "" ((org-agenda-overriding-header "")
                                                    (org-super-agenda-groups
                                                     '((:discard (:todo "RD"))
                                                       (:discard (:todo "TMPDROP"))
                                                       (:name "Next Items" :todo "NEXT" :order 3)
                                                       (:name "Important" :priority "A" :order 4)
                                                       (:name "Waiting and Blocked" :todo ("WAITING" "BLOCKED") :order 5)
                                                       (:name "OKRs" :category "OKRs" :order 6)
                                                       (:name "Books" :category "Books" :order 7)
                                                       (:name "To Read" :todo "READ" :order 8)
                                                       (:name "GOALS"
                                                        :and (:todo "GOAL"
                                                              :not (:category "OKRs"))
                                                        :order 9)
                                                       (:name "Done today"
                                                        :and (:regexp "State \"DONE\""
                                                              :log t)
                                                        :order 10)
                                                       (:discard (:habit))
                                                       ))))))
                                     ("n" "Weekly view"
                                      ((agenda "")
                                       (alltodo "" ((org-agenda-overriding-header "")
                                                    (org-super-agenda-groups
                                                     '((:discard (:todo "TMPDROP"))
                                                       (:name "Overdue" :deadline past :scheduled past)
                                                       (:name "Next Items" :todo "NEXT")
                                                       (:name "Important" :priority "A")
                                                       (:name "Waiting and Blocked" :todo ("WAITING" "BLOCKED"))
                                                       (:name "Projects" :todo "PROJ")
                                                       (:name "OKRs" :category "OKRs")
                                                       (:name "Books" :category "Books")
                                                       (:name "To Read" :todo "READ")
                                                       (:name "GOALS"
                                                        :and (:todo "GOAL"
                                                              :not (:category "OKRs")))
                                                       (:name "Done today"
                                                        :and (:regexp "State \"DONE\""
                                                              :log t))
                                                       (:discard (:habit))
                                                       ))))))
                                     ("r" "Main View"
                                      ((agenda "" ((org-agenda-span 'day)
                                                   (org-agenda-start-day "+0d")
                                                   (org-agenda-overriding-header "")
                                                   (org-super-agenda-groups
                                                    '((:name "Today"
                                                       :time-grid t
                                                       :date today
                                                       :order 1
                                                       :scheduled today
                                                       :todo "TODAY")))))
                                       (alltodo "" ((org-agenda-overriding-header "")
                                                    (org-super-agenda-groups
                                                     '(
                                                       (:discard (:habit))
                                                       (:todo "PROJ")
                                                       (:todo "NEXT")
                                                       (:todo "WAITING")
                                                       (:name "Important" :priority "A")
                                                       (:name "OKRs" :category "OKRs")
                                                       (:todo "GOAL")
                                                       (:todo "TODO")
                                                       (:discard (:todo "IDEA"))
                                                       ))))))))
  (org-super-agenda-mode))

(after! org-agenda
  (defvar vic/org-super-agenda-auto-show-groups
    '("OKRs" "Other items" "To Read" "Today"))

  (defun vic/org-super-agenda-origami-fold-default ()
    "Fold certain groups by default in Org Super Agenda buffer."
    (forward-line 3)
    (cl-loop do (origami-forward-toggle-node (current-buffer) (point))
             while (origami-forward-fold-same-level (current-buffer) (point)))
    (--each vic/org-super-agenda-auto-show-groups
      (goto-char (point-min))
      (when (re-search-forward (rx-to-string `(seq bol " " ,it)) nil t)
        (origami-close-node (current-buffer) (point)))))
  (map!
   :map org-super-agenda-header-map
   :g [tab] #'origami-toggle-node
   :g [backtab] #'origami-toggle-all-nodes)

  (add-hook 'org-agenda-mode-hook 'origami-mode))
  ;; (add-hook 'org-agenda-finalize-hook #'vic/org-super-agenda-origami-fold-default))
  ;; (use-package origami
  ;; :general (:keymaps 'org-super-agenda-header-map
  ;;                    "TAB" #'origami-toggle-node)
  ;; :config

  ;; (defvar vic/org-super-agenda-auto-fold-groups
  ;;   '("Other items" "To Read"))

  ;; (defun ap/org-super-agenda-origami-fold-default ()
  ;;   "Fold certain groups by default in Org Super Agenda buffer."
  ;;   (forward-line 3)
  ;;   (cl-loop do (origami-forward-toggle-node (current-buffer) (point))
  ;;            while (origami-forward-fold-same-level (current-buffer) (point)))
  ;;   (--each vic/org-super-agenda-auto-show-groups
  ;;     (goto-char (point-min))
  ;;     (when (re-search-forward (rx-to-string `(seq bol " " ,it)) nil t)
  ;;       (origami-close-node (current-buffer) (point)))))

  ;; :hook ((org-agenda-mode . origami-mode)
  ;;        (org-agenda-finalize . ap/org-super-agenda-origami-fold-default))))

(after! org
  (setq org-todo-keywords
        '((sequence "TODO(t)"
                    "PROJ(p)"
                    "WAITING(W@/!)"
                    "BLOCKED(b@/!)"
                    "NEXT(n!)"
                    "|"
                    "DONE(d)"
                    "CANCELLED(c@/!)"
                    "DELEGATED(D@/!)"
                    "PHONE"
                    "MEETING")
          (sequence "IDEA"
                    "GOAL"
                    "|"
                    "DROPPED(@)"
                    "COMPLETED")
          (sequence "READ"
                    "READING"
                    "TMPDROP"
                    "|"
                    "DROPPED(@/!)"
                    "FINISHED(!)"))))

(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
             ("WAITING" ("WAITING" . t))
             ("BLOCKED" ("WAITING") ("BLOCKED" . t))
             (done ("WAITING") ("BLOCKED"))
             ("TODO" ("WAITING") ("CANCELLED") ("BLOCKED"))
             ("NEXT" ("WAITING") ("CANCELLED") ("BLOCKED"))
             ("DONE" ("WAITING") ("CANCELLED") ("BLOCKED")))))

(defun vic/verify-refile-target ()
  "Exclude Done state tasks from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(after! org
  ; Targets includes this file and any agenda file up tp 9 levels deep
  (custom-set-variables '(org-refile-targets '((org-agenda-files . (:maxlevel . 4)))))
  ; Targets complete directly with IDO
  (setq org-outline-path-complete-in-steps t) ; Refile in a single go
  ; Use full outline paths for refile targets
  (setq org-refile-use-outline-path nil) ; Show full paths for refiling
  ; Allow refile to create parent task with confirmation
  (setq org-refile-allow-creating-parent-nodes (quote confirm))
  (setq org-refile-target-verify-function 'vic/verify-refile-target))

;; Save Org buffers after refiling!
(advice-add 'org-refile :after 'org-save-all-org-buffers)

(after! org
  ;; Save the running clock and all clock history when exiting Emacs, load it on startup
  (setq org-clock-persist t
        ;; Resume clock task on clock-in if the clock is open
        org-clock-in-resume t
        ;; Do not prommpt to resume an active clock, just resume it
        org-clock-persist-query-resume nil
        ;; Remove clocked tasks with 0:00 duration
        org-clock-out-remove-zero-time-clocks t
        ;;Clock out when moving a task to a done state
        org-clock-out-when-done t
        ;;Enable auto clock resolution for finding open clocks
        org-clock-auto-clock-resolution (quote when-no-clock-is-running)
        ;; Include open task in clock reports
        org-clock-report-include-clocking-task t
        ;; Use pretty things for the clocktable
        org-pretty-entities t))

(defun vic/get-okr-filename ()
  (concat (concat org-directory "agenda/OKRs/") (format-time-string "%Y.org")))

(defun vic/get-okr-quarter()
  "THis function dinamucally gets a OKR file name"
  (interactive)
  (format-time-string "Quarter %q"))

(defun vic/get-ppp-filename ()
  "This function dinamically gets a PPP file name"
  (concat (concat org-directory "agenda/PPPs/") (format-time-string "%Y/%b/W%V_%a-%d.org")))

(setq vic/inbox-path (concat org-directory "agenda/inbox.org"))

(after! org
       (setq org-log-into-drawer t)
        (setq org-capture-templates
              (doct `(("Todo" :keys "t"
                       :file vic/inbox-path
                       :template "* TODO %?\n%{time}:PROPERTIES:\n:CREATED: %U\n:Origin: %a\n:END:\n"
                       :clock-in t
                       :clock-resume t
                       :type entry
                       :children (("Normal" :keys "t" :time "")
                                  ("With deadline" :keys "d" :time "DEADLINE: %^t\n")
                                  ("Scheduled" :keys "s" :time "SCHEDULED: %^t\n")))
                      ("Respond" :keys "r"
                       :file vic/inbox-path
                       :template "* NEXT Respond %^{person} to %^{type}\nSCHEDULED: %t\n:PROPERTIES:\n:CREATED: %U\n:Origin: %a\n:END:\nRE: %?"
                       :type entry
                       :clock-in t
                       :clock-resume t)
                      ("OKR" :keys "o"
                       :file vic/get-okr-filename
                       :headline ,(vic/get-okr-quarter)
                       :template "* %? \n"
                       :clock-in t
                       :clock-resume t)
                      ("PPP" :keys "p"
                       :file vic/get-ppp-filename
                       :type plain
                       :template "%(format-time-string \"#+TITLE: PPP Week %V - %B, %d %Y\")\n\n* Victor PPPs\n- Progress\n- %?\n- Plans\n- Problems\n"
                       :clock-in t
                       :clock-resume t)
                      ("Note" :keys "n"
                       :file vic/inbox-path
                       :type entry
                       :template "* %^{title}\t:NOTE:\n:PROPERTIES:\n:CREATED: %U\n:Origin: %a\n:END:\n\n%?"
                       :clock-in t
                       :clock-resume t)
                      ("Feed BrainForest" :keys "f"
                       :file vic/inbox-path
                       :type entry
                       :template "* TODO Feed/extract [[%F][%f]] note about %^{topic} \t:BrainForest:\n:PROPERTIES:\n:CREATED: %U\n:Origin: %a\n:END:\n"
                       :immediate-finish t)
                      ("Meeting" :keys "m"
                       :file vic/inbox-path
                       :type entry
                       :template "* MEETING %^{title}\t:MEETING:\n:PROPERTIES:\n:CREATED: %U\n:Origin: %a\n:END:\n\n%?"
                       :clock-in t
                       :clock-resume t)
                      ("Phone call" :keys "c"
                       :file vic/inbox-path
                       :type entry
                       :template "* PHONE with %^{person}\t:PHONE:\n:PROPERTIES:\n:CREATED: %U\n:Origin: %a\n:END:\n\n%?"
                       :clock-in t
                       :clock-resume t)
                      ("Book" :keys "b"
                       :file "~/Dropbox/org/agenda/books.org"
                       :headline "To Read"
                       :type entry
                       :template "* READ %?\n:PROPERTIES:\n:CREATED: %U\n:Origin: %a\n:END:"
                       :clock-in t
                       :clock-resume t)))))

;; V2
(use-package! org-roam
  :after org
  :init

  (map! :leader
        :prefix ("r" . "roam")
        ;; :desc "insert" "i" #'org-roam-insert
        :desc "Show graph" "g" #'org-roam-graph
        :desc "Switch to buffer" "b" #'org-roam-switch-to-buffer
        :desc "Org Roam Capture" "c" #'org-roam-capture
        :desc "Org Roam" "r" #'org-roam-buffer-toggle
        :desc "Find node" "f" #'dendroam-node-find-initial-input
        :desc "Insert (skipping capture)" "I" #'org-roam-insert-immediate
        :desc "Capture in today's daily" "C" #'org-roam-dailies-capture-today
        (:prefix ("d" . "Open By date")
         :desc "Arbitrary date" "d" #'org-roam-dailies-find-date
         :desc "Tomorrow" "m" #'org-roam-dailies-find-tomorrow
         :desc "Today" "t" #'org-roam-dailies-find-today
         :desc "Yesterday" "y" #'org-roam-dailies-find-yesterday )
        ;; (:prefix ("j" . "Org Roam dailies capture")
        ;; :desc "Arbitrary date" "d" #'org-roam-dailies-capture-date
        ;; :desc "Tomorrow" "m" #'org-roam-dailies-capture-tomorrow
        ;; :desc "Today" "t" #'org-roam-dailies-capture-today
        ;; :desc "Yesterday" "y" #'org-roam-dailies-capture-yesterday )
        )
  ;; (map! :map org-roam-mode-map
  ;;       :g "C-c i" 'org-roam-node-insert)
  (global-set-key (kbd "C-c i") #'org-roam-node-insert)
  ;; (define-key map (kbd "C-c i") 'org-roam-node-insert)
  (setq org-roam-directory (concat org-directory "notes/")
        org-roam-node-display-template "${hierarchy}:${title}"
        org-roam-db-gc-threshold most-positive-fixnum
        ;; org-roam-title-to-slug-function #'vic/org-roam-title-input-to-slug
        org-id-link-to-org-use-id t)
  (add-to-list 'display-buffer-alist
               '(("^\\*org-roam\\*"
                  (display-buffer-in-direction)
                  (direction . right)
                  (window-width . 0.33)
                  (window-height . fit-window-to-buffer))))
  :config
  (setq org-roam-mode-sections
        (list #'org-roam-backlinks-section
              #'org-roam-reflinks-section
              ;;#'org-roam-unlinked-references-insert-section
              ))
  (org-roam-setup)
  ;;(load! "dendroam")
  (setq org-roam-capture-templates
        '(("d" "default" plain
           "%?"
           :if-new (file+head "${slug}.org"
                              "#+title: ${hierarchy-title}\n")
           :immediate-finish t
           :unnarrowed t)))
  (setq org-roam-capture-ref-templates
        '(("r" "ref" plain
           "%?"
           :if-new (file+head "${slug}.org"
                              "#+title: ${title}\n")
           :unnarrowed t)))
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* %?"
           :if-new (file+head "journal.daily.%<%Y.%m.%d>.org"
                              "#+title: %<%Y-%m-%d>\n"))))

  (set-company-backend! 'org-mode '(company-capf)))

(use-package lister
  :quelpa (lister :fetcher github
                  :repo "https://github.com/publicimageltd/lister"))

(use-package delve
  :quelpa (delve :fetcher github
                 :repo "https://github.com/publicimageltd/delve")
  :config
  (set-evil-initial-state! 'delve-mode 'insert)
  (map! :map delve-mode-map
        :n "gr"      #'delve-refresh-buffer
        :n "l" #'delve-expand-insert-tolinks
        :n "h"  #'devle-expand-insert-backlinks
        :localleader
        "RET"  #'lister-key-action
        "TAB"  #'delve-expand-toggle-sublist)
  (use-package! delve-minor-mode
    :hook (org-roam-mode . delve-minor-mode-maybe-activate))
  (use-package delve-minor-mode
            :config
            (add-hook 'org-mode-hook #'delve-minor-mode-maybe-activate))
  :bind (("<f12>" . delve-open-or-select)))

(use-package! dendroam
  :after org
  :config
  (setq dendroam-capture-templates
        '(("o" "OKRs" entry
           "* %?"
           :if-new (file+head "journal.okr.%<%Y.%m.%d>.org"
                              "#+title: %<%Y-%m-%d>\n"))
          ("p" "PPP" entry
           "* %?"
           :if-new (file+head "journal.ppp.%<%Y.%m.%V>.org"
                              "#+title: %<%Y-%m-%d>\n"))
          ("t" "Time note" entry
           "* %?"
           :if-new (file+head "${current-file}.%<%Y.%m.%d.%M%S%3N>.org"
                              "#+title: %^{title}\n\n"))
          ("s" "Scratch note" entry
           "* %?"
           :if-new (file+head "scratch.%<%Y.%m.%d.%M%S%3N>.org"
                              "#+title: %^{title}\n\n")))))

;; Some notes functions that are useful for me
(defun dendroam-insert-ppp (&optional goto)
  "Creates a new PPP note"
  (interactive "P")
  (org-roam-capture- :goto (when goto '(4))
                     :node (org-roam-node-create)
                     :templates org-roam-utils-capture-templates
                     :keys "p"
                     :props (list :default-time (current-time))))

(defun dendroam-node-find-initial-input ()
  (interactive)
  (org-roam-node-find nil (if (buffer-file-name)
                              (file-name-base (buffer-file-name))
                            "")))

(defun dendroam--eval-schema ()
  (interactive)
  (let ((file (concat org-roam-directory "cli.schema.el")))
    (eval
     (ignore-errors
       (thing-at-point--read-from-whole-string
        (with-temp-buffer
          (insert-file-contents file)
          (buffer-string)))))))


(defun dendroam-node-read (&optional initial-input filter-fn require-match)
  "Read and return an `org-roam-node'.
INITIAL-INPUT is the initial prompt value.
FILTER-FN is a function applied to the completion list.
If REQUIRE-MATCH, require returning a match."
  (let* ((nodes (org-roam-node--completions))
         (nodes (funcall (or filter-fn #'identity) nodes))
         (node (completing-read
                "Node: "
                (lambda (string pred action)
                  (message "String: %s, pred: %s, action: %s" string pred action)
                  (if (eq action 'metadata)
                      '(metadata
                        (annotation-function . (lambda (title)
                                                 (message "title: %s" title)
                                                 (funcall org-roam-node-annotation-function
                                                          (get-text-property 0 'node title))))
                        (category . org-roam-node))
                    (message "string in complete: %s " string)
                    (complete-with-action action nodes string pred)))
                nil require-match initial-input)))
    (or (cdr (assoc node nodes))
        (org-roam-node-create :title node))))

(defun test-hey ()
  (interactive)
  (complete-with-action t '("1" "2" "3") "2" nil))

(advice-add 'org-roam-node-read :override #'dendroam-node-read)

(defun vic/org-roam-title-to-slug (title)
  "Generates a dendron-like slug from *title*
this expects an input like: lang.elisp.what is nil
and will create a file wih the name: lang.elisp.what-is-nil"
  (cl-flet* ((nonspacing-mark-p (char)
                                (eq 'Mn (get-char-code-property char 'general-category)))
             (strip-nonspacing-marks (s)
                                     (apply #'string (seq-remove #'nonspacing-mark-p
                                                                 (ucs-normalize-NFD-string s))))
             (cl-replace (title pair)
                         (replace-regexp-in-string (car pair) (cdr pair) title)))
    (let* ((pairs `(("[^[:alnum:][:digit:]_.]" . "-")  ;; convert anything not alphanumeric except "."
                    (" " . "-")    ;; remove whitespaces
                    ("__*" . "-")  ;; remove sequential underscores
                    ("^_" . "")  ;; remove starting underscore
                    ("_$" . "")))  ;; remove ending underscore
           (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs)))
      (downcase slug))))


;; (defun vic/org-roam-format-hierarchy (hierarchy)
;;   "Formats path, title and tags to output something like:
;; (tag1,tag2) this.is.a.hierarchy: note title
;; where title will be the last child of the hierarchy
;; from the filename this.is.a.hierarchy.note-title.org"
;;   (let* ((name-split (split-string hierarchy "\\."))
;;          (hierarchy-no-title (mapconcat 'identity        ;;get just the hierarchy without the title
;;                                (remove (car(last name-split)) name-split) ".")))
;;     hierarchy-no-title))

(defun vic/org-roam--get-title-path-completions ()
  "Return an alist for completion.
The car is the displayed title for completion, and the cdr is a
plist containing the path and title for the file."
  (let* ((rows (org-roam-db-query [:select [files:file titles:title tags:tags files:meta] :from titles
                                   :left :join tags
                                   :on (= titles:file tags:file)
                                   :left :join files
                                   :on (= titles:file files:file)]))
         completions)
    (setq rows (seq-sort-by (lambda (x)
                              (plist-get (nth 3 x) :mtime))
                            #'time-less-p
                            rows))
    (dolist (row rows completions)
      (pcase-let ((`(,file-path ,title ,tags) row))
        (let ((k (vic/org-roam-prepend-hierarchy file-path title tags))
              (v (list :path file-path :title title)))
          (push (cons k v) completions))))))

(defun vic/org-roam-lookup (&optional initial-prompt completions filter-fn no-confirm)
  "Performs the same operation than org-roam-find-file but parsing initial-prompt
to get dendron-like hierarchies
this expects an input like: lang.elisp.what is nil
and will create a file wih the name: lang.elisp.what-is-nil
with a title: #+TITLE: What Is Nil"
  (interactive)
  (unless org-roam-mode (org-roam-mode))
  (let* ((completions (funcall (or filter-fn #'identity)
                               (or completions (vic/org-roam--get-title-path-completions))))
         (title-with-tags (if no-confirm
                              initial-prompt
                            (org-roam-completion--completing-read "Lookup: " completions
                                                                  :initial-input initial-prompt)))
         (res (cdr (assoc title-with-tags completions)))
         (file-path (plist-get res :path))
         (base-title (car (last (split-string title-with-tags "\\.")))))
    (if file-path
        (org-roam--find-file file-path)
      (let ((org-roam-capture--info `((title . ,base-title)
                                      (slug  . ,(funcall org-roam-title-to-slug-function title-with-tags))))
            (org-roam-capture--context 'title))
        (setq org-roam-capture-additional-template-props (list :finalize 'find-file))
         (org-roam-capture--capture)))))

(defun vic/org-roam-insert ()
  "a Wrapper to apply dendron lookup to insert link"
  (interactive "P")
  (apply #'org-roam-insert '(nil (vic/org-roam--get-title-path-completions) nil nil nil)))

(defun my/org-id-update-org-roam-files ()
  "Update Org-ID locations for all Org-roam files."
  (interactive)
  (org-id-update-id-locations (org-roam--list-all-files)))

(defun org-roam-v1-to-v2 ()
  ;; Create file level ID
  (org-with-point-at 1
    (org-id-get-create))
  ;; Replace roam_key into properties drawer roam_ref
  (when-let* ((refs (cdar (org-collect-keywords '("roam_key")))))
    (org-set-property "ROAM_REFS" (combine-and-quote-strings refs))
    (let ((case-fold-search t))
      (org-with-point-at 1
        (while (re-search-forward "^#\\+roam_key:" (point-max) t)
          (beginning-of-line)
          (kill-line)))))

  ;; Replace roam_alias into properties drawer roam_aliases
  (when-let* ((aliases (cdar (org-collect-keywords '("roam_alias")))))
    (org-set-property "ROAM_ALIASES" (combine-and-quote-strings aliases))
    (let ((case-fold-search t))
      (org-with-point-at 1
        (while (re-search-forward "^#\\+roam_alias:" (point-max) t)
          (beginning-of-line)
          (kill-line)))))
  (save-buffer))

(defun org-roam-replace-file-links-with-id ()
  (org-with-point-at 1
    (while (re-search-forward org-link-bracket-re nil t)
      (let* ((mdata (match-data))
             (path (match-string 1))
             (desc (match-string 2)))
        (when (string-prefix-p "file:" path)
          (setq path (expand-file-name (substring path 5)))
          (when-let ((node-id (caar (org-roam-db-query [:select [id] :from nodes
                                                        :where (= file $s1)
                                                        :and (= level 0)] path))))
            (set-match-data mdata)
            (replace-match (org-link-make-string (concat "id:" node-id) desc))))))))

;; Step 1: Convert all v1 files to v2 files
;; (dolist (f (org-roam--list-all-files))
;;   (with-current-buffer (find-file-noselect f)
;;     (org-roam-v1-to-v2)))

;; Step 2: Build cache
;; (org-roam-db-sync)

;; Step 3: Replace all file links with id links where possible

;; (dolist (f (org-roam--list-all-files))
;;   (with-current-buffer (find-file-noselect f)
;;     (org-roam-replace-file-links-with-id)))

;; Org roam
;; (map! :map org-roam-mode-map :g "C-c i" #'org-roam-node-insert)
;;Org journal
(map! :leader
      :prefix ("j" . "journal")
      :desc "New Entry" "j" #'org-journal-new-entry
      :desc "New Scheduled Entry" "J" #'org-journal-new-scheduled-entry
      :desc "Search forever" "s" #'org-journal-search-forever
      :desc "Open current journal file" "c" #'org-journal-open-current-journal-file
      :desc "Jump to next journal entry" "n" #'org-journal-next-entry
      :desc "Jump tolast journal entry" "l" #'org-journal-previous-entry
      )

(map! :leader
      :desc "Dired"
      "d d" #'dired
      :leader
      :desc "Dired jump to current"
      "d j" #'dired-jump
      (:after dired
        (:map dired-mode-map
         :leader
         :desc "Peep-dired image previews"
         "d p" #'peep-dired
         :leader
         :desc "Dired view file"
         "d v" #'dired-view-file)))
;; Make 'h' and 'l' go back and forward in dired. Much faster to navigate the directory structure!
(evil-define-key 'normal dired-mode-map
  (kbd "h") 'dired-up-directory
  (kbd "l") 'dired-open-file) ; use dired-find-file instead if not using dired-open package
;; If peep-dired is enabled, you will get image previews as you go up/down with 'j' and 'k'
(evil-define-key 'normal peep-dired-mode-map
  (kbd "j") 'peep-dired-next-file
  (kbd "k") 'peep-dired-prev-file)
(add-hook 'peep-dired-hook 'evil-normalize-keymaps)
;; Get file icons in dired
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
;; With dired-open plugin, you can launch external programs for certain extensions
;; For example, I set all .png files to open in 'sxiv' and all .mp4 files to open in 'mpv'
(setq dired-open-extensions '(("gif" . "sxiv")
                              ("jpg" . "sxiv")
                              ("png" . "sxiv")
                              ("mkv" . "mpv")
                              ("mp4" . "mpv")))
