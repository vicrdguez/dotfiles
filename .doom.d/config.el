(setq user-full-name "Victor Rodriguez"
      user-mail-address "vrodriguez@confluent.io")
(setq ns-auto-hide-menu-bar t)

(setq vic/org-dir "~/Dropbox/org/")

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

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

(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 17 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "JetBrainsMono Nerd Font" :size 17))
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
;; (custom-set-faces!
;;   '(font-lock-comment-face :slant italic)
;;   '(font-lock-keyword-face :slant italic))

(use-package all-the-icons)

(setq doom-theme 'doom-one)
(map! :leader
      :desc "Load new theme"
      "h t" #'counsel-load-theme)

(setq org-directory vic/org-dir)
(after! org
  ;; (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (setq org-hide-emphasis-markers t)
  (setq org-startup-folded 'content)
  (setq org-export-with-section-numbers nil)
  (setq org-directory "~/Dropbox/org/"
        org-default-notes-file (expand-file-name "notes.org" org-directory)
        org-ellipsis " ▼ "
        org-log-done 'time))
(setq display-line-numbers-type t)
(add-hook 'org-mode-hook 'turn-on-auto-fill)

(after! org
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
  ;; (setq org-superstar-remove-leading-stars t
    (setq org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●")))

(after! org
  (require 'org-download)
  (setq-default org-download-image-dir (concat org-directory "_attachments/"))
  (setq
        org-download-screenshot-method "screencapture -i %s"
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
                       :template "%(format-time-string \"#+TITLE: PPP Week %V - %B, %d %Y\")\n\n* Progress\n** %?\n* Plans\n* Problems\n"
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
                       :template "* TODO Keep feeding [[%F][%f]] note \t:BrainForest:\n:PROPERTIES:\n:CREATED: %U\n:Origin: %a\n:END:\n"
                       :immediate-finish t)
                      ("Meeting" :keys "m"
                       :file vic/inbox-path
                       :type entry
                       :template "* MEETING with %^{person}\t:MEETING:\n:PROPERTIES:\n:CREATED: %U\n:Origin: %a\n:END:\n\n%?"
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

(after! org-roam
  (setq org-roam-directory (concat org-directory "BrainForest/"))
  (setq org-roam-capture-templates
        '(("d" "default" plain #'org-roam--capture-get-point
           "%?"
           :file-name "%<%Y%m%d%H%M%S>-${slug}"
           :head "#+TITLE: ${title}\n#+CATEGORY: ${title}\n#+roam_tags: seedBox"
           :unnarrowed t
            )
          )
        )
  (setq org-roam-completion-everywhere nil)
  (setq org-roam-link-auto-replace nil)
  (setq org-roam-link-use-custom-faces nil)
  )

;; Org roam
(map! :leader
      :prefix ("r" . "roam")
      ;; :desc "insert" "i" #'org-roam-insert
      :desc "Show graph" "g" #'org-roam-graph
      :desc "Switch to buffer" "b" #'org-roam-switch-to-buffer
      :desc "Org Roam Capture" "c" #'org-roam-capture
      :desc "Org Roam" "r" #'org-roam
      :desc "Find file" "f" #'org-roam-insert
      :desc "Insert (skipping capture)" "I" #'org-roam-insert-immediate
      (:prefix ("d" . "By date")
      :desc "Arbitrary date" "d" #'org-roam-dailies-find-date
      :desc "Tomorrow" "m" #'org-roam-dailies-find-tomorrow
      :desc "Today" "t" #'org-roam-dailies-find-today
      :desc "Yesterday" "y" #'org-roam-dailies-find-yesterday )
      )
(map! :map org-roam-mode-map :g "C-c i" #'org-roam-insert)
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
