(load-file "~/.doom.d/utils.el")
(setq user-full-name "Victor Rodriguez"
      user-mail-address "vrodriguez@confluent.io"
      ;; Makes sure that weekdays in the timestamps of org-mode files and the agenda appear in English
      system-time-locale "C")

(setq-default fill-column 120)

(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

(add-hook 'window-configuration-change-hook #'vic/auto-balance-windows)

(add-hook 'after-save-hook #'vic/chezmoi-re-add-on-save)

(setq vic/org-dir "~/dev/braindump/")

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(setq vterm-shell "fish")

(setq display-line-numbers-type 'relative)

(setq doom-font (font-spec :family "Iosevka" :size 14)
      doom-variable-pitch-font (font-spec :family "Iosevka Etoile" :size 14)
      doom-unicode-font (font-spec :family "Symbols Nerd Font Mono" :size 14))

(after! doom-modeline
  (doom-modeline-def-modeline 'main
    '(bar matches buffer-info remote-host buffer-position parrot selection-info)
    '(misc-info minor-modes lsp checker input-method buffer-encoding major-mode process vcs "  "))) ; <-- added padding here

;; (after! org
;;   (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
;;   ;; (setq org-superstar-remove-leading-stars t
;;     (setq org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●")
;;           org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(with-eval-after-load 'org (global-org-modern-mode))

(custom-set-faces
  '(org-level-1 ((t (:inherit outline-1 :height 1.2))))
  '(org-level-2 ((t (:inherit outline-2 :height 1.2))))
  '(org-level-3 ((t (:inherit outline-3 :height 1.2))))
  '(org-level-4 ((t (:inherit outline-4 :height 1.2))))
  '(org-level-5 ((t (:inherit outline-5 :height 1.2))))
)

(setq org-list-indent-offset 2)

(setq doom-theme 'doom-gruvbox)

 (set-frame-parameter (selected-frame)'alpha '(95 . 95))
 (add-to-list 'default-frame-alist'(alpha . (95 . 95)))

(evil-define-key 'normal dired-mode-map
  (kbd "h") 'dired-up-directory
  ;; (kbd "l") 'dired-open-file) ; use dired-find-file instead if not using dired-open package
  (kbd "l") 'dired-find-file) ;

(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
(add-hook 'dired-mode-hook (lambda ()
                             display-fill-column-indicator-mode nil))

(setq dired-listing-switches "--group-directories-first -al")

(use-package deft
  :init
  (setq deft-directory vic/org-dir)
  (setq deft-extensions '("org"))
  (setq deft-recursive t))

(setq org-directory vic/org-dir)

(after! org (setq org-pretty-entities t
                  org-hide-emphasis-markers t)
  (setf (alist-get 'file org-link-frame-setup) #'find-file-other-window)
  (setq split-height-threshold nil)
  (setq split-width-threshold 0)
  ;; hooks
  ;; (add-hook 'org-mode-hook 'turn-on-auto-fill)
  (add-hook 'org-mode-hook 'org-appear-mode)
  (add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
  ;; bindings
  (map! :map org-mode-map
        "C-k" #'org-previous-visible-heading
        "C-j" #'org-next-visible-heading))

(add-hook 'org-mode-hook (lambda ()
                           (setq visual-fill-column-center-text nil)))

(add-hook 'org-mode-hook
          (lambda ()
            (add-to-list 'display-buffer-alist
                  ;; '(("\\`\\*Org\\(?:-mode\\| Agenda\\)\\*"
                  '(("\\`\\*Org-roam\\*\\*\\[[^]]+\\]\\*\\'"
                     (display-buffer-in-side-window)
                     (side . right)
                     (slot . -1)
                     (window-width . 0.33)
                     (preserve-size . (t . nil)))
                    ))))

(defvar vic/org-electric-pairs '((?~ . ?~) (?/ . ?/) (?= . ?=)) "Electric pairs for org-mode.")

(defun org-add-electric-pairs ()
  (electric-pair-mode  1)
  (setq-local electric-pair-pairs (append electric-pair-pairs vic/org-electric-pairs))
  (setq-local electric-pair-text-pairs electric-pair-pairs))

(add-hook 'org-mode-hook 'org-add-electric-pairs)

(add-hook 'org-mode-hook 'org-fragtog-mode)

(use-package! org-appear
  :after org
  :init (setq org-appear-autolinks nil))

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
        ;; :desc "Find node" "f" #'dendroam-node-find-initial-input
        :desc "Find node" "f" #'org-roam-node-find
        :desc "Insert node link" "i" #'org-roam-node-insert
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
  (global-set-key (kbd "C-c i") #'org-roam-node-insert)
  ;; (define-key map (kbd "C-c i") 'org-roam-node-insert)
  (setq org-roam-directory vic/org-dir
        org-roam-node-display-template (format "%s %s ${doom-hierarchy-alias:*} ${backlinkscount}"
                                               (propertize "${doom-type:10}" 'face 'font-lock-keyword-face)
                                               (propertize "${doom-tags:20}" 'face 'org-tag))))

(after! org-roam
  (set-popup-rules!
    `((,(regexp-quote org-roam-buffer) ; persistent org-roam buffer
       :side right :width 0.20 :height 0.5 :ttl nil :modeline nil :quit nil :slot 1)
      ("^\\*org-roam: " ; node dedicated org-roam buffer
       :side right :width 0.20 :height 0.5 :ttl nil :modeline nil :quit nil :slot 2))))

(setq org-roam-capture-templates
      '(("m" "main" plain
         "%?"
         :if-new (file+head "main/${slug}.org"
                            "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
        ("r" "reference" plain "%?"
         :if-new
         (file+head "refs/${title}.org" "#+title: ${title}\n#+filetags: :reference:")
         :immediate-finish t
         :unnarrowed t)
        ;; ("s" "Meeting" plain "%?"
        ;;  ;; :target (file+olp "confluent/${customer-slug}/${customer-slug}.org" ("Meetings" "%<%Y-%m-%d-%H:%M> ${title}"))
        ;;  :target (file+olp vic/pick-customer-file ("Meetings" "%<%Y-%m-%d-%H:%M> ${title}"))
        ;;  ;; :target (file+olp "confluent/sample/sample.org" ("Meetings" "%<%Y-%m-%d-%H:%M> ${title}"))
        ;;  :unnarrowed t
        ;;  :clock-in t
        ;;  )
        ("s" "Customer meeting" plain "%?"
         :if-new (file+head+olp "confluent/${slug}/${slug}.org" "#+title: ${title}\n#+filetags: :customer:\n\n* Use Cases\n* Architecture\n" ("Meetings" "%<%Y-%m-%d-%H:%M> ${Meeting title}"))
         ;; :file-name "confluent/${slug}/${slug}"
         ;; :head "#+title: ${title}"
         :unnarrowed t
         :clock-in t
         )
        ("c" "Customer" plain "%?"
         :target (file+head "confluent/${slug}/${slug}.org" "#+title: ${title}\n#+filetags: :customer:\n\n* Use Cases\n* Architecture\n* Meetings\n")
         ;; :file-name "confluent/${slug}/${slug}"
         ;; :head "#+title: ${title}"
         :unnarrowed t
         :immediate-finish t
         )
        ("d" "draft" plain "%?"
         :if-new
         (file+head "drafts/${title}.org" "#+title: ${title}\n#+filetags: :draft:\n")
         :immediate-finish t
         :unnarrowed t)))

(use-package! consult-org-roam
  :ensure t
  :after org-roam
  :init
  (require 'consult-org-roam)
  (consult-org-roam-mode 1)
  ;; previewed with delay
  (consult-customize
   consult-org-roam-search
   org-roam-node-find
   :preview-key '(:debounce 0.8 any))
  
  ;; Manual preview
  (consult-customize
   org-roam-node-insert
   :preview-key "C-SPC")

  :custom
  (consult-org-roam-grep-func #'consult-ripgrep))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell . t)))

(setq lsp-bash-highlight-parsing-errors t)
