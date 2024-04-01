(load-file "~/.doom.d/utils.el")
(setq user-full-name "Victor Rodriguez"
      user-mail-address "vrodriguez@confluent.io"
      ;; Makes sure that weekdays in the timestamps of org-mode files and the agenda appear in English
      system-time-locale "C")

(setq-default fill-column 120)

(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

(add-hook 'window-configuration-change-hook #'vic/auto-balance-windows)

(add-hook 'after-save-hook #'vic/chezmoi-re-add-on-save)

(setq vic/org-dir "~/dev/kb/braindump.org/")

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(setq vterm-shell "fish")

(map! :map 'override
      :leader
      :desc "M-x" ";" #'execute-extended-command
      :desc "Eval expression" ":" #'pp-eval-expression)

(map! :leader
      :prefix "f"
      :desc "Find file" "." #'find-file)

(setq display-line-numbers-type 'relative)

(setq doom-font (font-spec :family "Iosevka" :size 16)
      doom-variable-pitch-font (font-spec :family "Iosevka Etoile" :size 16)
      doom-unicode-font (font-spec :family "Symbols Nerd Font Mono" :size 16))

(after! doom-modeline
  (doom-modeline-def-modeline 'main
    '(bar matches buffer-info remote-host buffer-position parrot selection-info)
    '(misc-info minor-modes lsp checker input-method buffer-encoding major-mode process vcs "  "))) ; <-- added padding here

(setq doom-theme 'kanagawa)

;; (setq doom-horizon-comment-bg t)
;; (setq! doom-gruvbox-dark-variant "hard")
;; (setq catppuccin-flavor 'mocha) ;; or 'latte, 'macchiato, or 'mocha
;; (after! catppuccin
;;   (catppuccin-reload))

 (set-frame-parameter (selected-frame)'alpha '(97 . 97))
 (add-to-list 'default-frame-alist'(alpha . (97 . 97)))
 ;; (add-to-list 'default-frame-alist'(alpha . (100 . 100)))

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
  '(org-document-title ((t (:inherit document-title :height 1.4))))
)

(setq org-list-indent-offset 2)

;; (setq! org-modern-list '(("+" . "◦")
;;                          ("-" . "◦")
;;                          ("*" . "•")))
(setq! org-modern-list '((43 . "◦")     ;; "+"
                         (45 . "◦")     ;; "-"
                         (42 . "•")))   ;; "*"

;; (add-to-list 'org-emphasis-alist
;;              '("*" (:foreground "magenta" :weight extra-bold)))

(custom-set-faces
 `(org-verbatim ((t (:inherit 'italic :foreground ,(doom-color 'dark-yellow))))))

(setq org-emphasis-alist
      '(("*" (:foreground "#d3869b" :weight bold)) ;,(doom-color 'violet)
      ;; '(("*" (:foreground ,(doom-color 'violet) :weight bold))
        ;; ("*" bold)
         ("/" italic)
         ("_" underline)
         ("=" org-verbatim verbatim)
         ("~" org-code verbatim)
         ("+"
          (:strike-through t))))

(evil-define-key 'normal dired-mode-map
  (kbd "h") 'dired-up-directory
  ;; (kbd "l") 'dired-open-file) ; use dired-find-file instead if not using dired-open package
  (kbd "l") 'dired-find-file) ;

(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
(add-hook 'dired-mode-hook (lambda ()
                             display-fill-column-indicator-mode nil))

(setq dired-listing-switches "--group-directories-first -al")

;; (use-package deft
;;   :init
;;   (setq deft-directory vic/org-dir)
;;   (setq deft-extensions '("org"))
;;   (setq deft-recursive t))

;; (use-package! company
;;   :config
;;   (setq company-tooltip-limit 10
;;         company-tooltip-minimum-width 30
;;         company-minimum-prefix-length 2
;;         company-box-doc-enable t
;;         company-box-scrollbar nil
;;         company-idle-delay 0.3)
;;   ;; (after! org-mode
;;   ;;   (set-company-backend! 'org-mode'))
;;   )

(use-package! cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

(use-package! corfu
  :config
  (after! evil-collection-corfu
    (evil-collection-define-key 'insert 'corfu-map
      (kbd "C-f") #'corfu-insert))
  (map! (:map corfu-popupinfo-map
              "C-n" #'corfu-popupinfo-scroll-down
              "C-p" #'corfu-popupinfo-scroll-up)))

(use-package! vertico
  :init
  (map! :when (modulep! :editor evil +everywhere)
        :map vertico-map
        "C-<return>" #'vertico-exit-input))

(use-package! consult
  :defer t
  :init
  (map! :leader
        :desc "Search on current file" "/" #'consult-line
        :desc "Find file recursive" "." #'consult-find
        :desc "Switch buffers" "SPC" #'consult-buffer
        (:prefix "s"
         :desc "Ripgrep on current dir" "g" #'consult-ripgrep
         :desc "Search yank history" "y" #'consult-yank-from-kill-ring
         ))
  :config
  (add-to-list 'consult-preview-allowed-hooks 'global-org-modern-mode-check-buffers)
  (add-to-list 'consult-preview-allowed-hooks 'global-visual-line-mode-check-buffers)
  (add-to-list 'consult-preview-allowed-hooks 'global-visual-fill-column-mode-check-buffers))

;; (after! org
;;   (setf (alist-get 'file org-link-frame-setup) #'find-file-other-window)
;;   (setq! help-at-pt-display-when-idle t)
;;   (setq org-export-with-toc nil)
;;   (setq org-link-make-description-function 'vic/get-url-title)
  ;; hooks
  ;; (add-hook 'org-mode-hook 'turn-on-auto-fill)
  ;; (add-hook 'org-mode-hook 'org-appear-mode)
  ;; (add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
  ;; bindings

(use-package! org
  :init
  (setq org-directory vic/org-dir)
  :config
  (map! :map org-mode-map
        "C-k" #'org-previous-visible-heading
        "C-j" #'org-next-visible-heading
        ;; :n "RET" #'+org/dwim-at-point
        )
  (setf (alist-get 'file org-link-frame-setup) #'find-file-other-window)
  (add-hook 'org-mode-hook 'org-appear-mode)
  (setq org-pretty-entities t
        org-hide-emphasis-markers t
        org-indent-indentation-per-level 4
        org-export-with-toc nil
        org-link-make-description-function 'vic/get-url-title
        split-width-threshold 0
        split-height-threshold nil)
  (setq! help-at-pt-display-when-idle t)
  (global-visual-line-mode)
  (global-visual-fill-column-mode)
  :hook '(
          ;; (visual-line-mode . visual-fill-column-mode)
          (org-mode . turn-on-auto-fill)
          (org-mode . org-appear-mode)))

(add-hook 'org-mode-hook (lambda ()
                           (setq visual-fill-column-center-text t)
                           (setq visual-fill-column-enable-sensible-window-split t)
                           (setq visual-fill-column-width 120)))

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

(defvar vic/org-electric-pairs '((?~ . ?~)) "Electric pairs for org-mode.")

(defun org-add-electric-pairs ()
  (electric-pair-mode  1)
  (setq! electric-pair-pairs nil)
  (setq! electric-pair-text-pairs nil)
  (setq! electric-pair-pairs '((?~ . ?~)))
  (setq! electric-pair-text-pairs (delq '(?\< . ?\>) electric-pair-pairs))
  (setq-local electric-pair-inhibit-predicate
              `(lambda (c)
                 (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))
  )

(add-hook 'org-mode-hook 'org-add-electric-pairs)

;; (add-hook 'org-mode-hook 'org-fragtog-mode)

(use-package! org-appear
  :after org
  :config (setq org-appear-autolinks nil))

(use-package! org-roam
  :after org
  :init
  (map! :leader
        :prefix ("r" . "roam")
        ;; :desc "insert" "i" #'org-roam-insert
        :desc "Show graph" "g" #'org-roam-graph
        :desc "Find backlinks" "b" #'consult-org-roam-backlinks
        :desc "Search in Org Roam nodes" "s" #'consult-org-roam-search
        :desc "Org Roam Capture" "c" #'org-roam-capture
        :desc "Org Roam" "r" #'org-roam-buffer-toggle
        ;; :desc "Find node" "f" #'dendroam-node-find-initial-input
        :desc "Find node" "f" #'org-roam-node-find
        ;; :desc "Find node" "f" #'consult-org-roam-file-find
        :desc "Insert node link" "i" #'org-roam-node-insert
        :desc "Insert (skipping capture)" "I" #'org-roam-insert-immediate
        :desc "Capture in today's daily" "t" #'org-roam-dailies-capture-today
        :desc "Take screenshot and insert at point" "," #'org-download-screenshot
        :desc "Insert clipboard image at point" "." #'org-download-clipboard
        (:prefix ("d" . "Open By date")
         :desc "Arbitrary date" "d" #'org-roam-dailies-goto-date
         :desc "Tomorrow" "m" #'org-roam-dailies-goto-tomorrow
         :desc "Today" "t" #'org-roam-dailies-goto-today
         :desc "Yesterday" "y" #'org-roam-dailies-goto-yesterday )
        )
  (global-set-key (kbd "C-c i") #'org-roam-node-insert)
  ;; (define-key map (kbd "C-c i") 'org-roam-node-insert)
  (setq org-roam-directory vic/org-dir
        org-roam-completion-everywhere nil
        org-roam-node-display-template (format "%s %s ${doom-hierarchy-alias:*} ${backlinkscount}"
                                               (propertize "${doom-type:10}" 'face 'font-lock-keyword-face)
                                               (propertize "${doom-tags:20}" 'face 'org-tag))
        ;; org-roam-node-display-template "${doom-tags:20} ${title:*}"
        ))

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
        ("p" "Project" plain "%?"
         :if-new (file+head "projects/${slug}.org" "#+title: ${title}\n#+filetags: :project:\n\n")
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

(setq org-roam-dailies-directory (concat vic/org-dir "log/daily"))

(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(use-package! consult-org-roam
  :defer t
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

(use-package! org-download
  :init
  (with-eval-after-load 'org
    (org-download-enable))
  :config
  (setq-default org-download-image-dir (concat vic/org-dir "_meta/assets/"))
  (setq org-download-screenshot-method "screencapture -i %s"
        org-download-heading-lvl nil
        org-download-edit-cmd "open %s"
        org-download-method 'directory)
  (setq org-image-actual-width 720)
  :hook (dired-mode . org-download-enable))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell . t)))

(setq lsp-bash-highlight-parsing-errors t)

(after! evil
  (map! (:map evil-window-map
         "v" #'+evil/window-vsplit-and-follow
         "s" #'+evil/window-split-and-follow)))
