;;; utils.el -*- lexical-binding: t; -*-

(defun vic/display-new-buffer-vsplit (buffer force-other-window)
  (or (get-buffer-window buffer)
      (if (> (count-windows) 6)
          (let ((new-win
                 (if (> (window-width) 80)
                     (split-window-horizontally)
                   (split-window-vertically))))
            (set-window-buffer new-win buffer)
            (balance-windows (window-parent))
            new-win)
        (let ((new-win (get-lru-window)))
          (set-window-buffer new-win buffer)
          new-win))))

(defun vic/find-file-evil-vsplit (filename &optional wildcards)
  (interactive "P")
  (let ((evil-vsplit-window-right t))
  (evil-window-vsplit nil filename)))

(defun vic/display-buffer-make-rule (predicate display-func plist)
  (message plist)
  (if (plist-get plist :ignore)
      (list predicate nil)
    (let* ((alist
            `((actions       . ,(plist-get plist :actions))
              (side          . ,(plist-get plist :side))
              (size          . ,(plist-get plist :size))
              (window-width  . ,(plist-get plist :width))
              (window-height . ,(plist-get plist :height))
              (slot          . ,(plist-get plist :slot))
              (vslot         . ,(plist-get plist :vslot))))
           (params
            `((ttl      . ,(plist-get plist :ttl))
              (quit     . ,(plist-get plist :quit))
              (select   . ,(plist-get plist :select))
              (modeline . ,(plist-get plist :modeline))
              (autosave . ,(plist-get plist :autosave))
              ,@(plist-get plist :parameters))))
      `(,predicate (,display-func)
                   ,@alist
                   (window-parameters ,@params)))))

(defun vic/set-display-rule (predicate display-func &rest plist)
  (declare (indent defun))
  (add-to-list display-buffer-alist (vic/display-buffer-make-rule predicate display-func plist))
  display-buffer-alist)

(defun vic/auto-balance-windows ()
  "Auto-balance windows when new buffers are created. Excludes the `Org Src` buffer since emacs freezes when opening it"
  (unless
      (or
       (string-match-p "^\\*Org Src" (buffer-name (current-buffer)))
       (string-match-p "^ \\*which-key" (buffer-name (current-buffer)))
       (string-match-p "^\\*doom" (buffer-name (current-buffer)))
       (string-match-p "^\\*helpful" (buffer-name (current-buffer))))
    (balance-windows (window-parent))))

(defun vic/chezmoi-re-add-on-save ()
  "Runs `chezmoi add ' for `doom-user-dir' when any of the files in it is changed, so the doom user config gets tracked
by the chezmoi dotfiles manager"
  (when (string-prefix-p doom-user-dir buffer-file-name)
    (message (concat "chezmoi add " doom-user-dir))
    (shell-command (concat "chezmoi add " doom-user-dir))))


;; (defun create-meeting-note ()
;;   "Create a meeting note under the selected customer folder with org-capture and add it to org-roam."
;;   (interactive)
;;   (let* ((customer-dir (completing-read "Select a customer: " (directory-files (concat org-roam-directory "confluent") t "^[^.]")))
;;          (customer-name (file-name-nondirectory (directory-file-name customer-dir))))
;;     (unless (file-directory-p (expand-file-name "meetings" customer-dir))
;;       (make-directory (expand-file-name "meetings" customer-dir)))
;;     (let* ((file-name (concat customer-name ".org"))
;;            (file-path (expand-file-name file-name customer-dir))
;;            (org-roam-capture-templates `(("m" "Meeting" plain
;;                                           (function org-roam-capture--get-point)
;;                                           "%?"
;;                                           :file-name ,(concat "meetings/" (format-time-string "%Y-%m-%d") ".org")
;;                                           :head ,(concat "#+title: " customer-name " Meeting\n\n")
;;                                           :unnarrowed t))))
;;       (unless (file-exists-p file-path)
;;         (write-region "" nil file-path))
;;       (org-roam-capture -1 "m"))))

(defun vic/create-meeting-note ()
  "Create a new meeting note for a customer."
  (interactive)
  (let* ((customers-dir (concat org-roam-directory "confluent/"))
         (customer-list (seq-filter
                         (lambda (f) (file-directory-p (concat customers-dir f)))
                         (directory-files customers-dir)))
         (customer (completing-read "Select a customer: " customers-list))
         (customer-dir (expand-file-name customer customers-dir))
         (meetings-dir (concat customer-dir customer "/meetings/"))
         (org-roam-capture-templates `(("m" "Meeting" plain
                                        (function org-roam-capture--get-point)
                                        "%?"
                                        :file-name ,(concat meetings-dir (format-time-string "%Y-%m-%d") ".org")
                                        :head ,(concat "#+title: " customer-name " Meeting\n\n")
                                        :unnarrowed t
                                        :immediate-finish t)))
         (customer-notes-file (concat customer-notes-dir
                                      (format-time-string "%Y-%m-%d_%H%M%S")
                                      ".org")))
    (unless (file-directory-p (concat cust))
      (message customer-dir)
      (make-directory customer-dir))
    ;; (unless (file-directory-p customer-notes-dir)
    ;;   (make-directory customer-notes-dir))
    ;; (unless (file-exists-p customer-notes-file)
    ;;   (org-capture nil "c")
    ;;   (org-roam-db-update))
    ;; (find-file customer-notes-file)
    ;; (goto-char (point-max))
    ;; (insert (format "\n\n* %s Meeting on %s\n\n"
    ;;                 customer
    ;;                 (format-time-string "%A, %B %e, %Y"))))
    ))
;; (defun customer-meeting-note ()
;;   (interactive)
;;   (let ((customer (completing-read "Select customer: " (directory-files "~/customers/" t "^[^\.]"))))
;;     (org-roam-capture :keys "m" :node (org-roam-node-create :title customer :tags '("customer")) :extra-args `(:file ,(concat "~/customers/" customer "/meetings/" (format-time-string "%Y%m%d%H%M%S") "-" (downcase (replace-regexp-in-string " " "-" customer)) ".org"))))))


(defun vic/org-roam-db-get-customers ()
  (let ((customers
         (org-roam-db-query [:select [nodes:title, nodes]
                                     :from nodes
                                     :join tags :on (= nodes:id tags:node_id)
                                     :where (= tags:tag "customer")])))
    customers))

(defun vic/org-roam-pick-maybe-create-customer ()
    (interactive)
  (let ((customer (completing-read "Select customer: " (vic/org-roam-db-get-customers))))
    (message "Customer is: %s" customer))
    )


;; Org-roam custom getters
(cl-defmethod org-roam-node-cust-name (node)
  "Gets customer"
  (file-name-base (org-roam-node-file node)))

(provide 'vic-utils)
