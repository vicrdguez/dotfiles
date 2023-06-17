;;; obsidian.el -*- lexical-binding: t; -*-


(defun obsidian-to-org-roam-test1 (filename)
  "Converts Obsidian file FILENAME to Org-roam file with preserved links."
  (let ((buffer (find-file filename))
        (new-buffer (generate-new-buffer "*org-roam*")))
    (with-current-buffer buffer
      ;; Replace Obsidian-style ID links with Org-mode-style ID links
      (goto-char (point-min))
      (while (re-search-forward "\\[\\[\\#\\([^]]+\\)\\]\\]" nil t)
        (let* ((match-str (match-string 1))
               (org-id-link (concat "file:" (file-name-sans-extension (buffer-file-name buffer)) "::" match-str))
               (replacement-str (concat "[[id:" org-id-link "][" match-str "]]")))
          (replace-match replacement-str t nil)))
      ;; Insert Org ID property at top of buffer
      (goto-char (point-min))
      (insert (concat "#+title: " (file-name-nondirectory filename) "\n"
                      "#+roam_key: " (org-id-get-create) "\n"
                      "#+roam_tags: \n\n"))
      ;; Write converted buffer to new buffer and kill old buffer
      (write-region (point-min) (point-max) new-buffer)
      (kill-buffer buffer))
    (switch-to-buffer new-buffer)))


;; /Users/vrodriguez/Library/Mobile Documents/iCloud~md~obsidian/Documents/Brainforest/Increasing the number of partitions on a Kafka topic.md
(defun obsidian-to-org-roam-test2 (filename)
  "Converts Obsidian file FILENAME to Org-roam file with preserved links."
  (let ((buffer (find-file filename))
        (new-buffer (generate-new-buffer "*obs-org-roam*")))
    (with-current-buffer buffer
      ;; Replace Obsidian-style ID links with Org-mode-style ID links
      (goto-char (point-min))
      (while (re-search-forward "\\[\\[\\([^]|#]+\\)\\(|\\([^]]+\\)\\)?#?\\([^]]*\\)\\]\\]" nil t)
        (let* ((match-str (match-string 1))
               (alias-str (or (match-string 3) match-str))
               (heading-str (match-string 4))
               (org-id-link (concat "id:" (org-roam--get-create-identifier (org-roam--title-to-slug match-str))))
               (replacement-str (cond ((and (not (string= alias-str match-str)) heading-str) (concat "[[" org-id-link "][" alias-str "]]"))
                                       (heading-str (concat "[[" org-id-link "][" heading-str "]]"))
                                       (t (concat "[[" org-id-link "][" match-str "]]")))))
          (replace-match replacement-str t nil)))
      ;; Replace Markdown quote blocks with Org-mode blocks
      (goto-char (point-min))
      (while (re-search-forward "^> !\\(\\w+\\)$" nil t)
        (let ((block-type (match-string 1))
              (begin-block (concat "#+begin_" (match-string 1) "\n"))
              (end-block (concat "#+end_" (match-string 1) "\n")))
          (replace-match (concat begin-block end-block) t nil)))
      ;; Insert Org ID property at top of buffer
      (goto-char (point-min))
      (insert (concat "#+title: " (file-name-nondirectory filename) "\n"
                      "#+roam_tags: \n"))
      ;; (org-roam-tag-add)
      ;; (when-let ((yaml-frontmatter (org-roam--extract-yaml-frontmatter)))
      ;;   (when-let ((alias (cdr (assoc "alias" yaml-frontmatter))))
      ;;     (org-roam-alias-add alias)))
      (org-id-get-create)
      ;; Write converted buffer to new buffer and kill old buffer
      (write-region (point-min) (point-max) new-buffer)
      (kill-buffer buffer))
    (switch-to-buffer new-buffer)))

;; Define function to convert all Obsidian files in a folder to Org-roam files
(defun obsidian-folder-to-org-roam (folder)
  "Converts all Obsidian files in FOLDER to Org-roam files with preserved links."
  (let ((obsidian-files (directory-files folder t ".md$")))
    (mapc #'obsidian-to-org-roam obsidian-files)))
