;;; utils.el -*- lexical-binding: t; -*-

(defvar vic/auto-balance-exclude-list '())
(setq vic/auto-balance-exclude-list '( "^\\*Org Src"
                                       "^\\*doom"
                                       "^\\*Minibuf.*"
                                       "^\\*Minibuf-*"
                                       "^\\*Org Select"
                                       "^\\CAPTURE"
                                       "^\\*which-key*"
                                       "^\\ *which-key*"
                                       "^\\*compilation*"
                                       "^\\*Customize"
                                       "^\\*helpful"))

(defun vic/auto-balance-windows ()
  "Auto-balance windows when new buffers are created. Excludes the buffers from `vic/auto-balance-exclude-list' that
make emacs freez when balanced or give other problems.

`which-key' buffer in particular was difficult to target, but using `window-parent' as parameter for `balance-windows'
worked"
  (let* ((buff (buffer-name (current-buffer)))
         (win (get-buffer-window buff)))

    ;; (message "Current buffer name: %s\n is window?: %s\n is popup?: %s" buff (windowp win) (+popup-buffer-p win))
    (unless (or
             (not (windowp win))
             (and (windowp win)
                  (+popup-buffer-p win))
             (seq-some (lambda (pattern) (string-match-p pattern (buffer-name (current-buffer))))
                       vic/auto-balance-exclude-list))

      (balance-windows (window-parent))
      ;; (balance-windows)
      ;; (message "Current buffer name: %s" (buffer-name (current-buffer)))
      )))

(defun vic/chezmoi-re-add-on-save ()
  "Runs `chezmoi add ' for `doom-user-dir' when any of the files in it is changed, so the doom user config gets tracked
by the chezmoi dotfiles manager"
  (when (string-prefix-p doom-user-dir buffer-file-name)
    (message (concat "chezmoi add " doom-user-dir))
    (shell-command (concat "chezmoi add " doom-user-dir))))


(defun vic/org-roam-db-get-customers ()
  "Gets org-roam nodes tagged with `:customer:' and returnd them as a list"
  (let ((customers
         (org-roam-db-query [:select [nodes:title]
                             :from nodes
                             :join tags :on (= nodes:id tags:node_id)
                             :where (= tags:tag "customer")])))
    customers))

(defun vic/org-roam-pick-maybe-create-customer (&optional goto)
  (interactive)
  (let* ((customer-node (org-roam-node-read nil (lambda (node)
                                                  (member "customer" (org-roam-node-tags node)))
                                            nil nil "Select a customer: "))
         (customer-slug (org-roam-node-slug customer-node))
         (customer-file (org-roam-node-file customer-node)))
    (message "File is: %s" (org-roam-node-file customer-node))
    (unless customer-file
      (message "selected customer node is: %s" customer-node)
      (message "node does not exist")
      (org-roam-capture- :goto (when goto '(4))
                         :node customer-node
                         :templates org-roam-capture-templates
                         :keys "c"
                         :props '(:immediate-finish t)
                         )
      )
    (message "I'm here!: %s" customer-slug)
    customer-slug))



(defun vic/pick-customer-file ()
  (let* ((customer-slug (vic/org-roam-pick-maybe-create-customer))
         (target (expand-file-name (concat customer-slug ".org") (concat org-roam-directory "confluent/" customer-slug))))

    (while (not (file-exists-p target))
      (message "target %s is not ready" target)
      )
    target))

(with-eval-after-load 'org-roam
  (cl-defmethod org-roam-node-backlinkscount ((node org-roam-node))
    (let* ((count (caar (org-roam-db-query
                         [:select (funcall count source)
                          :from links
                          :where (= dest $s1)
                          :and (= type "id")]
                         (org-roam-node-id node)))))
      (format "[%d]" count))))


(with-eval-after-load 'org-roam
  (cl-defmethod org-roam-node-doom-hierarchy-alias ((node org-roam-node))
    "Return hierarchy for NODE, constructed of its file title, OLP and direct title.
If some elements are missing, they will be stripped out. Copied from doom to add the real note title in the display
for alias nodes"
    (let ((title     (org-roam-node-title node))
          (olp       (org-roam-node-olp   node))
          (level     (org-roam-node-level node))
          ;; (filetitle (org-roam-node-doom-filetitle node))
          (filetitle (org-roam-node-file-title node))
          (separator (propertize org-eldoc-breadcrumb-separator 'face 'shadow)))
      (cl-case level
        ;; node is a top-level file
        (0 (if (string= title filetitle)
               filetitle
             (concat title (propertize (concat " (" filetitle ")") 'face '(shadow italic)))))
        ;; node is a level 1 heading
        (1 (concat (propertize filetitle 'face '(shadow italic))
                   separator title))
        ;; node is a heading with an arbitrary outline path
        (t (concat (propertize filetitle 'face '(shadow italic))
                   separator (propertize (string-join olp separator) 'face '(shadow italic))
                   separator title))))))



(eval-after-load 'org-roam
  '(cl-defmethod org-roam-node-slug ((node org-roam-node))
     "Return the slug of NODE. Custom override to use dashes `-' instead of underscores `_'"
     (let ((title (org-roam-node-title node))
           (slug-trim-chars '(;; Combining Diacritical Marks https://www.unicode.org/charts/PDF/U0300.pdf
                              768 ; U+0300 COMBINING GRAVE ACCENT
                              769 ; U+0301 COMBINING ACUTE ACCENT
                              770 ; U+0302 COMBINING CIRCUMFLEX ACCENT
                              771 ; U+0303 COMBINING TILDE
                              772 ; U+0304 COMBINING MACRON
                              774 ; U+0306 COMBINING BREVE
                              775 ; U+0307 COMBINING DOT ABOVE
                              776 ; U+0308 COMBINING DIAERESIS
                              777 ; U+0309 COMBINING HOOK ABOVE
                              778 ; U+030A COMBINING RING ABOVE
                              779 ; U+030B COMBINING DOUBLE ACUTE ACCENT
                              780 ; U+030C COMBINING CARON
                              795 ; U+031B COMBINING HORN
                              803 ; U+0323 COMBINING DOT BELOW
                              804 ; U+0324 COMBINING DIAERESIS BELOW
                              805 ; U+0325 COMBINING RING BELOW
                              807 ; U+0327 COMBINING CEDILLA
                              813 ; U+032D COMBINING CIRCUMFLEX ACCENT BELOW
                              814 ; U+032E COMBINING BREVE BELOW
                              816 ; U+0330 COMBINING TILDE BELOW
                              817 ; U+0331 COMBINING MACRON BELOW
                              )))
       (cl-flet* ((nonspacing-mark-p (char) (memq char slug-trim-chars))
                  (strip-nonspacing-marks (s) (string-glyph-compose
                                               (apply #'string
                                                      (seq-remove #'nonspacing-mark-p
                                                                  (string-glyph-decompose s)))))
                  (cl-replace (title pair) (replace-regexp-in-string (car pair) (cdr pair) title)))
         (let* ((pairs `(("[^[:alnum:][:digit:]]" . "-") ;; convert anything not alphanumeric
                         ("__*" . "_")                   ;; remove sequential underscores
                         ("^_" . "")                     ;; remove starting underscore
                         ("_$" . "")))                   ;; remove ending underscore
                (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs)))
           (downcase slug))))))

;; Speeds up org-roam by caching `org-roam-node-read--completions' which computes all the candidates
;; displayed each time you search for notes with `org-roam-node-find'. With the following trick, the result of
;; `org-roam-node-read--completions' is reused as long as the db has not changed.
;; Nice trick seen at:
;; - https://github.com/Konubinix/Devel/blob/30d0184db0a61f46ca35e102302d707fef964a8c/elfiles/config/after-loads/KONIX_AL-org-roam.el#L770-L787
;; - https://github.com/org-roam/org-roam/issues/2330
(defvar vic/org-roam-node-read--completions/cache nil "Memory cache of the list of nodes")
(defvar vic/org-roam-node-read--completions/cache-time nil "The time when the cache was last taken")
(defun vic/org-roam-node-read--completions (orig-fun &rest args)
  (when (or
         (not vic/org-roam-node-read--completions/cache)
         (not vic/org-roam-node-read--completions/cache-time)
         (time-less-p
          vic/org-roam-node-read--completions/cache-time
          (file-attribute-modification-time (file-attributes org-roam-db-location))
          ))
    (message "Computing the org-roam-node-read--completions")
    (setq vic/org-roam-node-read--completions/cache-time (current-time))
    (setq vic/org-roam-node-read--completions/cache (apply orig-fun args)))
  vic/org-roam-node-read--completions/cache)

(advice-add #'org-roam-node-read--completions :around #'vic/org-roam-node-read--completions)

(defun vic/get-url-title (url &optional desc)
  "Takes a URL and returns the value of the <title> HTML tag,
   Thanks to https://frozenlock.org/tag/url-retrieve/ for documenting url-retrieve
Based on https://gist.github.com/jmn/34cd4205fa30ccf83f94cb1bc0198f3f"
  (let ((buffer (url-retrieve-synchronously url))
        (title nil))
    (with-current-buffer buffer
      (goto-char (point-min))
      (search-forward-regexp "<title>\\([^<]+?\\)</title>")
      (setq title (match-string 1 ) )
      (kill-buffer (current-buffer)))
    title))

(provide 'vic-utils)
