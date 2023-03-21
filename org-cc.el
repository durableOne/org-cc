;;; Package: org-cc.el - Context clues for Org projects
(require 'cl-lib)

(defcustom org-cc-directory (concat org-directory "org-cc") "Directory in which the context clue files will be stored.")
(defcustom org-cc-days 14 "Number of days since last work to trigger display of context clues.")

;;;###autoload
(defun org-cc-make-or-get-note-dir (&optional make-dir)
  "Creates or retrieves the ID for heading at point and uses it to create
   a subdirectory in the style of org-attach under org-cc-directory to
   store context clues for this heading. If the argument evaluates to t,
   will create the directory as well."
  (let ((target-dir (concat org-cc-directory
                            "/"
                            (org-attach-id-uuid-folder-format (org-id-get-create)))))
    (if make-dir
        (make-directory target-dir t))
    target-dir))

;;;###autoload
(defun org-cc-edit-cc-file ()
  "Opens the current heading's context clues file."
  (interactive)
  (find-file (concat (org-cc-make-or-get-note-dir t) "/cc.org")))

;;;###autoload
(defun org-cc-get-logbook-notes ()
  "Retrieve the contents of the LOGBOOK drawer"
  (save-excursion
    (unless (org-at-heading-p)
      (outline-previous-heading))
    (when (re-search-forward ":LOGBOOK:" (save-excursion
                                           (outline-next-heading)
                                           (point))
                             t)
      (let* ((elt (org-element-property-drawer-parser nil))
             (beg (org-element-property :contents-begin elt))
             (end (org-element-property :contents-end elt)))
        (buffer-substring-no-properties beg end)))))

;;;###autoload
(defun org-cc-get-time-since-last-work ()
  "Returns the time since you last clocked in on the heading at point.
   Returns nil if it can't find clocking data."
  (let ((logbook (org-get-logbook-notes)))
    (if logbook
        (save-match-data
          (string-match "^CLOCK: \\(.*--\\[.*\\)$" logbook)
          (let ((clock (match-string 1 logbook))) 
            (if clock
                (progn
                  ;; Match --[timestamp] to retrieve the last time you clocked out.
                  (string-match "--\\[\\([^\]]+\\)\\]" clock)
                  (org-time-stamp-to-now (match-string 1 clock)))
              nil)))
      nil)))

(defun org-cc--get-heading-notes-file-maybe ()
  "Return the context clues file for heading at point if it exists, nil otherwise."
  (let ((current-level-file (concat (org-cc-make-or-get-note-dir) "/cc.org")))
    (if (file-exists-p current-level-file)
        current-level-file
      nil)))

(defun org-cc--get-heading ()
  (org-no-properties (org-get-heading t t t t)))

;;;###autoload
(defun org-cc-get-notes-files ()
  "Returns a list of pairs (heading . heading's notes file) for the heading at point and all its parents.
   The list goes from the topmost parent down to the current heading."
  (let ((result (list (cons (org-cc--get-heading) (org-cc--get-heading-notes-file-maybe)))))
    (save-excursion (while (org-get-outline-path)
                      (outline-up-heading 1)
                      (let ((one-level-up (org-cc--get-heading-notes-file-maybe)))
                        (setf result (cons (cons (org-cc--get-heading) one-level-up) result)))))
    result))

;;;###autoload
(defun org-cc-display-notes (&optional no-time-check)
  "Retrieve all the notes for this heading and its parents and concat them to a buffer"
  (interactive "P")
  (let ((last-worked (org-cc-get-time-since-last-work))
        (no-time-check (or no-time-check nil))) 
    (when (or
           no-time-check
           (not last-worked)
           (<= last-worked (- org-cc-days)))
      (let ((buffer (get-buffer-create "*Org Context Clues*"))
            (headings-and-contents (org-cc-get-notes-files)))
        (unless (cl-every 'null (mapcar 'cdr headings-and-contents))
          (switch-to-buffer-other-window buffer)
          (kill-region (point-min) (point-max))
          (org-mode)
          (insert "#+title: Org Context Clues\n\n")
          (cl-labels ((insert-heading-and-contents (heading-and-contents level)
                        ;; Collect notes for all headings and insert them into the buffer
                        ;; Recursively cdrs down the list produced by org-cc-get-notes-files
                        (when (not (null heading-and-contents))
                          (let ((first-entry (car heading-and-contents)))
                            (insert (concat (s-repeat level "*")
                                            " "
                                            (car first-entry)
                                            "\n"))
                            (when (cdr first-entry)
                              (insert (with-temp-buffer (insert-file-contents (cdr first-entry))
                                                        (buffer-string)))))
                          (insert-heading-and-contents (cdr heading-and-contents) (1+ level)))))
            (insert-heading-and-contents headings-and-contents 1)))))))

(provide 'org-cc)
