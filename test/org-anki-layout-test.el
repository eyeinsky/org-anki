;;; org-anki-layout-test.el -*- lexical-binding: t; -*-
(require 'ert)
(require 'ox-html)
(require 'org-anki)

(defmacro org-anki-test--with-field-layout (&rest body)
  `(let ((org-anki-import-note-format 'fields)) ,@body))

(ert-deftest org-anki-import-basic-always-uses-field-headings ()
  (org-anki-test--with-field-layout
   (with-temp-buffer
     (org-mode)
     (org-anki--write-note
      (make-org-anki--note :maybe-id 42 :type "Basic"
                           :fields '(("Front" . "one line") ("Back" . ""))))
     (goto-char (point-min))
     (should (equal (org-entry-get nil "ANKI_NOTE_TYPE") "Basic"))
     (should (equal (org-map-entries (lambda () (org-entry-get nil "ITEM")))
                    '("42" "Front" "Back"))))))

(ert-deftest org-anki-cloze-fields-round-trip-with-html-export ()
  (org-anki-test--with-field-layout
   (let ((org-anki-model-fields '(("Cloze" "Text" "Extra")))
         (org-anki-media-method 'http))
     (cl-letf (((symbol-function 'org-anki-connect-request)
		(lambda (_body success _failure &rest _)
                  (funcall success ["Text" "Extra"]))))
       (with-temp-buffer
         (org-mode)
         (insert "#+ANKI_DECK: Deck\n\n")
         (org-anki--write-note
          (make-org-anki--note :maybe-id 42 :type "Cloze"
                               :fields '(("Text" . "A {{c1::cloze}}")
                                         ("Extra" . "Extra text"))))
         (goto-char (point-min))
         (outline-next-heading)
         (let* ((note (org-anki--note-at-point))
		(fields (org-anki--note-fields note)))
           (should (equal (org-anki--note-type note) "Cloze"))
           (should (string-match-p "{{c1::cloze}}" (cdr (assoc "Text" fields))))
           (should (string-match-p "Extra text" (cdr (assoc "Extra" fields))))))))))

(ert-deftest org-anki-field-content-headings-stay-in-field ()
  (org-anki-test--with-field-layout
   (let ((content "* Heading\nBody\n** Child\n#+begin_src org\n,* literal\n#+end_src"))
     (with-temp-buffer
       (org-mode)
       (org-anki--write-note
	(make-org-anki--note :maybe-id 42 :type "Basic"
                             :fields `(("Front" . ,content) ("Back" . "Answer"))))
       (should (equal (org-map-entries (lambda () (org-current-level)))
                      '(1 2 3 4 2)))
       (should (string-match-p "^,\\* literal$" (buffer-string)))))))

(ert-deftest org-anki-import-converts-html-with-pandoc ()
  (org-anki-test--with-field-layout
   (skip-unless (executable-find "pandoc"))
   (let ((org-anki-html-to-org #'org-anki--html-to-org-via-tempfile))
     (with-temp-buffer
       (org-mode)
       (org-anki--write-note
	(org-anki--parse-note
         '((noteId . 42) (modelName . "Basic")
           (fields . ((Front (value . "<h1>Heading</h1><p>Some <strong>bold</strong> text</p>")
                             (order . 0))
                      (Back (value . "") (order . 1)))))
         "Deck"))
       (should (equal (org-map-entries (lambda () (org-current-level))) '(1 2 3 2)))
       (should (string-match-p "Some \\*bold\\* text" (buffer-string)))))))

(ert-deftest org-anki-compact-layout-remains-default ()
  (let ((org-anki-import-note-format 'compact))
    (with-temp-buffer
      (org-mode)
      (org-anki--write-note
       (make-org-anki--note :maybe-id 42 :type "Basic"
                            :fields '(("Front" . "Question") ("Back" . "Answer"))))
      (goto-char (point-min))
      (should (equal (org-entry-get nil "ITEM") "Question"))
      (should (equal (org-map-entries (lambda () (org-current-level))) '(1))))))

(ert-deftest org-anki-bulk-sync-skips-fields-and-their-descendants ()
  (let ((org-anki-import-note-format 'fields)
        (org-anki-model-fields '(("Custom" "Question" "Answer" "Extra")))
        collected)
    (cl-letf (((symbol-function 'org-anki--org-to-html) #'string-trim)
              ((symbol-function 'org-anki--sync-notes)
               (lambda (notes) (setq collected notes))))
      (with-temp-buffer
        (org-mode)
        (insert "#+ANKI_DECK: Deck\n")
        (dotimes (i 2)
          (org-anki--write-note
           (make-org-anki--note :maybe-id (1+ i) :type "Custom"
                                :fields '(("Question" . "Question")
                                          ("Answer" . "* Nested\nAnswer")
                                          ("Extra" . "")))))
        (org-anki-sync-all)
        (should (= (length collected) 2))
        (dolist (note collected)
          (should (= (length (org-anki--note-fields note)) 3))
          (should (equal (cdr (assoc "Extra" (org-anki--note-fields note))) "")))))))
