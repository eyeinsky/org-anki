;;; org-anki.el --- Synchronize org-mode entries to Anki -*- lexical-binding: t -*-
;;
;; Copyright (C) 2021 Markus Läll
;;
;; URL: https://github.com/eyeinsky/org-anki
;; Version: 0.0.4
;; Author: Markus Läll <markus.l2ll@gmail.com>
;; Keywords: outlines, flashcards, memory
;; Package-Requires: ((emacs "24.4") (request "0.3.2"))

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; Minor mode to synchronize org-mode entries to Anki via AnkiConnect.
;;
;; See https://github.com/eyeinsky/org-anki for more.


;;; Code:

(require 'json)
(require 'org)
(require 'request)
(require 'org-element)
(require 'thunk)
(require 'cl-lib)
(require 'async-await)

;; Constants

(defconst org-anki-prop-note-id "ANKI_NOTE_ID")
(defconst org-anki-prop-deck "ANKI_DECK")

;; Customizable variables

(defcustom org-anki-default-deck nil
  "Default deck name if none is set on the org item nor as global
property"
  :type '(string)
  :group 'org-anki)

;; Stolen code

;; Get list of global properties
;;
;; From:
;;   https://emacs.stackexchange.com/questions/21713/how-to-get-property-values-from-org-file-headers
(defun org-anki--global-props (&optional name buffer)
  "Get the plists of global org properties by NAME in BUFFER.

Default NAME is \"PROPERTY\", default BUFFER the current buffer."
  (unless name (setq name "PROPERTY"))
  (with-current-buffer (or buffer (current-buffer))
    (org-element-map (org-element-parse-buffer) 'keyword
      (lambda (el) (when (string-match name (org-element-property :key el)) el))
      nil t)))

(defun org-anki--get-global-prop (name)
  "Get global property by NAME."
  (plist-get (car (cdr (org-anki--global-props name))) :value))


;; AnkiConnect API

(defun org-anki-connect-request (body on-result on-error)
  "Perform HTTP GET request to AnkiConnect's default http://localhost:8765.

BODY is the alist json payload, CALLBACK the function to call
with result."
  (let ((json (json-encode `(("version" . 6) ,@body))))
    (request
      "http://localhost:8765" ; This is where AnkiConnect add-on listens.
      :type "GET"
      :data json
      :headers '(("Content-Type" . "application/json"))
      :parser 'json-read

      :error
      (cl-function
       (lambda (&rest _args)
         (debug "Error response in variable '_args'")))

      :success
      (cl-function
       (lambda (&key data &allow-other-keys)
         (let ((the-error (assoc-default 'error data))
               (the-result (assoc-default 'result data)))
           (if the-error
               (if on-error
                   (funcall on-error the-error)
                 (error "Unhandled error: %s" the-error))
           (funcall on-result the-result))))))))

(defun org-anki--get-current-tags (id)
  (promise-new
   (lambda (resolve _reject)
     (org-anki-connect-request
      (org-anki--notes-info `(,id))
      (lambda (the-result)
        (let ((tags-v (assoc-default 'tags (aref the-result 0))))
          (funcall resolve (append tags-v nil))))
      (lambda (the-error) (reject the-error))))))

;; Note

(cl-defstruct note maybe-id front back tags deck point)

(defun org-anki--note-at-point ()
  (let
      ((front (org-anki--string-to-html (org-entry-get nil "ITEM")))
       (note-start (point))
       (back (org-anki--string-to-html (org-anki--entry-content-until-any-heading)))
       (tags (org-anki--get-tags))
       (deck (org-anki--find-deck))
       (maybe-id (org-entry-get nil org-anki-prop-note-id)))
    (make-note
     :front    front
     :tags     tags
     :maybe-id maybe-id
     :deck     deck
     :back     back
     :point    note-start)))

;;; JSON payloads

(defun org-anki--body (action params)
  "Wrap ACTION and PARAMS to a json payload AnkiConnect expects."
  `(("action" . ,action)
    ("params" . ,params)))

(defun org-anki--create-note-single (note)
  "Create an `addNote' json structure to be added to DECK with
card FRONT and BACK strings."
  (org-anki--body
   "addNote"
   `(("note" .
      (("deckName" . ,(note-deck note))
       ,@(org-anki--to-fields (note-front note) (note-back note))
       ("tags" . ,(if (note-tags note) (note-tags note) ""))
       ("options" .
        (("allowDuplicate" . :json-false)
         ("duplicateScope" . "deck"))))))))

(defun org-anki--update-note-single (id new-front new-back)
  "Create an `updateNoteFields' json structure with integer ID,
and NEW-FRONT and NEW-BACK strings."
  (org-anki--body
   "updateNoteFields"
   `(("note" .
      (("id" . ,id)
       ,@(org-anki--to-fields new-front new-back))))))

(defun org-anki--update-note (note)
  (let*
      ((current (await (org-anki--get-current-tags (note-maybe-id note))))
       (remove (cl-set-difference current (note-tags note) :test #'equal))
       (add (cl-set-difference (note-tags note) current :test #'equal)))

    (org-anki--multi
     `(,(org-anki--update-note-single (note-maybe-id note) (note-front note) (note-back note))
       ,(org-anki--remove-tags (note-maybe-id note) remove)
       ,(org-anki--add-tags    (note-maybe-id note) add)))))

(defun org-anki--to-fields (front back)
  "Convert org item title FRONT and content BACK to json fields
sent to AnkiConnect. If FRONT contains Cloze syntax then both the
question and answer are generated from it, and BACK is ignored."
  (cond
   ((org-anki--is-cloze front)
    `(("modelName" . "Cloze")
      ("fields" . (("Text" . ,front)))))
   ((org-anki--is-cloze back)
    `(("modelName" . "Cloze")
      ("fields" . (("Text" . ,back)))))
   (t
    `(("modelName" . "Basic")
      ("fields" . (("Front" . ,front) ("Back" . ,back)))))))

(defun org-anki--delete-notes (ids)
  "Create an `deleteNotes' json structure with integer IDS list."
  (org-anki--body "deleteNotes" `(("notes" . ,ids))))

(defun org-anki--multi (actions)
  (org-anki--body "multi" `(("actions" . ,actions))))

(defun org-anki--notes-info (note-ids)
  (org-anki--body "notesInfo" `(("notes" . ,note-ids))))

(defun org-anki--add-tags (note-id tags)
  (let ((tags_ (mapconcat 'identity tags " ")))
  (org-anki--body "addTags" `(("notes" ,note-id) ("tags" . ,tags_)))))

(defun org-anki--remove-tags (note-id tags)
  (let ((tags_ (mapconcat 'identity tags " ")))
    (org-anki--body "removeTags" `(("notes" ,note-id) ("tags" . ,tags_)))))

;; org-mode

(defun org-anki--entry-content-until-any-heading ()
  "Get entry content until any next heading."
  ;; We move around with regexes, so restore original position
  (save-excursion
    ;; Jump to beginning of entry
    (goto-char (org-entry-beginning-position)) ;; was: (re-search-backward "^\\*+ .*\n")
    ;; Skip heading
    (re-search-forward ".*\n")
    ;; Possibly skip property block until end of entry
    (re-search-forward ":properties:\\(.*\n\\)*:end:" (org-entry-end-position) t)
    ;; Get entry content
    (let ((from (point))
          (to (progn (outline-next-visible-heading 1) (point))))
      (buffer-substring-no-properties from to)
      )))

(defun org-anki--string-to-html (string)
  "Convert STRING (org element heading or content) to html."
  (org-export-string-as string 'html t '(:with-toc nil)))

(defun org-anki--report-error (format error)
  "FORMAT the ERROR and prefix it with `org-anki error'."
  (let ((fmt0 (concat "org-anki error: " format)))
    (message fmt0 error)))

(defun org-anki--find-deck ()
  (thunk-let
   ((prop-item (org-entry-get nil org-anki-prop-deck))
    (prop-global (org-anki--get-global-prop org-anki-prop-deck)))
    (cond
     ((stringp prop-item) prop-item)
     ((stringp prop-global) prop-global)
     ((stringp org-anki-default-deck) org-anki-default-deck)
     (t (error "No deck name in item nor file nor set as default deck!")))))

(defun org-anki--get-tags ()
  (let ((tags (org-entry-get nil "TAGS")))
    (cond
     (tags (split-string tags ":" t))
     (t nil))))

;;; Cloze

(defun org-anki--is-cloze (text)
  "Check if TEXT has cloze syntax, return nil if not."
  ;; Check for something similar to {{c1::Hidden-text::Hint}} in TEXT
  (if (string-match "{{c[0-9]+::\\([^:\}]*\\)::\\([^:\}]*\\)}}" text)
      "Cloze"
    nil))

;; Stolen from https://github.com/louietan/anki-editor
(defun org-anki--region-to-cloze (begin end arg hint)
  "Cloze region from BEGIN to END with number ARG."
  (let ((region (buffer-substring begin end)))
    (save-excursion
      (delete-region begin end)
      (insert (with-output-to-string
                (princ (format "{{c%d::%s" (or arg 1) region))
                (unless (string-blank-p hint) (princ (format "::%s" hint)))
                (princ "}}"))))))

;; Interactive commands

;;;###autoload
(defun org-anki-sync-entry ()
  "Synchronize single entry.

Tries to add, or update if id property exists, the note."

  (interactive)
  (let ((note (org-anki--note-at-point)))
    (cond
     ;; id property exists, update
     ((note-maybe-id note)
      (funcall
       (async-lambda ()
         (org-anki-connect-request
          (org-anki--multi (await (org-anki--update-note note)))
          (lambda (_) (message "org-anki says: note succesfully updated!"))
          (lambda (the-error)
            (org-anki--report-error
             "Couldn't update note, received: %s"
             the-error))))))

     ;; id property doesn't exist, try to create new
     (t
      (org-anki-connect-request
       (org-anki--create-note-single note)
       (lambda (the-result)
         (org-set-property org-anki-prop-note-id (number-to-string the-result))
         (message "org-anki says: note succesfully added!"))
       (lambda (the-error) (org-anki--report-error "Couldn't add note, received error: %s" the-error)))))))

;;;###autoload
(defun org-anki-delete-entry ()
  "Delete org entry under cursor (the id property must exist).

Will lose scheduling data so be careful"
  (interactive)
  (let*
      ((note-id (string-to-number (org-entry-get nil org-anki-prop-note-id))))
    (org-anki-connect-request
     (org-anki--delete-notes `(,note-id))
     (lambda (the-result)
       (org-delete-property org-anki-prop-note-id)
       (message "org-anki says: note successfully deleted!"))
     (lambda (the-error)
       (error "Couldn't delete note, received error: %s" the-error)))))

;; Stolen from https://github.com/louietan/anki-editor
;;;###autoload
(defun org-anki-cloze-dwim (&optional arg hint)
  "Convert current active region or word under cursor to Cloze
syntax."
  (interactive "p\nsHint (optional): ")
  (cond
   ((region-active-p)
    (org-anki--region-to-cloze (region-beginning) (region-end) arg hint))
   ((thing-at-point 'word)
    (let ((bounds (bounds-of-thing-at-point 'word)))
      (org-anki-cloze (car bounds) (cdr bounds) arg hint)))
   (t (error "Nothing to create cloze from"))))

(provide 'org-anki)
;;; org-anki.el ends here
