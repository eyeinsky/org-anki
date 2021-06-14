;;; org-anki.el --- Synchronize org-mode entries to Anki -*- lexical-binding: t -*-
;;
;; Copyright (C) 2020 Markus Läll
;;
;; URL: https://github.com/eyeinsky/org-anki
;; Version: 0.0.1
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

;;;; Constants

(defconst org-anki-prop-note-id "ANKI_NOTE_ID")
(defconst org-anki-prop-note-type "ANKI_NOTE_TYPE")
(defconst org-anki-prop-deck "ANKI_DECK")

;;;; Variables

(defvar org-anki-get-note-type-hook '(org-anki--check-cloze
                                      org-anki--check-basic)
  "List of functions to call when no note types are specified, take
the front and the back of a card as argument, must return a note type
in string, the string in turn, will be used as a key to an alist
`org-anki-get-fields-note' that get the \"fields\" of the note.

See the documentation of `org-anki-get-fields-note' or `org-anki--check-cloze'
for example.")

;;;; Customizable variables

(defcustom org-anki-default-deck nil
  "Default deck name if none is set on the org item nor as global property"
  :type '(string)
  :group 'org-anki)


(defcustom org-anki-get-fields-note '(("Cloze" . org-anki--get-fields-Cloze-type)
                                      ("Basic" . org-anki--get-fields-Basic-type))
  "An alist of functions to call that return the \"fields\" of the
note based on the front and the back of a card 
(the heading and the content of an org element essentially)."
  :type '(alist :key-type (regexp :tag "Note type")
                :value-type (function :tag "Get fields functions")))

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


;;;; Talk to AnkiConnect API

(defun org-anki-connect-request (body callback)
  "Perform HTTP GET request to AnkiConnect's default http://localhost:8765.

BODY is the alist json payload, CALLBACK the function to call with result."
  (request
    "http://localhost:8765" ; This is where AnkiConnect add-on listens.
    :type "GET"
    :data (json-encode body)
    :sync t
    :headers '(("Content-Type" . "application/json"))
    :parser 'json-read

    :error
    (cl-function
     (lambda (&rest _args)
       (debug "Error response in variable '_args'")))

    :success
    (cl-function
     (lambda (&key data &allow-other-keys)
       (funcall callback data)))))

(defun org-anki--body (action params)
  "Wrap ACTION and PARAMS to a json payload AnkiConnect expects."
  `(("version" . 6)
    ("action" . ,action)
    ("params" . ,params)))

(defun org-anki--create-note (id front back deck type)
  "Create an `addNote' json structure to be added to DECK with card FRONT and BACK strings."
  (org-anki--body
   "addNote"
   `(("note" .
      ,(org-anki--anki-connect-map-note id front back deck type)))))
   
(defun org-anki--update-note (id new-front new-back)
  "Create an `updateNoteFields' json structure with integer ID, and NEW-FRONT and NEW-BACK strings."
  (org-anki--body
   "updateNoteFields"
   (list (cons "note" (org-anki--anki-connect-map-note id new-front new-back)))))
   ;; `(("note" .
   ;;    (("id" . ,id)
   ;;     ("deckName" . "org-mode")
   ;;     ("fields" . (("Front" . ,new-front) ("Back" . ,new-back))))))))

(defun org-anki--delete-notes (ids)
  "Create an `deleteNotes' json structure with integer IDS list."
  (org-anki--body "deleteNotes" `(("notes" . ,ids))))


;;;; Get card content from org-mode:

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

(defun org-anki--anki-connect-map-note (id front back &optional deck type)
  "Return the parameters appropriate for an anki note from FRONT and BACK.

When ID is non-nil, return only the \"fields\" and \"id\".
The optional TYPE if non-nil, is used to determine which function in
`org-anki-check-note-type-hook' to call to get the \"fields\" of
the note, user can specify TYPE by the value of
`org-anki-prop-note-type' key in the note entry.

The return value is used by `org-anki--create-note' and `org-anki--body'."
  (let* ((note-type (or type (run-hook-with-args-until-success
                              'org-anki-get-note-type-hook front back)))
         (note-fn (alist-get note-type org-anki-get-fields-note
                             nil nil 'string=))
         (fields-params (funcall note-fn front back)))
    (if (not note-fn)
        (org-anki--report-error "%s not supported." note-type)
      (if id
          `(("id" . ,(string-to-number id))
            ("fields" . ,fields-params))
        `(("deckName" .  ,deck)
          ("modelName" . ,note-type)
          ("fields" . ,fields-params)
          ("options" .
           (("allowDuplicates" . :json-false)
            ("duplicateScope" . "deck"))))))))

;;    (("deckName" . ,deck)
;;     ("modelName" . "Basic")
;;     ("fields" .
;;      (("Front" . ,front)
;;       ("Back" . ,back)))
;;     ("options" .
;;      (("allowDuplicate" . :json-false)
;;       ("duplicateScope" . "deck"))))))))

(defun org-anki--check-cloze (front back)
  "Check if FRONT has an appropriate cloze syntax, if not, return nil."
  ;; Check for something similar to {{c1::Hidden-text::Hint}} in FRONT
  (if (string-match "{{c[0-9]+::\\([^:\}]*\\)::\\([^:\}]*\\)}}" front)
      "Cloze"                           ; Yes, Return "Cloze"
    ;; No, send a message then return nil.
    (org-anki--report-error "No cloze in %s, it's a Basic note."
                            ;; Remove newline from htmlized FRONT
                            (replace-regexp-in-string "\n" "" front))
    nil))

(defun org-anki--check-basic (front back)
  "Always return \"Basic\" for `org-anki-get-fields-note', this is
used as a fallback when other functions in
`org-anki-get-note-type-hook' fails, as in, return nil."
"Basic")

(defun org-anki--get-fields-Cloze-type (front back)
  "Ignore BACK, return FRONT as \"Text\" params for fields."
  (list (cons "Text" front)))

(defun org-anki--get-fields-Basic-type (front back)
  "Return FRONT as \"Front\" and BACK as \"Back\" params for fields."
  (list (cons "Front" front)
        (cons "Back" back)))

;;;; Public API, i.e commands what the org-anki user should use:

;;;###autoload
(defun org-anki-sync-entry ()
  "Synchronize single entry.
Tries to add, or update if id property exists, the note."

  (interactive)
  (let* ((front    (org-anki--string-to-html (org-entry-get nil "ITEM")))
         (maybe-id (org-entry-get nil org-anki-prop-note-id))
         (deck     (org-anki--find-deck))
         (back     (org-anki--string-to-html (org-anki--entry-content-until-any-heading)))
         (type     (org-entry-get nil org-anki-prop-note-type)))
    (cond
     ;; id property exists, update
     (maybe-id
      (org-anki-connect-request
       (org-anki--update-note maybe-id front back)
       (lambda (arg)
         (let ((the-error (assoc-default 'error arg)))
           (if the-error
               (org-anki--report-error
                "Couldn't update note, received: %s"
                the-error)
             (message "org-anki says: note succesfully updated!"))))))
     ;; id property doesn't exist, try to create new
     (t
      (org-anki-connect-request
       (org-anki--create-note maybe-id front back deck type)
       (lambda (arg)
         (let ((the-error (assoc-default 'error arg))
               (the-result (assoc-default 'result arg)))
           (cond
            (the-error
             (org-anki--report-error
              "Couldn't add note, received error: %s"
              the-error))
            (the-result
             (org-set-property org-anki-prop-note-id (number-to-string the-result))
             (message "org-anki says: note succesfully added!"))
            (t
             (org-anki--report-error "%s"
              "Empty response, it should return new note's id."))))))))))

;;;###autoload
(defun org-anki-delete-entry ()
  "Delete org entry under cursor (the id property must exist).

Will lose scheduling data so be careful"
  (interactive)
  (let*
      ((note-id (string-to-number (org-entry-get nil org-anki-prop-note-id))))
    (org-anki-connect-request
     (org-anki--delete-notes `(,note-id))
     (lambda (arg)
       (let ((the-error (assoc-default 'error arg)))
         (cond
          (the-error
           (error "Couldn't delete note, received error: %s" the-error))
          (t
           (org-delete-property org-anki-prop-note-id)
           (message "org-anki says: note successfully deleted!"))))))))


;;;; Helpers for development, don't use

(defun org-anki--sync-entry-debug ()
  "Debug command which reloads package before running."
  (interactive)
  (message "org-anki-sync-entry-debug")
  (eval-buffer "org-anki.el")
  (org-anki-sync-entry))

(defun org-anki--delete-entry-debug ()
  "Debug command which reloads package before running."
  (interactive)
  (message "org-anki-delete-entry-debug")
  (eval-buffer "org-anki.el")
  (org-anki-delete-entry))

(defun org-anki--debug-bind ()
  "Define keys for testing."
  (define-key org-mode-map (kbd "C-c C-c") 'org-anki--sync-entry-debug)
  (define-key org-mode-map (kbd "C-d C-d") 'org-anki--delete-entry-debug))

(provide 'org-anki)
;;; org-anki.el ends here
