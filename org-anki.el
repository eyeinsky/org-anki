;;; org-anki.el --- synchronize org-mode entries to Anki

;; URL: https://github.com/eyeinsky/org-anki
;; Version: 0.0.1
;; Author: Markus Läll <markus.l2ll@gmail.com>
;; Keywords: outlines, flashcards, memory
;; Package-Requires: ((emacs "24"))

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

;; Constants

(defconst anki-prop-note-id "ANKI_NOTE_ID")
(defconst anki-prop-deck "ANKI_DECK")


;; Stolen code

;; Get list of global properties
;;
;; From:
;;   https://emacs.stackexchange.com/questions/21713/how-to-get-property-values-from-org-file-headers
(defun org-global-props (&optional property buffer)
  "Get the plists of global org properties of current buffer."
  (unless property (setq property "PROPERTY"))
  (with-current-buffer (or buffer (current-buffer))
    (org-element-map (org-element-parse-buffer) 'keyword
      (lambda (el) (when (string-match property (org-element-property :key el)) el))
      nil t)))

(defun anki--get-global-prop (key) (plist-get (car (cdr (org-global-props key))) :value))


;; Talk to AnkiConnect API:

(defun anki-connect-request (body callback)
  (message (json-encode body))
  (request
    "http://localhost:8765" ; This is where AnkiConnect add-on listens.
    :type "GET"
    :data (json-encode body)
    :sync t
    :headers '(("Content-Type" . "application/json"))
    :parser 'json-read

    :error
    (cl-function
     (lambda (&rest args)
       (debug "Error response in variable 'args'")
       ))

    :success
    (cl-function
     (lambda (&key data &allow-other-keys)
       (funcall callback data)))))

(defun anki--body (action params)
  `(("version" . 6)
    ("action" . ,action)
    ("params" . ,params)))

(defun anki--create-note (front back deck)
  (anki--body
   "addNote"
   `(("note" .
      (("deckName" . ,deck)
       ("modelName" . "Basic")
       ("fields" .
        (("Front" . ,front)
         ("Back" . ,back)))
       ("options" .
        (("allowDuplicate" . :json-false)
         ("duplicateScope" . "deck"))))))))

(defun anki--update-note (id new-front new-back)
  (anki--body
   "updateNoteFields"
   `(("note" .
      (("id" . ,id)
       ("deckName" . "org-mode")
       ("fields" . (("Front" . ,new-front) ("Back" . ,new-back))))))))

(defun anki--delete-notes (ids)
  (anki--body "deleteNotes" `(("notes" . ,ids))))


;; Get card content from org-mode:

;; Entry content: until any next heading
(defun anki--entry-content-until-any-heading ()
  ;; We move around with regexes, so restore original position
  (save-excursion
    ;; Jump to beginning of entry
    (goto-char (org-entry-beginning-position)) ;; was: (re-search-backward "^\\*+ .*\n")
    ;; Skip heading
    (re-search-forward ".*\n")
    ;; Possibly skip property block until end of entry
    (re-search-forward ":properties:\\(.*\n\\)*:end:" (org-entry-end-position) t)
    ;; Get entry content
    (buffer-substring-no-properties (point) (re-search-forward "\\([^*].*\n\\)*" nil t))
    ))


;; Public API, i.e commands what the org-anki user should use:

;; Sync single entry
(defun anki-sync-entry ()
  (interactive)
  (let* ((front    (org-entry-get nil "ITEM"))
         (maybe-id (org-entry-get nil anki-prop-note-id))
         (deck     (anki--get-global-prop anki-prop-deck))
         (back     (anki--entry-content-until-any-heading)))

    (cond
     ;; id property exists, update
     (maybe-id
      (anki-connect-request
       (anki--update-note maybe-id front back)
       (lambda (arg)
         (let ((the-error (assoc-default 'error arg)))
           (when the-error
             (message
              "Couldn't update note, received error: %s"
              the-error))))))
     ;; id property doesn't exist, try to create new
     (t
      (anki-connect-request
       (anki--create-note front back deck)
       (lambda (arg)
         (let ((the-error (assoc-default 'error arg))
               (the-result (assoc-default 'result arg)))
           (cond
            (the-error
             (message "Couldn't add note, received error: %s" the-error))
            (the-result
             (org-set-property anki-prop-note-id (number-to-string the-result)))
            (t (message "Empty response"))))))))))

;; Delete entry under cursor -- will lose history so be careful
(defun anki-delete-entry ()
  (interactive)
  (let*
      ((note-id (string-to-number (org-entry-get nil anki-prop-note-id))))
    (anki-connect-request
     (anki--delete-notes `(,note-id))
     (lambda (arg)
       (let ((the-error (assoc-default 'error arg)))
         (cond
          (the-error
           (message "Couldn't delete note, received error: %s" the-error))
          (t
           (org-delete-property anki-prop-note-id))))))))


;; Helpers for development, don't use

(defun anki--sync-entry-debug ()
  (interactive)
  (message "anki-sync-entry-debug")
  (eval-buffer "org-anki.el")
  (anki-sync-entry)
  )

(defun anki--delete-entry-debug ()
  (interactive)
  (message "anki-delete-entry-debug")
  (eval-buffer "org-anki.el")
  (anki-delete-entry)
  )

(defun anki--debug-bind ()
  (define-key org-mode-map (kbd "C-c C-c") 'anki--sync-entry-debug)
  (define-key org-mode-map (kbd "C-d C-d") 'anki--delete-entry-debug)
  )

(provide 'org-anki)
