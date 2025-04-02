;;; votd.el --- Bible Verse Of The Day in Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2025 Kristjon Ciko

;; Author: Kristjon Ciko <kristjoc@uio.no>
;; Keywords: bible verse
;; Homepage: https://github.com/kristjoc/votd
;; Package-Requires: ((emacs "29.1"))
;; Package-Version: 0.5

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; votd is a simple Emacs package that fetches the Bible verse of the
;; day from https://www.biblegateway.com/ and formats it to be used as
;; an emacs-dashboard footer or *scratch* buffer message in Emacs.

;;; Code:

(require 'url)
(require 'json)

(defgroup votd nil
  "Package that fetches the Bible verse of the day from BibleGateway."
  :group 'external)

(defcustom votd-bible-version "KJV"
  "The Bible version, default KJV."
  :type 'string
  :group 'votd)

(defcustom votd-text-width 80
  "The width the votd body in number of characters."
  :type 'integer
  :group 'votd)

(defcustom votd-fallback-verse "For God so loved the world,
that he gave his only begotten Son,
that whosoever believeth in him should not perish,
but have everlasting life."
  "The fallback verse to use when the online request fails."
  :type 'string
  :group 'votd)

(defcustom votd-fallback-reference "John 3:16"
  "The reference for the fallback verse."
  :type 'string
  :group 'votd)

(defcustom votd-request-timeout 1
  "The timeout for the URL request in seconds."
  :type 'integer
  :group 'votd)

(defun split-with-spaces (str)
  "Split STR preserving original spacing between words."
  (let ((parts nil)
        (start 0)
        (len (length str)))
    (while (< start len)
      (if (string-match "\\([^ ]+\\)\\( *\\)" str start)
          (progn
            (push (match-string 1 str) parts)
            (push (match-string 2 str) parts)
            (setq start (match-end 0)))
        (setq start len)))
    (nreverse (if (string-match-p " $" str)
                  parts
                (butlast parts)))))

(defun justify-line (line width)
  "Justify LINE to WIDTH characters."
  (let* ((words (split-string line))
         (word-count (length words)))
    (if (<= word-count 1)
        line  ; Return single words unchanged
      (let* ((total-word-length (apply #'+ (mapcar #'length words)))
             (spaces-needed (- width total-word-length))
             (gaps (1- word-count))
             (base-spaces-per-gap (/ spaces-needed gaps))
             (extra-spaces (% spaces-needed gaps))
             (result ""))
        ;; Distribute spaces as evenly as possible
        (dotimes (i (1- word-count))
          (setq result
                (concat result
                        (nth i words)
                        (make-string
                         (if (< i extra-spaces)
                             (1+ base-spaces-per-gap)
                           base-spaces-per-gap)
                         ?\s))))
        (concat result (car (last words)))))))

(defun format-verse-text (text &optional width)
  "Format verse TEXT as a justified paragraph with optional WIDTH."
  (when text  ; Only process if text is not nil
    (with-temp-buffer
      (let* ((fill-column (or width votd-text-width))
             (lines '()))
        ;; First fill the paragraph normally
        (insert text)
        (fill-region (point-min) (point-max))
        ;; Collect lines
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (string-trim (buffer-substring (line-beginning-position)
                                                   (line-end-position)))))
            (unless (string-empty-p line)
              (push line lines)))
          (forward-line 1))
        ;; Justify each line except the last one
        (setq lines (nreverse lines))
        (let* ((justified-lines
                (append
                 (mapcar (lambda (line)
                           (justify-line line fill-column))
                         (butlast lines))
                 (last lines)))
               (result (string-join justified-lines "\n")))
          result)))))

(defun decode-html-entities (text)
  "Decode HTML entities in TEXT."
  (when text
    (let ((entity-map '(("&ldquo;" . "\"")
                        ("&rdquo;" . "\"")
                        ("&#8212;" . "--")
                        ("&#8217;" . "'")
                        ("&#8220;" . "\"")
                        ("&#8221;" . "\"")))
          (decoded text))
      (dolist (entity entity-map)
        (setq decoded (replace-regexp-in-string (car entity) (cdr entity) decoded)))
      decoded)))

(defun fetch-daily-bible-verse ()
  "Fetch the daily Bible verse from BibleGateway API."
  (let ((url-request-method "GET")
        (url-queue-timeout votd-request-timeout)
        (url (concat "https://www.biblegateway.com/votd/get/?format=json&version=" votd-bible-version)))
    (condition-case nil
        (with-current-buffer (url-retrieve-synchronously url t t)
          (goto-char (point-min))
          (when (search-forward "\n\n" nil t)
            (let* ((json-string (buffer-substring-no-properties (point) (point-max)))
                   (json-object-type 'hash-table)
                   (json-array-type 'list)
                   (json-key-type 'string)
                   (json-data (json-read-from-string json-string))
                   (votd (gethash "votd" json-data))
                   (raw-text (gethash "text" votd))
                   (verse-text (decode-html-entities raw-text))
                   (clean-verse (replace-regexp-in-string "[\"]" "" verse-text))
                   (formatted-verse (format-verse-text clean-verse))
                   (verse-reference (gethash "display_ref" votd))
                   (fill-width votd-text-width))
              (format "%s\n%s" 
                      formatted-verse 
                      (let ((ref-text verse-reference))
                        (concat (make-string (- fill-width (length ref-text)) ?\s)
                                ref-text))))))
      (error
       (format "%s\n%s"
               (format-verse-text votd-fallback-verse)
               (let ((ref-text votd-fallback-reference))
                 (concat (make-string (- votd-text-width (length ref-text)) ?\s)
                         ref-text)))))))

(defun get-votd ()
  "Get the daily verse and handle errors."
  (condition-case err
      (fetch-daily-bible-verse)
    (error
     (format "Today's verse could not be fetched: %s" (error-message-string err)))))

(provide 'votd)
;;; votd.el ends here
