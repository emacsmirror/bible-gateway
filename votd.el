;;; votd.el --- Bible Verse Of The Day -*- lexical-binding: t -*-

;; Copyright (C) 2025 Kristjon Ciko

;; Author: Kristjon Ciko
;; Keywords: convenience comm hypermedia
;; Homepage: https://github.com/kristjoc/votd
;; Package-Requires: ((emacs "29.1"))
;; Package-Version: 0.7.6

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

;; votd is a simple package that fetches the Bible verse of the day
;; from https://www.biblegateway.com/ and formats it to be used as an
;; emacs-dashboard footer or *scratch* buffer message. It can also
;; insert any requested Bible verse, passage, or chapter(s) at the
;; current point in the buffer.
;;
;; (votd-get-verse) fetches the verse of the day from https://www.biblegateway.com/
;; (votd-get-passage) fetches the requested passage and inserts it at point.

;;; Code:

(require 'url)
(require 'json)

(defgroup votd nil
  "Package that fetches the Bible verse of the day from BibleGateway."
  :group 'external)

(defcustom votd-bible-version "KJV"
  "The Bible version, default KJV. Other supported versions, which are available in the Public domain, are LSG in French, RVA in Spanish, and ALB in Albanian."
  :type 'string
  :group 'votd)

(defcustom votd-text-width 80
  "The width the votd body in number of characters, default 80."
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

(defcustom votd-request-timeout 3
  "The timeout for the URL request in seconds."
  :type 'integer
  :group 'votd)

(defcustom votd-include-ref t
  "If non-nil, include the reference (e.g., 'John 3 (KJV)') when printing the passage."
  :type 'boolean
  :group 'votd)

(defconst votd-bible-books-kjv
  '("Genesis" "Exodus" "Leviticus" "Numbers" "Deuteronomy" "Joshua" "Judges" "Ruth"
    "1 Samuel" "2 Samuel" "1 Kings" "2 Kings" "1 Chronicles" "2 Chronicles"
    "Ezra" "Nehemiah" "Esther" "Job" "Psalms" "Proverbs" "Ecclesiastes"
    "Song of Solomon" "Isaiah" "Jeremiah" "Lamentations" "Ezekiel" "Daniel"
    "Hosea" "Joel" "Amos" "Obadiah" "Jonah" "Micah" "Nahum" "Habakkuk"
    "Zephaniah" "Haggai" "Zechariah" "Malachi"
    "Matthew" "Mark" "Luke" "John" "Acts" "Romans" "1 Corinthians" "2 Corinthians"
    "Galatians" "Ephesians" "Philippians" "Colossians" "1 Thessalonians"
    "2 Thessalonians" "1 Timothy" "2 Timothy" "Titus" "Philemon" "Hebrews"
    "James" "1 Peter" "2 Peter" "1 John" "2 John" "3 John" "Jude" "Revelation")
  "List of Bible books in order.")

(defconst votd-bible-books-lsg
  '("Genèse" "Exode" "Lévitique" "Nombres" "Deutéronome" "Josué" "Juges" "Ruth"
    "1 Samuel" "2 Samuel" "1 Rois" "2 Rois" "1 Chroniques" "2 Chroniques"
    "Esdras" "Néhémie" "Esther" "Job" "Psaumes" "Proverbes" "Ecclésiaste"
    "Cantique des Cantiques" "Ésaïe" "Jérémie" "Lamentations" "Ézéchiel" "Daniel"
    "Osée" "Joël" "Amos" "Abdias" "Jonas" "Michée" "Nahum" "Habacuc"
    "Sophonie" "Aggée" "Zacharie" "Malachie"
    "Matthieu" "Marc" "Luc" "Jean" "Actes" "Romains" "1 Corinthiens" "2 Corinthiens"
    "Galates" "Éphésiens" "Philippiens" "Colossiens" "1 Thessaloniciens"
    "2 Thessaloniciens" "1 Timothée" "2 Timothée" "Tite" "Philémon" "Hébreux"
    "Jacques" "1 Pierre" "2 Pierre" "1 Jean" "2 Jean" "3 Jean" "Jude" "Apocalypse")
  "List of Bible books in order for the Louis Segond (LSG) version.")

(defconst votd-bible-books-rva
  '("Génesis" "Éxodo" "Levítico" "Números" "Deuteronomio" "Josué" "Jueces" "Rut"
    "1 Samuel" "2 Samuel" "1 Reyes" "2 Reyes" "1 Crónicas" "2 Crónicas"
    "Esdras" "Nehemías" "Ester" "Job" "Salmos" "Proverbios" "Eclesiastés"
    "Cantares" "Isaías" "Jeremías" "Lamentaciones" "Ezequiel" "Daniel"
    "Oseas" "Joel" "Amós" "Abdías" "Jonás" "Miqueas" "Nahúm" "Habacuc"
    "Sofonías" "Hageo" "Zacarías" "Malaquías"
    "Mateo" "Marcos" "Lucas" "Juan" "Hechos" "Romanos" "1 Corintios" "2 Corintios"
    "Gálatas" "Efesios" "Filipenses" "Colosenses" "1 Tesalonicenses"
    "2 Tesalonicenses" "1 Timoteo" "2 Timoteo" "Tito" "Filemón" "Hebreos"
    "Santiago" "1 Pedro" "2 Pedro" "1 Juan" "2 Juan" "3 Juan" "Judas" "Apocalipsis")
  "List of Bible books in order for the Reina-Valera Antigua (RVA) version.")

(defvar votd-bible-books-alb
  '("Zanafilla" "Eksodi" "Levitiku" "Numrat" "Ligji i Përtërirë" "Jozueu" "Gjyqtarët" "Ruthi"
    "1 i Samuelit" "2 i Samuelit" "1 i Mbretërve" "2 i Mbretërve" "1 i Kronikave" "2 i Kronikave"
    "Esdra" "Nehemia" "Ester" "Jobi" "Psalmet" "Fjalët e urta" "Predikuesi" "Kantiku i Kantikëve"
    "Isaia" "Jeremia" "Vajtimet" "Ezekieli" "Danieli" "Osea" "Joeli" "Amosi" "Abdia" "Jona" "Mikea"
    "Nahumi" "Habakuku" "Sofonia" "Hagai" "Zakaria" "Malakia" "Mateu" "Marku" "Luka" "Gjoni"
    "Veprat e Apostujve" "Romakëve" "1 e Korintasve" "2 e Korintasve" "Galatasve" "Efesianëve"
    "Filipianëve" "Kolosianëve"    "1 Thesalonikasve" "2 Thesalonikasve" "1 Timoteut" "2 Timoteut"
    "Titi" "Filemonit" "Hebrenjve" "Jakobit" "1 Pjetrit" "2 Pjetrit" "1 Gjonit" "2 Gjonit"
    "3 Gjonit" "Juda" "Zbulesa")
  "List of Bible books in Albanian (ALB).")

(defun votd-justify-line (line width)
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

(defun votd-format-verse-text (text &optional width)
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
                           (votd-justify-line line fill-column))
                         (butlast lines))
                 (last lines)))
               (result (string-join justified-lines "\n")))
          result)))))

(defun votd-decode-html-entities (text)
  "Decode HTML entities in TEXT, including numeric character references."
  (when text
    (let ((entity-map '(("&ldquo;" . "\"")
                        ("&rdquo;" . "\"")
                        ("&#8212;" . "--")
                        ("&#8217;" . "'")
                        ("&#8220;" . "\"")
                        ("&#8221;" . "\"")
                        ("&quot;" . "\"")
                        ("&apos;" . "'")
                        ("&lt;" . "<")
                        ("&gt;" . ">")
                        ("&nbsp;" . " ")
                        ("&amp;" . "&") ; Decode &amp; first
                        ("&#039;" . "'")))) ; Decode numeric entities like &#039; after &amp;
      ;; Replace named entities
      (dolist (entity entity-map text)
        (setq text (replace-regexp-in-string (car entity) (cdr entity) text)))
      ;; Replace numeric character references (e.g., &#233; -> é)
      ;; Fix for special characters in LSG and RVA
      (setq text
	    (replace-regexp-in-string "&#\\([0-9]+\\);"
                                      (lambda (match)
                                        (char-to-string (string-to-number (match-string 1 match))))
                                      text))
      ;; Return the decoded text
      text)))

(defun votd-fetch-daily-bible-verse ()
  "Fetch the daily Bible verse from BibleGateway API."
  (let ((url-request-method "GET")
        (url (concat "https://www.biblegateway.com/votd/get/?format=json&version=" votd-bible-version)))
    (condition-case nil
        (with-current-buffer (url-retrieve-synchronously url t t votd-request-timeout)
          (goto-char (point-min))
          (when (search-forward "\n\n" nil t)
            (let* ((json-string (buffer-substring-no-properties (point) (point-max)))
                   (json-object-type 'hash-table)
                   (json-array-type 'list)
                   (json-key-type 'string)
                   (json-data (json-read-from-string json-string))
                   (votd (gethash "votd" json-data))
                   (raw-text (gethash "text" votd))
                   (verse-text (votd-decode-html-entities raw-text))
                   (clean-verse (replace-regexp-in-string "[\"]" "" verse-text))
                   (formatted-verse (votd-format-verse-text clean-verse))
                   (verse-reference (gethash "display_ref" votd))
                   (fill-width votd-text-width))
              (format "%s\n%s"
                      formatted-verse
                      (let ((ref-text verse-reference))
                        (concat (make-string (- fill-width (length ref-text)) ?\s)
                                ref-text))))))
      (error
       (message "%s\n%s"
                (votd-format-verse-text votd-fallback-verse)
                (let ((ref-text votd-fallback-reference))
                  (concat (make-string (- votd-text-width (length ref-text)) ?\s)
                          ref-text)))))))

;;;###autoload
(defun votd-get-verse ()
  "Get the daily verse and handle errors."
  (condition-case err
      (votd-fetch-daily-bible-verse)
    (error
     (message "Today's verse could not be fetched: %s" (error-message-string err)))))

;; Below is the section for votd-get-passage

(defun votd-read-book ()
  "Read a Bible book name with completion based on selected Bible version."
  (completing-read
   "Book: "
   (cond ((string= votd-bible-version "LSG")
          votd-bible-books-lsg)
         ((string= votd-bible-version "KJV")
          votd-bible-books-kjv)
	 ((string= votd-bible-version "RVA")
          votd-bible-books-rva)
	 ((string= votd-bible-version "ALB")
          votd-bible-books-alb)
         (t votd-bible-books-kjv)) ; default to KJV for unknown versions
   nil t))

(defun votd-read-chapter-verse (book)
  "Read chapter and verse for BOOK."
  (let ((input (read-string
                (format "Select passage from: %s " book))))
    (format "%s %s" book input)))

(defun votd-process-verse-text (text)
  "Process verse TEXT, handling special cases like small-caps LORD and UTF-8 encoding."
  (let ((processed-text text))
    ;; First decode UTF-8 octal sequences
    (setq processed-text
          (decode-coding-string
           (string-as-unibyte processed-text)
           'utf-8))

    ;; Replace small-caps span with "LORD"
    (setq processed-text
          (replace-regexp-in-string
           "<span style=\"font-variant: small-caps\" class=\"small-caps\">\\(Lord\\)</span>"
           "LORD"
           processed-text t))

    ;; Remove "Read full chapter" text
    (setq processed-text
          (replace-regexp-in-string "Read full chapter.*$" "" processed-text))

    ;; Remove trailing span IDs and div tags
    (setq processed-text
          (replace-regexp-in-string "\\(<span id=\"[^\"]*\".*\\|</div.*\\)$" "" processed-text))

    ;; Remove any remaining HTML tags
    (setq processed-text
          (replace-regexp-in-string "<[^>]+>" "" processed-text))

    ;; Fix non-breaking space
    (setq processed-text
          (replace-regexp-in-string "\\\\302\\\\240" " " processed-text))

    ;; Decode HTML entities (if any remain)
    (setq processed-text (votd-decode-html-entities processed-text))

    ;; Trim whitespace and return
    (string-trim processed-text)))

(defun votd-wrap-verse-text (verse-text)
  "Wrap VERSE-TEXT according to window width with 4 spaces for wrapped lines."
  (with-temp-buffer
    (insert verse-text)
    (let ((fill-prefix "    ")  ; 4 spaces for wrapped lines
          (fill-column (- (window-width) 5))) ; window width minus some margin
      (fill-region (point-min) (point-max))
      (buffer-string))))

;;;###autoload
(defun votd-get-passage ()
  "Fetch a Bible passage with book name completion."
  (interactive)
  (let* ((book (votd-read-book))
         (passage (votd-read-chapter-verse book))
	 (formatted-passage (replace-regexp-in-string " " "" passage))
         (url (concat "https://www.biblegateway.com/passage/?search="
		      formatted-passage "&version=" votd-bible-version)))
    (condition-case err
        (let ((original-buffer (current-buffer)))
          (with-current-buffer (url-retrieve-synchronously url t t votd-request-timeout)
            (goto-char (point-min))

	    ;; First get the title if required
	    (let ((title nil))
	      (when votd-include-ref
		(goto-char (point-min))
		(when (search-forward "<meta name=\"twitter:title\" content=\"" nil t)
		  (let ((start (point))
			(end (search-forward "\"")))
		    ;; Decode the title here
		    (setq title (decode-coding-string
				 (string-as-unibyte
				  (buffer-substring-no-properties start (1- end)))
				 'utf-8)))))

              ;; Then get the verses
              (goto-char (point-min))
              (if (search-forward "<div class='passage-content passage-class-0'>" nil t)
                  (let* ((start (point))
                         (end (search-forward "</div>"))
                         (raw-content (buffer-substring-no-properties start (1- end)))
                         (verses '())
                         (pos 0))

                    ;; First, remove chapter numbers
                    (setq raw-content
                          (replace-regexp-in-string
			   "<span class=\"chapternum\">[^<]*</span>" "" raw-content))

                    ;; Remove verse numbers but keep content
                    (setq raw-content
                          (replace-regexp-in-string
			   "<sup class=\"versenum\">[^<]*</sup>" "" raw-content))

                    ;; Break the content into individual verse spans for processing
                    (with-temp-buffer
                      (insert raw-content)
                      (goto-char (point-min))
                      (while (re-search-forward "class=\"text [^\"]*-\\([0-9]+\\)\">" nil t)
                        (let* ((verse-num (match-string 1))
                               (verse-start (match-end 0))
                               (verse-end (save-excursion
                                            (if (re-search-forward "class=\"text" nil t)
						(match-beginning 0)
                                              (point-max))))
                               (verse-content
				(buffer-substring-no-properties verse-start verse-end))
			       ;; First process the verse text normally
                               (verse-text (votd-process-verse-text verse-content))
			       ;; Then wrap it with proper formatting
			       (final-text (votd-wrap-verse-text verse-text)))
                          (when (not (string-empty-p final-text))
			    (push (format "%s.%s%s" verse-num
					  (if (< (string-to-number verse-num) 10) "  " " ")
					  final-text)
				  verses)))))

                    ;; Insert title and verses
                    (with-current-buffer original-buffer
                      (when (and votd-include-ref title)
			(insert title "\n\n"))
                      (insert (string-join (reverse verses) "\n"))))
	        (message "Sorry, we didn’t find any results for your search. Please double-check that the chapter and verse numbers are valid.")))))
      ('error
       (message "Error while fetching the passage: %s" (error-message-string err))))))

(provide 'votd)
;;; votd.el ends here
