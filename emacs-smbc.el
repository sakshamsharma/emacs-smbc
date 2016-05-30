;;; emacs-smbc.el --- View SMBC from Emacs

;;; Copyright (C) 2016 Saksham Sharma <saksham0808@gmail.com>

;; Url: https://github.com/sakshamsharma/emacs-smbc
;; Author: Saksham Sharma <saksham0808@gmail.com>
;; Version: 1.0
;; Keywords: smbc webcomic

;;; Commentary:

;; For more information, visit https://github.com/sakshamsharma/emacs-smbc
;; This file is not a part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Code:
(require 'url)

;; TODO Shift to using an XML parser for rss.php file of smbc
;; Currently not doing that since it will make it hard to adapt
;; for a comic which does not have such a feed

(defun get-latest-smbc ()
  "Get latest SMBC comic and display in new buffer"
  (interactive)
  (get-smbc-image (parse-html-for-smbc (get-smbc-index-page))))

(defun get-smbc-image (imageID)
  "Retrieve and display image placed at SMBC."
  (with-help-window "SMBC"
    (with-current-buffer "SMBC"
      (let ((buffer (url-retrieve-synchronously
                     (concat "http://smbc-comics.com/" imageID))))
        (let ((data (with-current-buffer buffer
                      (goto-char (point-min))
                      (search-forward "\n\n")
                      (buffer-substring (point) (point-max)))))
          (insert-image (create-image data nil t))))
      )))

(defun parse-html-for-smbc (htmlPage)
  "Parse the input HTML for the comic image url"
  (chomp (let ((index (get-smbc-index-page)))
           (replace-regexp-in-string
            "\" id=\"comic.*" ""
            (replace-regexp-in-string
             ".*src=\"comics/\.\./" "" index))
           )))

(defun get-smbc-index-page ()
  "Retrieve a part of the index page of SMBC"
  (let ((buffer (url-retrieve-synchronously
                 "http://smbc-comics.com")))
    (with-current-buffer buffer
      (goto-char (point-min))
      (search-forward "comics/../comics")
      (thing-at-point 'line)
      )))

(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                       str)
    (setq str (replace-match "" t t str)))
  str)

(provide 'emacs-smbc)
;;; emacs-smbc.el ends here
