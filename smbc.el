;;; smbc.el --- View SMBC from Emacs

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

(defun smbc-get-latest ()
  "Get latest SMBC comic and display in new buffer."
  (interactive)
  (smbc-get-image (smbc-parse-html (smbc-get-index-page))))

(defun smbc-get-image-from-image-id (image-id)
  "Fetch image from SMBC, given the IMAGE-ID."
  (interactive
   (list (read-string "Image ID (smbc-comics.com/): ")))
  (smbc-get-image image-id))

(defun smbc-get-image (image-id)
  "Retrieve, display image placed at SMBC with given IMAGE-ID."
  (let ((smbc-buffer-name (generate-new-buffer-name "SMBC")))
    (get-buffer-create smbc-buffer-name)
    (switch-to-buffer-other-window smbc-buffer-name)
    (read-only-mode 0)
    (let ((buffer (url-retrieve-synchronously
                   (concat "http://smbc-comics.com/" image-id))))
      (let ((data (with-current-buffer buffer
                    (goto-char (point-min))
                    (search-forward "\n\n")
                    (buffer-substring (point) (point-max)))))
        (insert-image (create-image data nil t))))
    (special-mode)))

(defun smbc-parse-html (html-page)
  "Parse the input HTML-PAGE for the comic image url."
  (smbc-chomp (let ((index html-page))
                (replace-regexp-in-string
                 "\" id=\"comic.*" ""
                 (replace-regexp-in-string
                  ".*src=\"comics/\.\./" "" index)))))

(defun smbc-get-index-page ()
  "Retrieve a part of the index page of SMBC."
  (let ((buffer (url-retrieve-synchronously
                 "http://smbc-comics.com")))
    (with-current-buffer buffer
      (goto-char (point-min))
      (search-forward "comics/../comics")
      (thing-at-point 'line))))

(defun smbc-get-page-given-id (id)
  "Retrieve part of a page with given ID to be used as a GET parameter."
  (let ((buffer (url-retrieve-synchronously
                 (concat "http://smbc-comics.com/index.php?id=" id))))
    (with-current-buffer buffer
      (goto-char (point-min))
      (search-forward "comics/../comics")
      (thing-at-point 'line))))

(defun smbc-chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                       str)
    (setq str (replace-match "" t t str)))
  str)

(provide 'smbc)
;;; smbc.el ends here
