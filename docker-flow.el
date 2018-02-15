;;; docker-flow.el --- Emacs interface to Docker workflow

;; Author: Alexey Ermolaev <afay.zangetsu@gmail.com>
;; URL: https://github.com/AfsmNGhr/docker-flow.el
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.4") (dash "2.12.1") (s "1.11.0") (json-mode "1.7.0"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; # Emacs interface to Docker workflow!
;;
;; Improvement docker containers workflow from Emacs.

;;; Code:

(defun docker-flow-select-container-name (name)
  (interactive (list (docker-flow-read-container-name "Select container: "))))

(defun docker-flow-read-container-name (prompt)
  "Read an container name using PROMPT."
  (completing-read prompt (-map #'car (docker-flow-containers-names))))

(defun docker-flow-containers-names ()
  "Return the docker containers data for `tabulated-list-entries'."
  (let* ((filter "[{{json .Names}}]")
         (data (docker "ps" (format "--format=\"%s\"" filter) "-a"))
         (lines (split-string data "\n")))
    (-map #'docker-flow-container-parse lines)))

(defun docker-flow-container-parse (line)
  "Convert a LINE from \"docker ps\" to a `tabulated-list-entries' entry."
  (let (data)
    (condition-case err
        (setq data (json-read-from-string line))
      (json-readtable-error
       (error "could not read following string as json:\n%s" line)))
    (list (aref data 1) data)))

(defun docker (action &rest args)
  "Execute docker ACTION passing arguments ARGS."
  (let ((command (format "%s %s %s" "docker" action (s-join " " (-non-nil args)))))
    (message command)
    (shell-command-to-string command)))

(provide 'docker-flow)

;;; docker-flow.el ends here
