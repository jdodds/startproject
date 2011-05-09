;;; startproject.el --- simple project starter

;; Copyright (C) 2010 myfreeweb

;; Author: myfreeweb <me@myfreeweb.ru>,
;;         Jeremiah Dodds <jeremiah.dodds@gmail.com>
;; Keywords: tools

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

;; Simple project starter. See README.md for more info.

;;; Code:

(require 'ido)
(require 'vc nil t) ; optional

;;;###autoload
(defgroup startproject nil
  "Project starting toolkit"
  :group 'tools)

(defcustom startproject-projects-root "~/"
  "The folder that contains your project folders"
  :type 'directory
  :group 'startproject)

(defcustom startproject-vc-systems  '("bzr" "hg" "git" "svn")
  "List of available version control systems"
  :type '(repeat string)
  :group 'startproject)

(defcustom startproject-vc-init-commands-alist
  '(("bzr" . "init") ("hg" . "init") ("git" . "init") ("svn" . "init"))
  "An alist mapping vcs names to their init-repository commands"
  :type '(alist :value-type string)
  :group 'startproject)

(defcustom startproject-vc-open-dir t
  "Whether or not to open the basedir of the projet with vc-open-dir"
  :type 'boolean
  :group 'startproject)

(defcustom startproject-project-starters
  '(("django" . ("django-admin.py startproject"))
    ("pylons" . ("paster create -t pylons"))
    ("rails" . ("rails"))
    ("catalyst" . ("catalyst.pl"))
    ("sproutcore" . ("sc-init")))
  "An alist mapping project types to the list of commands that should be run to
initialize this type of project"
  :type '(alist :value-type (repeat string))
  :group 'startproject)

(defun add-to-project-start (project-type command)
  "Add a command to the list that will be run when starting a project-type
project"
  (let ((current-commands (aget startproject-project-starters project-type t)))
    (if current-commands
	(add-to-list 'current-commands command t)
      (setq current-commands (list command)))
    (aput 'startproject-project-starters project-type current-commands)))


;;;###autoload
(defun startproject-add-commands (project-type &rest commands)
  "Add multiple commands to the list that will be run when starting a
project-type project"
  (let ((project-add-command
	 (apply-partially 'add-to-project-start project-type)))
    (dolist (command commands)
      (funcall project-add-command command))))

(defun really-start-project (project-type vcs project-name)
  (let ((commands (aget startproject-project-starters project-type t))
	(project-dir
	 (expand-file-name project-name startproject-projects-root)))
    (unless (file-exists-p project-dir)
      (make-directory project-dir))
    (let ((default-directory project-dir))
      (cd project-dir)
      (dolist (command commands)
	(shell-command command))
      (shell-command
       (combine-and-quote-strings
	(list vcs (aget startproject-vc-init-commands-alist vcs t))))
      (dired default-directory)
      (if startproject-vc-open-dir
	  (vc-dir default-directory)))))

;;;###autoload
(defun startproject (name)
  "Start a new project"
  (interactive "sProject Name: ")
  (really-start-project (ido-completing-read
			 "Type: "
			 (mapcar (lambda (values) (car values))
				 startproject-project-starters)
			 nil 'require-match nil nil)
			(ido-completing-read
			 "VCS: "
			 startproject-vc-systems
			 nil 'require-match nil nil)
			name))
(provide 'startproject)
;;; startproject.el ends here
