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

;(defvar projects-dir "~/")
(defvar projects-root "~/workspace")

(defvar project-starters '())

(defvar vc-systems
  '("bzr" "hg" "git" "svn" "none"))

(defvar vc-init-commands-alist '())

;; XXX temp until proper custom setup
(dolist (system '("bzr" "hg" "git" "svn")) 
  (aput 'vc-init-commands-alist system "init"))

(defvar sp-open-vc-dir t) ; maybe some users think it's annoying.

(defun add-to-project-start (project-type command)
  "Add a command to the list that will be run when starting a project-type
project"
  (let ((current-commands (aget project-starters project-type t)))
    (if current-commands
	(append current-commands command)
      (setq current-commands (list command)))
    (aput 'project-starters project-type current-commands)))

(defun add-commands (project-type &rest commands)
  "Add multiple commands to the list that will be run when starting a project-type project"
  (let ((project-add-command (apply-partially 'add-to-project-start project-type)))
    (dolist (command commands)
      (funcall project-add-command command))))

(add-commands "django" "django-admin.py startproject")
(add-commands "pylons" "paster create -t pylons")
(add-commands "rails" "rails")
(add-commands "catalyst" "catalyst.pl")
(add-commands "sproutcore" "sc-init")
(add-commands "test" "echo 'whoo'")

(defun really-start-project (project-type vcs project-name)
  (let ((commands (aget project-starters project-type t))
	(project-dir (expand-file-name project-name projects-root)))
    (unless (file-exists-p project-dir)
      (make-directory project-dir))
    (let ((default-directory project-dir))
      (dolist (command commands)
	(shell-command command))
      (shell-command (combine-and-quote-strings (list vcs (aget vc-init-commands-alist vcs t))))
      (dired default-directory)
      (if sp-open-vc-dir
	  (vc-dir default-directory)))))

(defun start-project (name)
  "Start a new project"
  (interactive "sProject Name: ")
  (really-start-project (ido-completing-read
                         "Type: "
			 (mapcar (lambda (values) (car values)) project-starters)
                         nil 'require-match nil nil)
                        (ido-completing-read
                         "VCS: "
                         vc-systems
                         nil 'require-match nil nil)
                        name))

(provide 'startproject)
;;; startproject.el ends here
