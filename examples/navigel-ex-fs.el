;;; redid.el --- A simple file manager  -*- lexical-binding: t; -*-

;; Copyright (C) 2019, 2020  Damien Cassou, 2021 Dmytro Lispyvnyi

;; Authors: Damien Cassou <damien@cassou.me>, Dmytro Lispyvnyi
;; Homepage: https://github.com/a13/redid

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file is an example usage of navigel.  It guides the reader
;; through an implementation of a tablist-based directory navigator.

;; In this example, we will implement a tablist-based UI to navigate
;; the folders of your computer.  As in dired, we want one file or
;; directory per line.  Pressing `RET' on a line should open the file
;; or directory at point.  Pressing `m' should mark the file at point
;; while `d' should delete all marked files.

;;; Code:

(require 'f)
(require 'memoize)

(require 'navigel)

;; Navigel is based on the notion of "entity".  In our example of a
;; directory navigator, the entities will be absolute filenames.

;; Navigel requires the developer to implement a command that calls
;; `navigel-open' on the initial entity.  Navigel also requires an
;; application name dynamically bound in the variable `navigel-app'.
;; This name is meant to disambiguate method definitions and is *not*
;; visible to the user.  In this example, we use `redid'
;; as name.  The code below defines the command:

(defun redid-list-files (&optional directory)
  "List files of DIRECTORY, home directory if nil."
  (interactive (list (getenv "HOME")))
  (let ((navigel-app 'redid))
    (navigel-open (f-expand directory) nil)))

;; For this command to display the files in the home directory (i.e.,
;; "~/"), navigel needs a way to get the children of a file entity.
;; Specifying behavior with navigel is done through method overriding.
;; How to get the children of an entity should be specified by
;; overriding the method `navigel-children':

(navigel-method redid navigel-children (directory callback)
  "Call CALLBACK with the files in DIRECTORY as parameter."
  (funcall callback (f-entries directory)))

;; `navigel-method' (which is syntactic sugar around `cl-defmethod')
;; is used to override the methods of navigel.  To distinguish this
;; override of `navigel-children' from other overrides made by other
;; navigel clients, the first parameter to `navigel-method' must be
;; the name of the application saved in `navigel-app' in the command
;; above.

;; At this point, you should be able to type `M-x
;; redid-list-files RET' to get a buffer showing all
;; files and folders in your home directory.  If you move the point to
;; a folder and press `RET', a new buffer should open listing its
;; files and folders.  If you type `M-x imenu RET', you can select one
;; entity of the buffer using completion: I recommend binding this
;; command or `counsel-imenu' to a key (e.g., to `M-i') because this
;; can be useful in many kinds of buffers.

;; A problem though: the absolute filenames (e.g., "/home/me/.bashrc")
;; are shown whereas a user probably expects to see basenames (e.g.,
;; ".bashrc") as in all file browsers.  We can easily change that by
;; overriding the `navigel-name' method:

(navigel-method redid navigel-name (file)
  (f-filename file))

;; This is much better.  With `RET', we can easily navigate from a
;; folder to its sub-folders.  Nevertheless, we have no way yet to
;; navigate back, i.e., from a folder to its parent.  To do that, we
;; need to override the `navigel-parent' method whose responsibility
;; is to return the parent entity of the entity passed as parameter:

(navigel-method redid navigel-parent (file)
  (f-dirname file))

;; You should now be able to press `^' to go to the parent directory
;; of the current one.

;; Pressing `RET' on a folder correctly opens the folder in another
;; navigel buffer.  But, just like in `dired', you might want that
;; pressing `RET' on a file opens the file itself.  This can be done
;; by overriding `navigel-open':

(navigel-method redid navigel-open (file _target)
  (setq-local buffer-file-name file)
  (if (f-file-p file)
      (find-file file)
    (cl-call-next-method)))

;; The `cl-call-next-method' call is used to express that we don't
;; have anything specific to do for a non-file first parameter and
;; that we want the default behavior.  This works perfectly fine!

;; We can improve the list of files a bit by adding some more
;; information about each file.  For example, we could have a first
;; column representing the size of each file.  We start by
;; implementing a function returning the size of its file argument:

;; We now specify the column values for each file by overriding
;; `navigel-entity-to-columns':

;; defcustom, maybe rename
(defvar redid-id-format 'string)

(defun redid--number-file-attribute (attr)
  (if (numberp attr)
      (number-to-string attr)
    (format "%s" attr)))

(defvar redid-file-size-flavor nil)

(defun redid--size-handler (size)
  (if (numberp size)
      (file-size-human-readable size redid-file-size-flavor)
    (format "%s" size)))

(defun redid--size-handler (size)
  (if (numberp size)
      (file-size-human-readable size redid-file-size-flavor)
    (format "%s" size)))

(defun redid-default-attribute-handler (attr)
  (if (stringp attr)
      attr
    (format "%s" attr)))

(defun redid-type-handler (file-type)
  (cond ((stringp file-type) file-type)
        (file-type "DIR")
        (t "FILE")))

;; defcustom
(defvar redid-time-format "%F %R")

(defun redid-time-handler (time)
  (format-time-string redid-time-format time))

;; defcustom?
(setq redid-file-attribute-handlers-alist
  '((file-attribute-type . redid-type-handler)
    (file-attribute-link-number . redid--number-file-attribute)
    (file-attribute-user-id)
    (file-attribute-group-id)

    (file-attribute-access-time . redid-time-handler)
    (file-attribute-modification-time . redid-time-handler)
    (file-attribute-status-change-time . redid-time-handler)

    (file-attribute-size . redid--size-handler)
    (file-attribute-modes)
    (file-attribute-inode-number . redid--number-file-attribute)
    (file-attribute-device-number . redid--number-file-attribute)))

(defvar redid--file-attributes-cache nil)

(defun redid--pp-file-attribute (attr-fn file)
  (when (functionp attr-fn)
    (let* ((attrs (or redid--file-attributes-cache
                      (file-attributes file 'string)))
           (attr (funcall attr-fn attrs))
           (handler (alist-get attr-fn redid-file-attribute-handlers-alist)))
      ;; TODO: cond
      (if (functionp handler)
          (funcall handler attr)
        (if (stringp attr)
            attr
          (funcall #'redid-default-attribute-handler attr))))))


(defun redid-type (file)
  (redid--pp-file-attribute #'file-attribute-type file))

(defun redid-link-number (file)
  (redid--pp-file-attribute #'file-attribute-link-number file))

(defun redid-user-id (file)
  (redid--pp-file-attribute #'file-attribute-user-id file))

(defun redid-group-id (file)
  (redid--pp-file-attribute #'file-attribute-group-id file))

(defun redid-access-time (file)
  (redid--pp-file-attribute #'file-attribute-access-time file))

(defun redid-modification-time (file)
  (redid--pp-file-attribute #'file-attribute-modification-time file))

(defun redid-status-change-time (file)
  (redid--pp-file-attribute #'file-attribute-status-change-time file))

(defun redid-size (file)
  (redid--pp-file-attribute #'file-attribute-size file))

(defun redid-modes (file)
  (redid--pp-file-attribute #'file-attribute-modes file))

(defun redid-inode-number (file)
  (redid--pp-file-attribute #'file-attribute-inode-number file))

(defun redid-device-number (file)
  (redid--pp-file-attribute #'file-attribute-device-number file))




(defvar redid-active-columns
  '(size name))

(defun redid-get-active-columns ()
  redid-active-columns)

;; defcustoms
(setq redid-columns-alist
      (let ((time-length (length (format-time-string redid-time-format))))
        `((size redid-size ("Size" 6 nil :right-align t) diredfl-number)
          (name navigel-name ("Name" 30 t) diredfl-file-name)
          (user redid-user-id ("UID" 10 t) default)
          (group redid-group-id ("GID" 10 t) default)
          (modes redid-modes ("MODES" 11) default)
          (access-time redid-access-time ("TIME" ,time-length t) default)
          (modification-time redid-modification-time ("TIME" ,time-length t) default)
          (status-change-time redid-status-change-time ("TIME" ,time-length t) default)
          (ext f-ext ("Extension" 10 t) diredfl-file-suffix))))

(defun redid--map-into-vector (function sequence)
  (apply #'vector (mapcar function sequence)))


(navigel-method redid navigel-entity-to-columns (file)
  ;; memoization doesn't work here
  (prog2
      (setq redid--file-attributes-cache (file-attributes file redid-id-format))
      (redid--map-into-vector (lambda (column)
                           (let* ((spec (alist-get column redid-columns-alist))
                                  (column-handler (car spec))
                                  (column-face (caddr spec))
                                  (column (or (and (functionp column-handler)
                                                   (funcall column-handler file))
                                              "")))
                             ;; TODO: check if column-face is a function?
                             (propertize column 'face column-face)))
                         (redid-get-active-columns))
    (setq redid--file-attributes-cache nil)))



;; The code above specifies that the first column of a file line will
;; contain the file size and the second will contain the filename.  We
;; aren't exactly done yet as we also need to specify what each column
;; shbaould look like.  This is done by overriding
;; `navigel-tablist-format':

(navigel-method redid navigel-tablist-format (_entity)
  (redid--map-into-vector (lambda (column)
                       (or (cadr (alist-get column redid-columns-alist))
                           (list (symbol-name column) 0 t)))
                     (redid-get-active-columns)))


;; This code defines the format of columns. The first column will have
;; "Size (B)" as title to indicate that the displayed numbers
;; represent the size in bytes.  The first column will be 10
;; characters wide and the numbers will be right aligned.  The second
;; column will have "Name as title and will take the rest of the
;; buffer width.  Read the documentation of `tabulated-list-format' to
;; get more information about the column format specification.

;; By default, navigel first sets the header information and then
;; proceeds to read the children of the current entity to display
;; them.  If you need to use the list of children to decide the format
;; of the header, you can override `navigel-tablist-format-children',
;; which is called _after_ the entities returned by `navigel-children'
;; are available.

;; As a final step, we might want to be able to delete files from the
;; file system.  This can be done by overriding `navigel-delete':

(navigel-method redid navigel-delete (file &optional callback)
  (f-delete file)
  (funcall callback))

;; The `funcall' is here to tell navigel that deletion is
;; finished. You can now mark files with `m' and delete them with `D'.

;; By default, all entities of the new application will be displayed
;; in their own buffers, named using the generic function
;; `navigel-name'.  Users of your application can ask navigel to reuse
;; the same buffer for all entities in the app by customizing the
;; variable `navigel-single-buffer-apps'.  The name of this single
;; buffer when it is displaying a given entity is constructed using
;; the generic function `navigel-single-buffer-name'.

(provide 'redid)
;;; redid.el ends here
