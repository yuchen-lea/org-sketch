;;; org-xournal.el --- Xournal support for Org Mode -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Yuchen Lea

;; Author: Yuchen Lea <yuchen.lea@gmail.com>
;; Version: 0.1.0
;; URL: https://github.com/yuchen-lea/org-xournal

;;; Commentary:

;; Quick insert Xournal++ files in org-mode Notes.
;; Auto refresh the preview image. Exportation supported.

;;; License:

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

;;; Code:
;;;; Requirements

(require 'org)
(require 'org-link-edit)
(require 'cl-lib)
(require 'filenotify)
(require 'f)

;;;; Customization

(defgroup org-xournal nil
  "org-xournal customization."
  :group 'org
  :package-version '(org-xournal . "0.1.0"))

(defcustom org-xournal-note-dir org-directory
  "Default directory to save Xournal note files."
  :group 'org-xournal
  :type 'string
  :package-version '(org-xournal . "0.1.0"))

(defcustom org-xournal-template-dir (expand-file-name "resources/" org-directory)
  "Directory to save Xournal template files."
  :group 'org-xournal
  :type 'string
  :package-version '(org-xournal . "0.1.0"))

(defcustom org-xournal-default-template-name "template.xopp"
  "Default Xournal template file.
Should located in `org-xournal-template-dir'"
  :group 'org-xournal
  :type 'string
  :package-version '(org-xournal . "0.1.0"))

(defcustom org-xournal-link-prefix "xournal"
  "Xournal Link Prefix."
  :group 'org-xournal
  :type 'string
  :package-version '(org-xournal . "0.1.0"))

(defcustom org-xournal-bin "xournalpp"
  "Location of xournalpp executable."
  :group 'org-xournal
  :type 'string
  :package-version '(org-xournal . "0.1.0"))

(defcustom org-xournal-process-picture-after-convert t
  "Whether to process the picture after conversion from *.xopp file."
  :group 'org-xournal
  :type 'bool
  :package-version '(org-xournal . "0.1.0"))

(defcustom org-xournal-use-relative-filename t
  "Whether to use relative link path or absolute link path."
  :group 'org-xournal
  :type 'bool
  :package-version '(org-xournal . "0.1.0"))


(defcustom org-xournal-get-new-filepath #'org-xournal--new-xournal-file-in-default-dir
  "Function returning filepath of new created image."
  :group 'org-xournal
  :type 'function
  :package-version '(org-xournal . "0.1.0"))

(defcustom org-xournal-get-new-desc (lambda () (read-string "Description: "))
  "Function returning description of new created image."
  :group 'org-xournal
  :type 'function
  :package-version '(org-xournal . "0.1.0"))

(defcustom org-xournal-path-format-function #'org-xournal-path-format-function-default
  "Function that takes FILENAME and returns a xournal note file path."
  :group 'org-xournal
  :type 'function
  :package-version '(org-xournal . "0.1.0"))

(defcustom org-xournal-process-picture-functon #'org-xournal-process-picture-functon-default
  "Function that process the image which is converted from *.xopp file."
  :group 'org-xournal
  :type 'function
  :package-version '(org-xournal . "0.1.0"))


;;;; Variables

(defvar-local org-xournal-overlays nil
  "A-list mapping file names to overlay.")

(defvar-local org-xournal-watchers nil
  "A-list mapping file names to change watcher descriptors.")

(defvar org-xournal-always-use-default-template nil)

;;;; Commands

(defun org-xournal--new-xournal-file-in-default-dir ()
  (let (
        (file-name (read-minibuffer "New Xournal file: "
                                    (format "%s_%s"
                                            (format-time-string "%Y%m%d_%H%M%S")
                                            (org-xournal-org-heading-escape (org-entry-get nil "ITEM")))))
        )
    (funcall org-xournal-path-format-function file-name)
    )
  )


(defun org-xournal-org-heading-escape (heading)
  (setq heading (replace-regexp-in-string "\\[.*\\]" "" heading))
  ;; First filter out weird symbols
  (setq heading (replace-regexp-in-string "[/;:'\"\(\)]+" "" heading))
  (setq heading (string-trim heading))
  ;; filter out swedish characters åäö -> aao
  (setq heading(replace-regexp-in-string "[åÅäÄ]+" "a" heading))
  (setq heading(replace-regexp-in-string "[öÓ]+" "o" heading))
  ;; whitespace and . to underscores
  (setq heading (replace-regexp-in-string "[ .]+" "_" heading))
  heading
  )

(defun org-xournal-path-format-function-default (file-name)
  "The default function of `org-xournal-path-format-function'.
Get the xournal note file link path by file-name."
  (let ((absolute-path (expand-file-name (format "%s.xopp" file-name)
                                         org-xournal-note-dir)))
    (if org-xournal-use-relative-filename
        (org-link-escape (file-relative-name absolute-path))
      (org-link-escape absolute-path))
    )
  )

(defun org-xournal-process-picture-functon-default (png-path)
  "Process the image png-path after conversion."
  (call-process-shell-command (format "convert %s -trim -bordercolor '#FFFFFF' -border 25 +repage %s" png-path png-path))
  )


(defun org-xournal-get-links ()
  "Get all xournal link in buffer"
  (org-element-map (org-element-parse-buffer) 'link
    (lambda (link)
      (when (string-equal (org-element-property :type link) org-xournal-link-prefix)
        link))))


(defun org-xournal-save-image (xournal-path png-path)
  "Convert XOURNAL-PATH to PNG and write it to PNG-PATH."
  (call-process-shell-command (format "%s %s -i %s" org-xournal-bin xournal-path png-path))
  )

(defun org-xournal-get-png (xournal-path)
  "Get png image data from given XOURNAL-PATH."
  (let ((png-path (expand-file-name (format "%s.png" (file-name-base xournal-path))
                                    temporary-file-directory)))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (org-xournal-save-image xournal-path png-path)
      (if org-xournal-process-picture-after-convert (funcall org-xournal-process-picture-functon png-path))
      (insert-file-contents-literally png-path)
      (buffer-string))
    )
  )

(defun org-xournal-make-new-image (output-xournal-path &optional default)
  "Create a new Xournal file based on a template at OUTPUT-XOURNAL-PATH."
  (let ((template
         (if org-xournal-always-use-default-template
             (expand-file-name org-xournal-default-template-name org-xournal-template-dir)
           (read-file-name "Chose Template:"  org-xournal-template-dir org-xournal-default-template-name t)
           )
         ))
    (f-copy template output-xournal-path)))


(defun org-xournal-show-link (link)
  (org-xournal-hide-link link)
  (let* ((start (org-element-property :begin link))
         (end (org-element-property :end link))
         (overlay (make-overlay start end))
         (xournal-path (org-element-property :path link)))
    (overlay-put overlay 'display (create-image (org-xournal-get-png xournal-path) 'png t :scale 0.4))
    (push (cons xournal-path overlay) org-xournal-overlays)))


(defun org-xournal-show-current-link (&optional complete-file link-location description)
  (cl-multiple-value-bind (start end link desc) (org-link-edit--link-data)
    (let* (
           (overlay (make-overlay start end))
           (xournal-path (nth 1 (split-string link ":")))
           )
      (overlay-put overlay 'display (create-image (org-xournal-get-png xournal-path) 'png t :scale 0.4))
      (push (cons xournal-path overlay) org-xournal-overlays)
      )
    )
  )

(defun org-xournal-hide-link (link)
  (let ((overlay (alist-get (org-element-property :path link) org-xournal-overlays nil nil #'string-equal)))
    (when overlay (delete-overlay overlay))))

(defun org-xournal-hide-all ()
  (dolist (link (org-xournal-get-links))
    (org-xournal-hide-link link)))


(defun org-xournal-disable ()
  "Disable watchers and hide xournal images."
  (dolist (watcher org-xournal-watchers)
    (file-notify-rm-watch (cdr watcher)))
  (setq org-xournal-watchers nil)
  (org-xournal-hide-all))

(defun org-xournal-event-file-path (event)
  (if (eq (nth 1 event) 'renamed)
      (nth 3 event)
    (nth 2 event)))

(defun org-xournal-watcher-callback (event)
  "Callback that runs after xournal files are modified."
  (let* ((xournal-path (org-xournal-event-file-path event))
         (links (org-xournal-get-links))  ;;TODO get all links?
         (paths (mapcar (lambda (it) (expand-file-name (org-element-property :path it))) links))
         (idx (cl-position xournal-path paths :test #'string-equal))) ;; then find current
    (when idx (org-xournal-show-link (nth idx links)))))

(defun org-xournal-add-watcher (xournal-path)
  "Setup auto-refreshing watcher for given xournal LINK."
  (let ((desc (file-notify-add-watch xournal-path '(change) #'org-xournal-watcher-callback)))
    (unless (alist-get xournal-path org-xournal-watchers nil nil #'string-equal)
      (push (cons xournal-path desc) org-xournal-watchers))))


(defun org-xournal-enable ()
  (unless (file-directory-p org-xournal-note-dir)
    (make-directory org-xournal-note-dir))
  (dolist (link (org-xournal-get-links))
    (org-xournal-show-link link)))


(defun org-xournal-edit (path)
  "Edit given PATH in xournal."
  (let ((xournal-path (expand-file-name path)))
    (when (f-exists-p xournal-path)
      (cond
       ((eq system-type 'darwin)
        (call-process-shell-command (format "open -a %s %s" org-xournal-bin xournal-path))
        )
       (t  ;; TODO need feedback from other os
        (call-process-shell-command (format "%s %s" org-xournal-bin xournal-path))
        ))
      (org-xournal-add-watcher xournal-path)
      )))

(defun org-xournal-export (_path _desc _backend)
  "Export xournal canvas _PATH from Org files.
Argument _DESC refers to link description.
Argument _BACKEND refers to export backend."
  (let ((png-path (f-swap-ext _path "png")))
    (cl-case _backend
      (html (format "<img src=\"%s\">"
                    (prog1 png-path
                      (org-xournal-save-image _path png-path))))
      (ascii (format "%s (%s)" (or _desc _path) _path))
      (latex (format "\\includegraphics[width=\\textheight,height=\\textwidth,keepaspectratio]{%s}"
                     (prog1 png-path
                       (org-xournal-save-image _path png-path)))))))


;;;; Functions

(org-link-set-parameters org-xournal-link-prefix
                         :follow #'org-xournal-edit
                         :export #'org-xournal-export
                         )

;;;###autoload
(defun org-xournal-insert-new-image (output-xournal-path desc)
  "Insert new image in current buffer."
  (interactive
   (let ((output-xournal-path (funcall org-xournal-get-new-filepath))
         (desc (funcall org-xournal-get-new-desc)))
     (list output-xournal-path desc)))
  (org-xournal-make-new-image output-xournal-path)
  (org-insert-link nil (format "%s:%s" org-xournal-link-prefix output-xournal-path) desc)
  (org-xournal-show-current-link)
  )

;;;###autoload
(defun org-xournal-insert-new-image-with-default-template ()
  "Insert new image in current buffer."
  (interactive)
  (let ((org-xournal-always-use-default-template t))
    (org-xournal-insert-new-image (funcall org-xournal-get-new-filepath) (funcall org-xournal-get-new-desc))
    )
  )


;;;###autoload
(define-minor-mode org-xournal-mode
  "Mode for displaying editable xournal images within Org file."
  :init-value nil
  (if org-xournal-mode (org-xournal-enable) (org-xournal-disable)))

;;;; Footer
(provide 'org-xournal)

;;; org-xournal.el ends here
