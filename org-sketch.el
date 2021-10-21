;;; org-sketch.el --- Xournal, Draw.io support for Org Mode -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Yuchen Lea

;; Author: Yuchen Lea <yuchen.lea@gmail.com>
;; Version: 0.2.0
;; URL: https://github.com/yuchen-lea/org-xournal

;;; Commentary:

;; Quick insert Xournal++, Draw.io files in org-mode Notes.
;; Auto refresh the preview image.

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

(defgroup org-sketch nil
  "org-sketch customization."
  :group 'org
  :package-version '(org-sketch . "0.2.0"))

(defcustom org-sketch-apps nil
  "Customize sketch apps. Currently support: xournal, drawio
For example:

  (\"xournal\"
   \"drawio\")"
  :type '(repeat string))

(defcustom org-sketch-note-dir-function (lambda () default-directory)
  "Default directory to save sketch note files, including *.xopp, *.drawio."
  :group 'org-sketch
  :type 'function
  :package-version '(org-sketch . "0.2.0"))

(defcustom org-sketch-process-picture-after-conversion t
  "Whether to process the picture after conversion from original sketch file."
  :group 'org-sketch
  :type 'bool
  :package-version '(org-sketch . "0.2.0"))

(defcustom org-sketch-process-picture-function #'org-sketch-process-picture-function-default
  "Function that process the image which is converted from *.xopp file."
  :group 'org-sketch
  :type 'function
  :package-version '(org-sketch . "0.2.0"))

(defcustom org-sketch-use-relative-filename t
  "Whether to use relative link path or absolute link path."
  :group 'org-sketch
  :type 'bool
  :package-version '(org-sketch . "0.2.0"))

(defcustom org-sketch-get-new-filepath #'org-sketch--new-sketch-file-path
  "Function returning filepath of new created image for given sketch type."
  :group 'org-sketch
  :type 'function
  :package-version '(org-sketch . "0.2.0"))

(defcustom org-sketch-get-new-desc (lambda () (read-string "Description: "))
  "Function returning description of new created image."
  :group 'org-sketch
  :type 'function
  :package-version '(org-sketch . "0.2.0"))

(defcustom org-sketch-path-format-function #'org-sketch-path-format-function-default
  "Function that takes FILENAME and EXTENSION, returns a sketch file path."
  :group 'org-sketch
  :type 'function
  :package-version '(org-sketch . "0.2.0"))

;;;;; Customization for xournal
(defcustom org-sketch-xournal-template-dir (expand-file-name "resources/" org-directory)
  "Directory to save Xournal template files."
  :group 'org-sketch
  :type 'string
  :package-version '(org-sketch . "0.2.0"))

(defcustom org-sketch-xournal-default-template-name "template.xopp"
  "Default Xournal template file.
Should located in `org-sketch-xournal-template-dir'"
  :group 'org-sketch
  :type 'string
  :package-version '(org-sketch . "0.2.0"))

(defcustom org-sketch-xournal-link-prefix "xournal"
  "Xournal Link Prefix."
  :group 'org-sketch
  :type 'string
  :package-version '(org-sketch . "0.2.0"))

(defcustom org-sketch-xournal-bin (cond
                            ((eq system-type 'darwin) "/Applications/Xournal++.app/Contents/MacOS/xournalpp")
                            (t "xournalpp"))
  "Location of xournalpp executable."
  :group 'org-sketch
  :type 'string
  :package-version '(org-sketch . "0.2.0"))


;;;;; Customization for draw.io
(defcustom org-sketch-drawio-link-prefix "drawio"
  "Xournal Link Prefix."
  :group 'org-sketch
  :type 'string
  :package-version '(org-sketch . "0.2.0"))

(defcustom org-sketch-drawio-bin (cond
                                  ((eq system-type 'darwin) "/Applications/draw.io.app/Contents/MacOS/draw.io")
                                  (t "draw.io"))
  "Location of xournalpp executable."
  :group 'org-sketch
  :type 'string
  :package-version '(org-sketch . "0.2.0"))


;;;; Variables

(defvar-local org-sketch-overlays nil
  "A-list mapping file names to overlay.")

(defvar-local org-sketch-watchers nil
  "A-list mapping file names to change watcher descriptors.")

(defvar org-sketch-always-use-default-xournal-template nil)

(defvar org-sketch-app-config (list '((prefix . "xournal")
                                      (extension . "xopp"))
                                    '((prefix . "drawio")
                                      (extension . "drawio"))
                                    '((prefix . "krita")
                                      (extension . "kra"))))

;;;; Commands
;;;;; Create

;;;###autoload
(defun org-sketch-insert (sketch-type sketch-file-path desc)
  "Insert new sketch file in current buffer."
  (interactive (let* ((sketch-type (ido-completing-read "Select sketch app: "
                                                        org-sketch-apps))
                      (sketch-file-path (funcall org-sketch-get-new-filepath sketch-type))
                      (desc (funcall org-sketch-get-new-desc)))
                 (list sketch-type sketch-file-path desc)))
  (cond
   ((string= sketch-type "xournal")
    (let ((org-sketch-always-use-default-xournal-template (y-or-n-p "Use default template?")))
      (org-sketch-create-new-xournal-file sketch-file-path)))
   ((string= sketch-type "drawio")
    (org-sketch-create-new-drawio-file sketch-file-path)))
  (org-insert-link nil
                   (format "%s:%s" sketch-type sketch-file-path)
                   desc)
  (org-sketch-edit sketch-file-path)
  (org-sketch-show-current-link)
  (org-sketch-add-watcher sketch-file-path))


;;;###autoload
(defun org-sketch-insert-new-xournal (output-xournal-path desc)
  "Insert new xournal file in current buffer."
  (interactive (let ((output-xournal-path (funcall org-sketch-get-new-filepath "xournal"))
                     (desc (funcall org-sketch-get-new-desc)))
                 (list output-xournal-path desc)))
  (org-sketch-insert "xournal" output-xournal-path desc))


(defun org-sketch-create-new-xournal-file (output-xournal-path &optional default)
  "Create a new Xournal file based on a template at OUTPUT-XOURNAL-PATH."
  (let ((template
         (if org-sketch-always-use-default-xournal-template
             (expand-file-name org-sketch-xournal-default-template-name org-sketch-xournal-template-dir)
           (read-file-name "Chose Template:"  org-sketch-xournal-template-dir org-sketch-xournal-default-template-name t))))
    (f-copy template output-xournal-path)))

;; TODO cmd fails sometimes
(defun org-sketch-create-new-drawio-file (output-drawio-path)
  "Create a new Xournal file based on a template at OUTPUT-XOURNAL-PATH."
  ;; (async-shell-command (format "%s -c %s" org-sketch-drawio-bin output-drawio-path))
  (call-process-shell-command (format "%s -c %s" org-sketch-drawio-bin output-drawio-path))
  )

(defun org-sketch--new-sketch-file-path (sketch-type)
  (let* ((extension (org-sketch-get-extension-by-prefix sketch-type))
         (heading (org-entry-get nil "ITEM"))
         (file-name (read-minibuffer "New file: "
                                     (format "%s_%s"
                                             (format-time-string "%Y%m%d_%H%M%S")
                                             (if heading
                                                 (org-sketch--org-heading-escape (org-entry-get nil "ITEM"))
                                               "")))))
    (funcall org-sketch-path-format-function file-name extension)))

(defun org-sketch--org-heading-escape (heading)
  (setq heading (replace-regexp-in-string "\\[.*\\]" "" heading))
  ;; First filter out weird symbols
  (setq heading (replace-regexp-in-string "[/;:'\"\(\)]+" "" heading))
  (setq heading (string-trim heading))
  ;; filter out swedish characters åäö -> aao
  (setq heading(replace-regexp-in-string "[åÅäÄ]+" "a" heading))
  (setq heading(replace-regexp-in-string "[öÓ]+" "o" heading))
  ;; whitespace and . to underscores
  (setq heading (replace-regexp-in-string "[ .]+" "_" heading))
  heading)

(defun org-sketch-path-format-function-default (file-name extension)
  "The default function of `org-sketch-path-format-function'.
Get the sketch file link path in default dir by file-name and extension."
  (let ((absolute-path (expand-file-name (format "%s.%s" file-name extension)
                                         (funcall org-sketch-note-dir-function))))
    (if org-sketch-use-relative-filename
        (org-link-escape (file-relative-name absolute-path))
      (org-link-escape absolute-path))))

;;;;; Display

(defun org-sketch-get-links ()
  "Get all sketch link in buffer"
  (org-element-map (org-element-parse-buffer) 'link
    (lambda (link)
      (when (member (org-element-property :type link) org-sketch-apps)
        link))))

(defun org-sketch-show-link (link)
  "Display sketch link as image."
  (org-sketch-hide-link link)
  (let* ((start (org-element-property :begin link))
         (end (org-element-property :end link))
         (overlay (make-overlay start end))
         (sketch-file-path (org-element-property :path link)))
    (org-sketch--add-sketch-image-to-overlay sketch-file-path overlay)))

(defun org-sketch-show-current-link (&optional complete-file link-location description)
  "Display current sketch link as image."
  (cl-multiple-value-bind (start end link desc) (org-link-edit--link-data)
    (let* ((overlay (make-overlay start end))
           (sketch-file-path (nth 1 (split-string link ":"))))
      (when (f-exists-p sketch-file-path)
      (org-sketch--add-sketch-image-to-overlay sketch-file-path overlay)))))

(defun org-sketch-hide-link (link)
  (let ((overlay (alist-get (org-element-property :path link) org-sketch-overlays nil nil #'string-equal)))
    (when overlay (delete-overlay overlay))))

(defun org-sketch--add-sketch-image-to-overlay (sketch-file-path overlay)
    (overlay-put overlay 'display (create-image (org-sketch-get-image-data sketch-file-path) 'png t :scale 0.4)) ;; TODO use config set image type
    (push (cons sketch-file-path overlay) org-sketch-overlays)
  )

(defun org-sketch-get-image-data (sketch-file-path)
  "Get png image data from given SKETCH-FILE-PATH."
  (let ((sketch-type (org-sketch-get-prefix-by-extension (file-name-extension sketch-file-path)))
        (image-path (expand-file-name (format "%s.png" (file-name-base sketch-file-path))  ;; TODO use config set image type
                                    temporary-file-directory)))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (org-sketch-save-image sketch-file-path image-path)
      (insert-file-contents-literally image-path)
      (buffer-string))))

(defun org-sketch-save-image (sketch-file-path image-path)
  "Convert XOURNAL-PATH to PNG and write it to PNG-PATH."
  (let ((sketch-type (org-sketch-get-prefix-by-extension (file-name-extension sketch-file-path))))
    (cond
     ((string= sketch-type "xournal")
      (org-sketch-xournal-save-image sketch-file-path
                                     image-path))
     ((string= sketch-type "drawio")
      (org-sketch-drawio-save-image sketch-file-path
                                    image-path))))
        (if org-sketch-process-picture-after-conversion (funcall org-sketch-process-picture-function image-path))) ;; TODO depends on sketch app

(defun org-sketch-xournal-save-image (xournal-path image-path)
  "Convert XOURNAL-PATH to image and write it to IMAGE-PATH."
  (call-process-shell-command (format "%s %s -i %s" org-sketch-xournal-bin xournal-path image-path)))

(defun org-sketch-drawio-save-image (drawio-path image-path)
  "Convert DRAWIO-PATH to image and write it to IMAGE-PATH."
  (call-process-shell-command (format "%s %s -x -o %s" org-sketch-drawio-bin drawio-path image-path)))

(defun org-sketch-process-picture-function-default (png-path)
  "Process the image png-path after conversion."
  (call-process-shell-command (format "mogrify -trim -fuzz 15%% -bordercolor '#FFFFFF' -border 10 %s" png-path)))

(defun org-sketch-hide-all ()
  (dolist (link (org-sketch-get-links))
    (org-sketch-hide-link link)))

;;;;; Edit
(defun org-sketch-edit (path)
  "Edit given PATH in corresponding app."
  (let ((sketch-type (org-sketch-get-prefix-by-extension (file-name-extension path)))
        (sketch-file-path (expand-file-name path)))
    (when (f-exists-p sketch-file-path)
      (cond
       ((string= sketch-type "xournal")
        (org-sketch-xournal-edit sketch-file-path))
       ((string= sketch-type "drawio")
        (org-sketch-drawio-edit sketch-file-path))))))

(defun org-sketch-xournal-edit (sketch-file-path)
  (cond
   ((eq system-type 'darwin)
    (call-process-shell-command (format "open -a %s %s" org-sketch-xournal-bin sketch-file-path)))
   (t (call-process-shell-command (format "%s %s" org-sketch-xournal-bin sketch-file-path)))))

(defun org-sketch-drawio-edit (sketch-file-path)
  (call-process-shell-command (format "%s %s" org-sketch-drawio-bin sketch-file-path)))

;;;;; Export
(defun org-sketch-export (_path _desc _backend)
  "Export sketch canvas _PATH from Org files.
Argument _DESC refers to link description.
Argument _BACKEND refers to export backend."
  (let ((png-path (f-swap-ext _path "png")))
    (cl-case _backend
      (html (format "<img src=\"%s\">"
                    (prog1 png-path
                      (org-sketch-save-image _path png-path))))
      (ascii (format "%s (%s)" (or _desc _path) _path))
      (latex (format "\\includegraphics[width=.9\\linewidth, keepaspectratio]{%s}"
                     (prog1 png-path
                       (org-sketch-save-image _path png-path)))))))

;;;;; Watcher
(defun org-sketch-add-watcher (sketch-file-path)
  "Setup auto-refreshing watcher for given sketch file LINK."
  (unless (alist-get sketch-file-path org-sketch-watchers nil nil #'string-equal)
    (let ((desc (file-notify-add-watch sketch-file-path '(change) #'org-sketch-watcher-callback)))
      (push (cons sketch-file-path desc) org-sketch-watchers))))

(defun org-sketch-find-link (sketch-file-path)
  (let* ((links (org-sketch-get-links))  ;;TODO get all links?
         (paths (mapcar (lambda (it) (expand-file-name (org-element-property :path it))) links))
         (idx (cl-position sketch-file-path paths :test #'string-equal))) ;; then find current
    (if idx (nth idx links))))

(defun org-sketch-watcher-callback (event)
  (if (eq (nth 1 event) 'changed)
      (let ((link (org-sketch-find-link (org-sketch-event-file-path event))))
        (if link (org-sketch-show-link link)))))

(defun org-sketch-event-file-path (event)
  (if (eq (nth 1 event) 'renamed)
      (nth 3 event)
    (nth 2 event)))

;;;;; Utils
(defun org-sketch-get-prefix-by-extension (extension)
  (let (extension-prefix-mapping)
    (mapcar (lambda (config)
              (let ((prefix (alist-get 'prefix config))
                    (extension (alist-get 'extension config)))
                (when extension
                  (setf (alist-get extension extension-prefix-mapping) prefix))))
            org-sketch-app-config)
    (cdr (assoc extension extension-prefix-mapping))))

(defun org-sketch-get-extension-by-prefix (prefix)
  (let (prefix-extension-mapping)
    (mapcar (lambda (config)
              (let ((prefix (alist-get 'prefix config))
                    (extension (alist-get 'extension config)))
                (when prefix
                  (setf (alist-get prefix prefix-extension-mapping) extension))))
            org-sketch-app-config)
    (cdr (assoc prefix prefix-extension-mapping))))

;;;;; Mode
;;;###autoload
(define-minor-mode org-sketch-mode
  "Mode for displaying editable sketch images within Org file."
  :init-value nil
  (if org-sketch-mode (org-sketch-enable) (org-sketch-disable)))

(defun org-sketch-enable ()
  (unless (file-directory-p (funcall org-sketch-note-dir-function))
    (make-directory (funcall org-sketch-note-dir-function)))
  (org-sketch-setup-customized-link)
  (dolist (link (org-sketch-get-links))
    (org-sketch-add-watcher (org-element-property :path link))
    (org-sketch-show-link link)))

(defun org-sketch-setup-customized-link ()
  (dolist (link-type org-sketch-apps)
    (when (string= link-type "xournal")
      (org-link-set-parameters org-sketch-xournal-link-prefix
                               :follow #'org-sketch-edit
                               :export #'org-sketch-export))
    (when (string= link-type org-sketch-drawio-link-prefix)
      (org-link-set-parameters "drawio" :follow #'org-sketch-edit
                               :export #'org-sketch-export))))

(defun org-sketch-disable ()
  "Disable watchers and hide xournal images."
  (dolist (watcher org-sketch-watchers)
    (file-notify-rm-watch (cdr watcher)))
  (setq org-sketch-watchers nil)
  (org-sketch-hide-all))

;;;; Footer
(provide 'org-sketch)

;;; org-sketch.el ends here
