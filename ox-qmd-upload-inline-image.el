;;; ox-qmd-upload-inline-image.el --- Upload org-mode inline images for Qiita  -*- lexical-binding: t -*-

;; Copyright (C) 2022 0x60DF

;; Author: 0x60DF <0x60df@gmail.com>
;; Created: 24 Nov 2022
;; Version: 0.0.1
;; Keywords: wp
;; URL: https://github.com/0x60df/ox-qmd
;; Package-Requires: ((emacs "27.1") (ox-qmd "1.0.5") (request "0.3.3") (mimetypes "1.0"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; ox-qmd-upload-inline-image provides the commands to upload org-mode
;; inline images for Qiita.

;; CAUTION
;; This library depends on NON-PUBLIC API of Qiita,
;; thus this library may cease to work without notice.

;; To upload images, call `ox-qmd-upload-inline-image'.
;; It scans buffer and try to upload all detected inline images.
;; The command above is also called via org-mode export menu.

;; URL of uploaded images are stored in a comment line in a buffer:
;;   # ox-qmd-upload-inline-image: (("local-link" . "URL") ...)
;; After loadig this library, qmd backend respects a comment line
;; like above, and transcode inline images as replacing local-link
;; to URL.
;; Cdr of each element can be a string of URL or a list whose car is
;; is a string of URL and cadr is a string of alternative text:
;;   # ox-qmd-upload-inline-image: (("local-link" "URL" "alt") ...)
;; The comment line can be located at any position in a buffer.
;; Buffer can contain multiple comment lines but only first one is
;; recognized.

;;; Code:

(require 'ox-qmd)
(require 'request)
(require 'mimetypes)

(defvar ox-qmd-upload-inline-image--uploaded-stack nil
  "Stack of uploaded images.
Elements looks like (IMAGE-FILE . UPLOAED-URL).")

(defconst ox-qmd-upload-inline-image--url-data-regexp
  " *#+ *ox-qmd-upload-inline-image: \\(.+\\)"
  "Regexp for detecting url data stored in buffer.")

(defcustom ox-qmd-upload-inline-image-access-token nil
  "String representing access token for Qiita.
Both read and write should be enabled for the token, and
scope of token should cover target service
Qiita/Qiita Team"
  :group 'org-export-qmd
  :type 'string)

(defcustom ox-qmd-upload-inline-image-no-ask nil
  "Flag if operations are performed without prompt."
  :group 'org-export-qmd
  :type 'boolean)

(defun ox-qmd-upload-inline-image--find-inline-images (org-buffer)
  "Return list of inline images of ORG-BUFFER.
ORG-BUFFER is a buffer object or buffer name."
  (with-current-buffer org-buffer
    (save-excursion
      (goto-char 1)
      (let ((match-images))
        (while
            (re-search-forward
             (concat "\\[\\[\\("
                     "\\(file:\\)?"
                     "\\(.*\\.\\(gif\\|jpeg\\|jpg\\|png\\|tiff\\|tif\\)\\)"
                     "\\)\\]\\]")
             nil t)
          (push (buffer-substring-no-properties (nth 6 (match-data))
                                                (nth 7 (match-data)))
                match-images))
        match-images))))

(defun ox-qmd-upload-inline-image--read-url-data (org-buffer)
  "Read and return URL data of already uploaded images in ORG-BUFFER.
ORG-BUFFER is a buffer object or buffer name."
  (let (data)
    (with-current-buffer org-buffer
      (save-excursion
        (goto-char 1)
        (if (re-search-forward
             ox-qmd-upload-inline-image--url-data-regexp nil t)
            (setq data (buffer-substring-no-properties (nth 2 (match-data))
                                                   (nth 3 (match-data)))))))
    (if data
        (with-temp-buffer
          (save-excursion
            (insert data))
          (read (current-buffer))))))

(defun ox-qmd-upload-inline-image--do-upload (image-file)
  "Upload IMAGE-FILE for Qiita.
IMAGE-FILE is a full path of a image-file to upload."
  (interactive "fImage-File: ")
  (or (file-name-absolute-p image-file)
      (error "File must be full path: %s" image-file))
  (or (file-readable-p image-file)
      (error "File is not readable: %s" image-file))
  (let ((content-type (mimetypes-guess-mime image-file))
        (name (file-name-nondirectory image-file))
        (size (file-attribute-size (file-attributes image-file))))
    (unless (and content-type name (numberp size))
      (error "Acquiring file properties faild: %s" image-file))
    (if (< (* 10 (expt 2 20)) size)
        (error "File is larger than max allowed size 10MB: %sMB"
               (/ size (expt 2.0 20))))
    (let ((extension (file-name-extension image-file)))
      (unless (member extension '("gif" "jpeg" "jpg" "png" "tiff" "tif"))
        (error "Unsupported file format: %s" extension)))
    (request "https://qiita.com/api/v2/authenticated_user"
      :type "GET"
      :headers `(("Authorization" .
                  ,(format "Bearer %s"
                           ox-qmd-upload-inline-image-access-token)))
      :parser 'json-read
      :error (cl-function
              (lambda (&rest args &key error-thrown &allow-other-keys)
                (message "Failed to get authenticated_user info: %s"
                         error-thrown)
                ;; to release thread
                (setq ox-qmd-upload-inline-image--uploaded-stack
                      ox-qmd-upload-inline-image--uploaded-stack)))
      :success
      `(lambda (&rest args)
         (if (or ox-qmd-upload-inline-image-no-ask
                 (y-or-n-p
                  (format "Upload %s [%.2fMB] (/[%.1fMB @%s]) ? "
                          ,name
                          (/ ,size (expt 2.0 20))
                          (/ (cdr (assq 'image_monthly_upload_remaining
                                        (plist-get args :data)))
                             (expt 2.0 20))
                          (format-time-string "%b" (current-time)))))
             (request "https://qiita.com/api/v2/upload_policies"
               :type "POST"
               :headers '(("Authorization" .
                           ,(format "Bearer %s"
                                    ox-qmd-upload-inline-image-access-token))
                          ("Content-Type" . "application/json"))
               :data (json-serialize '((content_type . ,content-type)
                                       (name . ,name)
                                       (size . ,size)))
               :parser 'json-read
               :error (cl-function
                       (lambda (&rest args &key error-thrown &allow-other-keys)
                         (message "Failed to get upload_policies from Qiita: %s"
                                  error-thrown)
                         ;; to release thread
                         (setq ox-qmd-upload-inline-image--uploaded-stack
                               ox-qmd-upload-inline-image--uploaded-stack)))
               :success
               (cl-function
                (lambda (&key data &allow-other-keys)
                  (let ((bucket (cdr (assq 'bucket data)))
                        (download-url (cdr (assq 'download_url data)))
                        (form-data (cdr (assq 'form_data data))))
                    (let ((prepend-data
                           (lambda (ret)
                             (append
                              (apply
                               'append
                               (mapcar
                                (lambda (key-value)
                                  (list "--form" (format "%s=%s"
                                                         (car key-value)
                                                         (cdr key-value))))
                                form-data))
                              ret))))
                      (unwind-protect
                          (progn
                            (advice-add 'request--curl-command-args
                                        :filter-return prepend-data)
                            (request (format "https://%s.s3.amazonaws.com/"
                                             bucket)
                              :type "POST"
                              :files '((file . ,image-file))
                              :error
                              (cl-function
                               (lambda (&rest args &key error-thrown
                                              &allow-other-keys)
                                 (message "Failed to upload image: %s"
                                          error-thrown)
                                 ;; to release thread
                                 (setq
                                  ox-qmd-upload-inline-image--uploaded-stack
                                  ox-qmd-upload-inline-image--uploaded-stack)))
                              :success
                              `(lambda (&rest args)
                                 (push
                                  (cons ,,image-file ,download-url)
                                  ox-qmd-upload-inline-image--uploaded-stack)
                                 (message "Image successfully upload: %s"
                                          ,download-url))))
                        (advice-remove 'request--curl-command-args
                                       prepend-data)))))))
           ;; to release thread
           (setq ox-qmd-upload-inline-image--uploaded-stack
                 ox-qmd-upload-inline-image--uploaded-stack))))))

(defun ox-qmd-upload-inline-image (org-buffer)
  "Upload inline images in ORG-BUFFER.
ORG-BUFFER is a buffer object or buffer name."
  (interactive
   (list (if current-prefix-arg
             (read-buffer "Buffer to upload inline images: "
                          nil t
                          (lambda (buffer-name)
                            (eq (buffer-local-value 'major-mode
                                                    (get-buffer
                                                     (if (consp buffer-name)
                                                         (car buffer-name)
                                                       buffer-name)))
                                #'org-mode)))
           (if (eq major-mode #'org-mode)
               (current-buffer)))))
  (let* ((image-list (ox-qmd-upload-inline-image--find-inline-images
                      org-buffer))
         (image-path-alist
          (mapcar (lambda (image)
                    (cons image
                          (with-current-buffer org-buffer
                            (expand-file-name image))))
                  image-list))
         (existing-url-alist (ox-qmd-upload-inline-image--read-url-data
                              org-buffer))
         (not-uploaded-uniq-path-list
          (seq-uniq
           (seq-filter
            (lambda (path)
              (not (member path
                           (mapcar (lambda (image-url)
                                     (expand-file-name (car image-url)))
                                   existing-url-alist))))
            (mapcar #'cdr image-path-alist))))
         (mutex (make-mutex))
         (cond-var (make-condition-variable mutex))
         (watcher (make-symbol "ox-qmd-upload-inline-image-variable-watcher"))
         (watcher-counter 0)
         (cond-notified nil))
    (fset watcher
          (lambda (_symbol _newval operation _where)
            (if (eq operation 'set)
                (if (< (- (length not-uploaded-uniq-path-list) 2)
                       watcher-counter)
                    (unwind-protect
                        (with-mutex mutex
                          (setq cond-notified t)
                          (condition-notify cond-var))
                      (remove-variable-watcher
                       'ox-qmd-upload-inline-image--uploaded-stack watcher))
                  (setq watcher-counter (1+ watcher-counter))))))
    (if (null not-uploaded-uniq-path-list)
        (if image-list
            (message "All inline images have already been uploaded")
          (message "No inline images are detected"))
      (add-variable-watcher 'ox-qmd-upload-inline-image--uploaded-stack watcher)
      (dolist (path not-uploaded-uniq-path-list)
        (ox-qmd-upload-inline-image--do-upload path))
      (make-thread
       (lambda ()
         (with-mutex mutex
           (while (not cond-notified)
             (condition-wait cond-var)))
         (let (path-url-alist)
           (dolist (path not-uploaded-uniq-path-list)
             (let ((cell (assoc path
                                ox-qmd-upload-inline-image--uploaded-stack)))
               (when cell
                 (push cell path-url-alist)
                 (setq ox-qmd-upload-inline-image--uploaded-stack
                       (assoc-delete-all
                        path
                        ox-qmd-upload-inline-image--uploaded-stack)))))
           (setq path-url-alist (reverse path-url-alist))
           (let ((image-url-alist
                  (mapcar (lambda (image-path)
                            (cons (car image-path)
                                  (cdr (assoc (cdr image-path)
                                              path-url-alist))))
                          (seq-filter (lambda (image-path)
                                        (assoc (cdr image-path) path-url-alist))
                                      image-path-alist))))
             (if image-url-alist
                 (with-current-buffer org-buffer
                   (save-excursion
                     (goto-char 1)
                     (let ((updated-url
                            (prin1-to-string
                             (append existing-url-alist image-url-alist))))
                       (if (re-search-forward
                            ox-qmd-upload-inline-image--url-data-regexp nil t)
                           (replace-match updated-url nil nil nil 1)
                         (goto-char (point-max))
                         (insert "\n# ox-qmd-upload-inline-image: "
                                 updated-url)))))))))))))

(defun org-qmd--link-suppoting-image-upload (link desc info)
  "Transcode LINK element with support for image upload.
DESC is nil.  INFO is a plist used as a communication
channel."
  (let ((standard (org-md-link link desc info)))
    (if (org-export-inline-image-p link org-html-inline-image-rules)
        (let* ((uploaded (copy-sequence standard))
               (url-data (ox-qmd-upload-inline-image--read-url-data
                          (plist-get info :input-buffer)))
               (regexp "!\\[\\(img\\)\\](\\(.+?\\)\\( .*\\)?)")
               (image (progn
                        (string-match regexp uploaded)
                        (match-string 2 uploaded)))
               (url-datum (cdr (assoc image url-data)))
               (url (if (consp url-datum)
                        (car url-datum)
                      url-datum))
               (alt (if (consp url-datum)
                        (cadr url-datum))))
          (if (stringp url)
              (let ((replaced (replace-match url nil t uploaded 2)))
                (if (stringp alt)
                    (replace-match alt nil t replaced 1)
                  replaced))
            uploaded))
      standard)))

(unless (featurep 'ox-qmd-upload-inline-image)
  (advice-add 'org-qmd--link :override #'org-qmd--link-suppoting-image-upload)

  (dolist (backend org-export-registered-backends)
    (if (eq 'qmd (org-export-backend-name backend))
        (setf (caddr (org-export-backend-menu backend))
              (append (caddr (org-export-backend-menu backend))
                      (list '(?u "To upload inline images"
                                 (lambda (a s v b)
                                   (ox-qmd-upload-inline-image
                                    (current-buffer))))))))))

(provide 'ox-qmd-upload-inline-image)

;;; ox-qmd-upload-inline-image.el ends here
