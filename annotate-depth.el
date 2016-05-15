;;; annotate-depth.el --- Annotate buffer if indentation depth is beyond threshold.

;; Copyright (C) 2016  Morten Slot Kristensen

;; Author: Morten Slot Kristensen <msk AT nullpointer DOT dk>
;; Keywords: convenience
;; URL: https://github.com/netromdk/annotate-depth
;; Version: 0.1.0

;; This program is free software; you can redistribute it and/or modify it under the terms of the
;; GNU General Public License as published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
;; even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License along with this program.  If
;; not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Annotate buffer if indentation depth is beyond threshold. The face `annotate-depth-face' is
;; applied at indentation level and to end-of-line for each line beyond threshold.
;;
;; Use `annotate-depth' to (re)annotate the current buffer, or `annotate-depth-clear' to remove any
;; annotations.

;;; Code:

(defgroup annotate-depth nil
  "Annotate buffer if indentation depth is beyond threshold."
  :version "0.1"
  :group 'convenience)

(defface annotate-depth
  '((t :background "#990000"))
  "Default face to highlight too deep indentation levels."
  :version "0.1"
  :group 'annotate-depth)

(defcustom annotate-depth-face 'annotate-depth
  "Face to highlight too deep identation levels."
  :type 'face
  :group 'annotate-depth)

(defcustom annotate-depth-threshold 4
  "Depth threshold and beyond to annotate."
  :type 'integer
  :group 'annotate-depth)

(setq-local annotate-depth--overlays '())

(defun annotate-depth--determine-indent ()
  "Determine tab width or indentation offset."
  (or (catch 'loop
        (dolist (indentf '((lambda () (when (boundp 'c-basic-offset) c-basic-offset))
                           (lambda () (when (boundp 'lisp-indent-offset) lisp-indent-offset))
                           (lambda () (when (boundp 'sh-indentation) sh-indentation))
                           (lambda () (when (boundp 'js-indentation) js-indentation))
                           (lambda () (when (or (equal t tab-always-indent)
                                                (and (boundp 'c-tab-always-indent)
                                                     (equal t c-tab-always-indent)))
                                        tab-width))))
          (let ((val (funcall indentf)))
            (when (integerp val)
              (throw 'loop val)))))
      standard-indent))

;;;###autoload
(defun annotate-depth ()
  "Annotate depth when it gets beyond `annotate-depth-threshold'."
  (interactive)
  (save-excursion
    (annotate-depth-clear)
    (goto-char (point-min))
    (let ((indent-offset (annotate-depth--determine-indent)))
      (while (and (= 0 (forward-line 1))
                  (not (eobp)))
        (beginning-of-line)
        (back-to-indentation)
        (when (> (/ (current-indentation) indent-offset)
                 annotate-depth-threshold)
          (let ((overlay (make-overlay (point) (point-at-eol) nil t t)))
            (overlay-put overlay 'face annotate-depth-face)
            (add-to-list 'annotate-depth--overlays overlay)))))))

;;;###autoload
(defun annotate-depth-clear ()
  "Remove all annotations."
  (interactive)
  (dolist (overlay annotate-depth--overlays)
    (delete-overlay overlay))
  (setq annotate-depth--overlays '()))


(provide 'annotate-depth)
;;; annotate-depth.el ends here
