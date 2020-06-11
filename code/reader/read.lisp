(cl:in-package #:eclector.reader)

;;; Entry points

(defun read-aux
    (input-stream eof-error-p eof-value recursive-p preserve-whitespace-p)
  (let ((client *client*))
    (if recursive-p
        (read-common client input-stream eof-error-p eof-value)
        (let* ((*labels* (make-hash-table))
               (values (multiple-value-list
                        (read-common client input-stream eof-error-p eof-value)))
               (result (first values)))
          ;; *LABELS* maps labels to conses of the form
          ;;
          ;;   (TEMPORARY-OBJECT . FINAL-OBJECT)
          ;;
          ;; where TEMPORARY-OBJECT is EQ-comparable and its
          ;; sub-structure does not matter here. For the fixup step,
          ;; convert these conses into a hash-table mapping temporary
          ;; objects to final objects.
          (unless (zerop (hash-table-count *labels*))
            (let ((seen (make-hash-table :test #'eq))
                  (mapping (alexandria:alist-hash-table
                            (alexandria:hash-table-values *labels*)
                            :test #'eq)))
              (fixup client result seen mapping)))
          ;; All reading in READ-COMMON and its callees was done in a
          ;; whitespace-preserving way. So we skip zero to one
          ;; whitespace characters here if requested via
          ;; PRESERVE-WHITESPACE-P.
          (unless preserve-whitespace-p
            (skip-whitespace input-stream))
          (values-list values)))))

(macrolet
    ((define (name preserve-whitespace-form)
       `(progn
          (defun ,name (&optional
                        (input-stream *standard-input*)
                        (eof-error-p t)
                        (eof-value nil)
                        (recursive-p nil))
            (read-aux input-stream eof-error-p eof-value recursive-p ,preserve-whitespace-form))

          (define-compiler-macro ,name (&whole form
                                        &optional (input-stream '*standard-input*)
                                                  (eof-error-p 't)
                                                  (eof-value 'nil)
                                                  (recursive-p nil))
            (if (and (constantp recursive-p)
                     (eval recursive-p))
                `(read-common *client* ,input-stream ,eof-error-p ,eof-value)
                form)))))
  (define read                       recursive-p)
  (define read-preserving-whitespace t))

(defun read-from-string (string &optional (eof-error-p t)
                                          (eof-value nil)
                                &key (start 0)
                                     (end nil)
                                     (preserve-whitespace nil))
  (let ((index))
    (values (with-input-from-string (stream string :start start :end end
                                                   :index index)
              (read-aux stream eof-error-p eof-value nil preserve-whitespace))
            index)))

;;; Reading lists

;;; Read a list terminated by CLOSE-CHAR from STREAM. For each
;;; encountered list element as well the end of the list (or premature
;;; end of input) call FUNCTION with two arguments: 1) an element kind
;;; indicator which is one of :PROPER, :TAIL and :END 2) the read
;;; element, EOL-VALUE or EOF-VALUE.
(defun %read-list-elements (stream function eol-value eof-value
                            close-char recursive-p consing-dot-allowed-p)
  (let ((state :proper))
    (flet ((special-result (result)
             (return-from %read-list-elements
               (cond
                 ((or (eq result **end-of-list**)
                      (typep result 'end-of-list))
                  (let ((char (%character result)))
                    (unless (char= close-char char)
                      (%recoverable-reader-error
                       stream 'invalid-context-for-right-parenthesis
                       :expected-character close-char :found-character char
                       :report 'ignore-trailing-right-paren)))
                  (cond ((and (not (null eol-value))
                              (funcall function state eol-value)))
                        ((eq state :tail)
                         (%recoverable-reader-error
                          stream 'object-must-follow-consing-dot
                          :report 'inject-nil))))
                 ((eq result +end-of-input+)
                  (cond ((and (not (null eof-value))
                              (funcall function state eof-value)))
                        (t
                         (when (eq state :tail)
                           (%recoverable-reader-error
                            stream 'end-of-input-after-consing-dot
                            ;; :stream-position (stream-position result)
                            :report 'inject-nil))
                         (%recoverable-reader-error
                          stream 'unterminated-list
                                        ; :stream-position (stream-position result)
                          :delimiter close-char :report 'use-partial-list))))))))
      (loop with *consing-dot-allowed-p* = consing-dot-allowed-p
            for object = (let ((*consing-dot-allowed-p* nil))
                           (read stream nil +end-of-input+ t))
            then (read stream nil +end-of-input+ t)
            do (when (or (eq object +end-of-input+)
                         (eq object **end-of-list**)
                         (typep object 'end-of-list))
                 (special-result object))
            if (eq object *consing-dot*)
            do (setf *consing-dot-allowed-p* nil
                     state :tail)
               (let ((result (read stream nil +end-of-input+ t)))
                 (cond
                   ((eq result +end-of-input+)
                    (special-result result))
                   ((or (eq result **end-of-list**)
                        (typep result 'end-of-list))
                    (special-result result))
                   (t
                    (funcall function :tail result))))
               (setf state :end)
               ;; This call to read must not return (it has to signal
               ;; END-OF-LIST).
               (let ((result (read stream t +end-of-input+ t)))
                 (cond ((or (eq result +end-of-input+)
                            (eq result **end-of-list**)
                            (typep result 'end-of-list))
                        (special-result result))
                       (t
                        (%recoverable-reader-error
                         stream 'multiple-objects-following-consing-dot
                         :report 'ignore-object))))
            else
            do (funcall function state object)))))

(defun %read-delimited-list (stream close-char recursive-p)
  (alexandria:when-let ((list-reader *list-reader*))
    (return-from %read-delimited-list
      (funcall list-reader stream close-char)))

  (let ((reversed-result '())
        (tail nil))
    (flet ((element (state value)
             (case state
               (:proper (push value reversed-result))
               (:tail (setf tail value)))))
      (%read-list-elements stream #'element nil nil close-char recursive-p t))
    (nreconc reversed-result tail)))

(defun read-delimited-list (char &optional (input-stream *standard-input*) recursive-p)
  (%read-delimited-list input-stream char recursive-p))
