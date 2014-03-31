(in-package someshell)

;;;; TODO:
;;; More sophisticated parser
;;; More portable: Though I like GNU CLISP, it should not REQUIRE it.
;;; shell command completion
;;;

;;; Generic Utility Functions and Macros
(defmacro with-gensyms (symbols &body body)
  `(let ,(mapcar (lambda (s)
                   `(,s (gensym ,(concatenate 'string (symbol-name s) "-"))))
                 (remove-duplicates symbols))
     ,@body))

(defmacro nor (&rest predicate-statements)
  `(not (or ,@predicate-statements)))

(defmacro range (lower-limit upper-limit &optional step)
  (with-gensyms (accumulator)
    `(loop for ,accumulator from ,lower-limit to ,upper-limit ,@(when step (list 'by step))
        collecting ,accumulator)))

(defun iota (upper-limit)
  (range 0 (- upper-limit 1)))

(defmacro string-equalp-batch (comparator a-string &rest strings)
  `(,comparator ,@(mapcar #'(lambda (s) `(string= ,a-string ,s))
                          strings)))

(defmacro sexpr-extractor (string-sexpr)
  `(read-from-string ,string-sexpr))

(defun sexp-p (expr-string)
  "Test a string to see if it is an s-expression"
  (cond ((and (char= (char expr-string 0) #\()
              (char= (char expr-string
                           (1- (length expr-string)))
                     #\)))
         't)
        ((and (char= (char expr-string 0) #\()
              (not (char= (char expr-string
                                (1- (length expr-string)))
                          #\))))
         (error "Unbalanced Parenthesis"))
        (t
         'nil)))

;;; Core functionality of the shell system as such.
(defmacro parse-shell-command (input-string)
  "Break a string constituting a shell command up
into sub-strings, and submit them to the
RUN-PROGRAM macro for execution."
  (with-gensyms (command-list command arguments)
    `(let* ((,command-list (cl-ppcre:split "\\s\\s" ,input-string))
            (,command      (car ,command-list))
            (,arguments    (cdr ,command-list)))
       (list ,command ,@(when arguments
                              `(:arguments ,arguments))))))

;; (defun detect-non-name-space (string)
;;   ())

(let ((alias-list)
      (alias-load-filename #P"~/.clsh-aliases"))

  (defun load-aliases ()
    (with-open-file (in alias-load-filename)
      (with-standard-io-syntax
        (setf alias-list (read in)))))

  (defun save-aliases ()
    (with-open-file (out alias-load-filename
                         :direction :output
                         :if-exists :supersede)
      (with-standard-io-syntax
        (print alias-list out))))

  (defun return-aliases ()
    alias-list)

  (defun alias-add (alias command)
    (push (list alias command) alias-list)))

(defun shell-alias-p (command-string alias-list)
  (if (assoc (intern (string-upcase (car (cl-ppcre:split "\\s" command-string))))
             alias-list)
      t
      nil))

(defmacro run-shell-alias (command-string alias-list)
  (with-gensyms (command-list)
    `(let ((,command-list (cl-ppcre:split "\\s" ,command-string)))
       (eval (append (cdr (assoc
                           (intern (string-upcase (car ,command-list)))
                           ,alias-list))
                     (cdr ,command-list))))))

(defmacro wrap-dir (&optional directory)
  `(ext:dir ,@(when directory
                    `(,directory))))

(defun print-friendly-path ()
  (cl-ppcre:regex-replace
   (format nil "~d" (ext:absolute-pathname "~"))
   (format nil "~d" (ext:absolute-pathname "."))
   "~/"))

(defun shell-reader% ()
  (let ((aliases (copy-list (return-aliases))))
    (loop (format t "(~d)→ " (print-friendly-path))
       (let ((current-expr (read-line)))
         (cond ((sexp-p current-expr)
                (format t "~d~%" (eval (sexpr-extractor current-expr))))
               ((shell-alias-p current-expr aliases)
                  (run-shell-alias current-expr aliases))
               ((string-equalp-batch or current-expr "exit" "quit" "bye")
                (format t " 😿 b-bye... ~%")
                (ext:quit))
               (t (apply #'ext:run-program (parse-shell-command current-expr))))))))
