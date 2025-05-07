;;; flex-complete.el -*- lexical-binding:t -*-
;; Copyright (C) 2025 The Calendrical System
;; SPDX-License-Identifier: 0BSD

;; Core Fuzzy Searching Function

(defun flex-complete-fuzzy-search (target source)
  "Fuzzy search TARGET string within SOURCE.

If TARGET matches, return a list of the positions of each character of
TARGET in the SOURCE. Otherwise return NIL."
  (cl-loop with start = -1
	   for c across target
	   do (setq start (cl-position c source :start (1+ start) :test #'=))
	   if (null start) do (cl-return)
	   else collect start))

;; Core Complete Function

;; --- Helper ---
(defcustom flex-complete-simple-before 5
  "Run simple completion before this length of string inserted."
  :group 'flex-complete
  :type 'integer)
;; --- Helper ---
(defvar flex-complete-elisp-cache nil
  "Cache for incremental search") ; (str . ((sym starts string) ...))
;; --- END Helper ---
(defun flex-complete-elisp (str)
  "Complete function for Flex Complete Symbol."
  (when (and str (cl-plusp (length str)))
    (if (< (length str) flex-complete-simple-before)
        (all-completions str obarray)
      (let (result
	    (kwp (= (aref str 0) ?\:)))
        (if (and flex-complete-elisp-cache
	         (string-prefix-p (car flex-complete-elisp-cache) str))
	    (dolist (x (cdr flex-complete-elisp-cache))
	      (cl-destructuring-bind (sym _ string) x
	        (when-let (starts (flex-complete-fuzzy-search str string))
		  (push (list sym starts string) result))))
	  (mapatoms
	   (lambda (sym)
	     (let ((string (symbol-name sym)))
	       (when (eq kwp (keywordp sym))
	         (when-let (starts (flex-complete-fuzzy-search str string))
		   (push (list sym starts string) result)))))))
        ;; Sort symbols according to the "density" of matched characters -
        ;; more the matched characters grouped together, more it is
        ;; preferred. The start position of the first match is also count.
        ;; If they're same, then rank two symbols using length & string<
        ;; of their string representation.
        (setq result
	      (cl-sort
               result
	       #'(lambda (list1 list2)
		   (cl-destructuring-bind (starts1 string1) list1
		     (cl-destructuring-bind (starts2 string2) list2
		       (let ((n1 (cl-loop for j = 0 then i
					  for i in starts1
					  sum (- i j)))
			     (n2 (cl-loop for j = 0 then i
					  for i in starts2
					  sum (- i j))))
			 (if (= n1 n2)
			     (let ((len1 (length string1))
				   (len2 (length string2)))
			       (if (= len1 len2)
				   (string< string1 string2)
				 (< len1 len2)))
			   (< n1 n2))))))
	       :key #'cdr))
        (setq flex-complete-elisp-cache (cons str result))
        (mapcar #'cl-third result)))))

(defun flex-complete-company (command &optional arg &rest _)
  (interactive (list 'interactive))
  (when (and flex-complete-mode
             (eq major-mode 'emacs-lisp-mode))
    (pcase command
      (`interactive (company-begin-backend 'company-capf))
      (`prefix
       (company-capf--prefix))
      (`candidates
       (flex-complete-elisp arg))
      (`sorted t)
      (`duplicates nil)
      (`deprecated (let ((sym (intern-soft arg)))
		     (or (get sym 'byte-obsolete-variable)
			 (get sym 'byte-obsolete-info))))
      (`no-cache t)
      (`match (let ((starts (cl-second
			     (cl-find arg (cdr flex-complete-elisp-cache)
				      :test #'string=
				      :key #'cl-third))))
		(mapcar (lambda (i) (cons i (1+ i))) starts)))
      (`kind (let ((sym (intern arg)))
	       (cond ((member arg '("t" "nil")) 'boolean)
		     ((special-form-p sym)      'keyword)
		     ((macrop sym)              t)
		     ((keywordp sym)            'value)
		     ((= (aref arg 0) ?\&)      'value)
		     ((cl-find-class sym)       'class)
		     ((fboundp sym)             'function)
		     ((boundp sym)              'variable)
		     (t                         'text)))))))

(define-minor-mode flex-complete-mode
  "Flex completion for Emacs Lisp. Compatible with Company and Cape

Recommend binding:
(add-hook 'emacs-lisp-mode-hook 'flex-complete-mode)
"
  :group 'colourful
  (when (boundp 'company-backends)
    (setq company-backends
          '(company-bbdb
	    company-semantic company-cmake
	    company-clang company-files
	    flex-complete-company company-capf
	    (company-dabbrev-code company-gtags company-etags
				  company-keywords)
	    company-oddmuse company-dabbrev)))

  (when (bound-and-true-p corfu-mode)
    (setq-local completion-at-point-functions
                (cape-company-to-capf #'flex-complete-company))))

(provide 'flex-complete)

;;; flex-complete.el ends here