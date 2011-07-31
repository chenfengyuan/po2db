(defvar *need-load* t)
(cond (*need-load*
       ;; (asdf:oos 'asdf:load-op :sqlite))
       (asdf:oos 'asdf:load-op :cl-ppcre)))
(setf *need-load* nil)

(defpackage cfy.po2db
  ;; (:use :common-lisp :sqlite :cl-ppcre)
  (:use :common-lisp :cl-ppcre)
  (:export :po-read :po-get-headinfo :po-parse :flatlist :po-clear))

(in-package cfy.po2db)
(defvar *db-default-filename* "/dev/shm/main.sqlite")
(defvar *db* nil)
(defvar *table-suffix* "default")

;;file coding(utf-8)
#+ccl
(setf ccl:*default-external-format* :utf-8)


;; regular expressions
(defvar quote-text "\"[^\"\\\\]*(?:(?:\\\\.)+[^\"\\\\]*)+\"")

(defvar mydebug nil)
(defun mydebug(&rest rest)
    (if mydebug
	(apply #'format rest)))
(defun flatlist (l)
  (cond
    ((null l) nil)
    ((atom l) (list l))
    ((atom (car l)) (cons (car l) (flatlist (cdr l))))
    ((append (flatlist (car l)) (flatlist (cdr l))))))

(defun read-file-to-vector(filepath)
  (let ((content (make-array 0 :fill-pointer t :adjustable t)))
    (with-open-file (in filepath)
      (loop as i = (read-line in nil) while i do (vector-push-extend  i content)))
    content))


(defun read-file-to-list-and-count-msgid(filepath)
  (with-open-file (in filepath)
    (apply #'values
	   (loop as i = (read-line in nil)
	      while i
	      collect i into s
	      count (search "msgid" i) into id
	      finally (return (list s id))))))

(defun concatenate-strings(&rest strings)
  (apply #'concatenate 'string strings))

(let ((po)(index)(total))
  (defun po-clear()
    (setf po nil
	  index 0
	  total 0))
  
  (defun po-reset-index()
    (setf index 0))
  
  (defun po-read(filename)
    (setf po (read-file-to-vector filename))
    (setf index 0)
    (setf total (length po)))

  (defun po-read-line()
    (cond ((< index total)
	   (aref po (1- (incf index))))
	  (t nil)))

  (defun po-goto-previous-line()
    (if (> index 0)
	(decf index)))

  (defun po-if-eof()
    (= index total))

  (defun po-index()
    index)

  (defun po-total()
    total)

  (defun po-set-index(i)
    (cond ((>= i total)
	   (setf index (1- total)))
	  ((< i 0)
	   (po-reset-index))
	  (t
	   (setf index i)))))

(let ((pre))
  (defun po-index-save()
    (setf pre (po-index)))
  (defun po-index-restore()
    (and pre (po-set-index pre))))


(defun escape (string)
  (let ((l (coerce string 'list)))
    (coerce
     (loop for i in l
	if (eql #\' i)
	collect #\' and collect #\'
	else collect i) 'string)))

(defun escape-args (&rest args)
  (loop for i in args collect (escape i)))

(defmacro escape-and-setf (&rest args)
  `(progn ,@(loop for i in args collect `(setf ,i (escape ,i)))))

(defun get-quoted-text(string)
  (let* ((first (search "\"" string))
	 (last (search "\"" string :from-end t)))
    (if (and first last (not (= first last)))
	(subseq string (1+ first) last)
	"")))

(defun po-read-whole-item()
  (let ((first (get-quoted-text (po-read-line))))
    (apply #'concatenate-strings
	   first
	   (loop for i = (po-read-line)
	      while i
	      if (eql 0 (search "\"" i))
	      collect (get-quoted-text i) into s
	      else
	      do (po-goto-previous-line) and
	      return s))))

(defun po-get-headinfo-item(re string)
  (cadr
   (multiple-value-list
    (cl-ppcre:scan-to-strings
     re
     string))))

(defun po-get-headinfo()
  (po-index-save)
  (po-reset-index)
  (values
   (mapcar #'po-get-headinfo-item
	   ;; ("\"Last-Translator: YunQiang Su <wzssyqa@gmail.com>\\n\""
	   ;;  "\"Language-Team: Chinese (simplified) <i18n-zh@googlegroups.com>\\n\""
	   ;;  "\"Content-Type: text/plain; charset=UTF-8\\n\""
	   ;;  "\"Plural-Forms: nplurals=1; plural=0;\\n\"")
	   '("^\"Last-Translator: *([^<]+[^ <]) *<([^>]+)>"
	     "^\"Language-Team: *([^<]+[^ <]) *<([^>]+)>"
	     "^\"Content-Type: text/plain; charset=([^ ]+) *\\\\n\""
	     "^\"Plural-Forms: *(.+[^ ]) *\\\\n\"")
	   (flatlist
	    (loop for i = (po-read-line)
	       while i
	       if (eql 0 (search "\"Last-Translator:" i ))collect i into last
	       if (eql 0 (search "\"Language-Team:" i ))collect i into lang
	       if (eql 0 (search "\"Content-Type: text\/plain; charset=" i)) collect i into char
	       if (eql 0 (search "\"Plural-Forms:" i)) collect i into plural
	       until (and last lang char plural) finally (return (list last lang char plural)))))
   (po-index-restore)))

(defun po-read-whole-item-for-loop()
  (po-goto-previous-line)
  (po-read-whole-item))

(defun  po-parse()
  (let* ((id)(str)(ctxt)(flag)(result (make-array 0 :fill-pointer t :adjustable t))
	(when-id (lambda (new-id)
		   (if (eql nil id)
		       (setf id new-id)
		       (error (format nil "dumplicated id:~a~%" (po-index))))))
	(when-str (lambda (new-str)
		    (if (and (not (eql nil id)) (eql nil str))
			(setf str new-str)
			(error (format nil "error str:~a~%" (po-index))))))
	(when-ctxt (lambda (new-ctxt)
		     (if (eql nil ctxt)
			 (setf ctxt new-ctxt)
			 (error (format nil "dumplicated ctxt:~a~%" (po-index))))))
	(when-flag (lambda (new-flag)
		     (if (eql nil flag)
			 (setf flag new-flag)
			 (error (format nil "dumplicated flag:~a~%" (po-index))))))
	(when-comment (lambda (string)
			string
			(cond ((and id str)
			       (vector-push-extend (list id str ctxt flag) result)
			       (setf id nil str nil ctxt nil flag nil))
			      ((not (eql nil flag))
			       (setf flag nil)))))
	(when-blank-or-eof (lambda (string)
			     string
			     (mydebug t "blank:~a " (po-index))
			     (cond ((and id str)
				    (vector-push-extend (list id str ctxt flag) result)
				    (setf id nil str nil ctxt nil flag nil)))))
	(s0
	 (lambda (s1 s2 fn &optional (ext-fun nil))
	   (cond ((eql 0 (search s1 s2))
		  (if (eql nil  ext-fun)
		      (funcall fn s2)
		      (funcall fn (funcall ext-fun)))
		  t)
		 (t nil))))
	  (determined-when
	   (lambda (string)
	     (cond ((funcall s0 "msgid " string when-id #'po-read-whole-item-for-loop)(mydebug t "id:~a~%" (po-index)))
		   ((funcall s0 "msgstr " string when-str #'po-read-whole-item-for-loop)(mydebug t "str:~a~%" (po-index)))
		   ((funcall s0 "msgstr[0]" string when-str #'po-read-whole-item-for-loop)(mydebug t "str:~a~%" (po-index)))
		   ((funcall s0 "msgctxt" string when-ctxt #'po-read-whole-item-for-loop)(mydebug t "ctxt:~a~%" (po-index)))
		   ((funcall s0 "#," string when-flag)(mydebug t "#,:~a~%" (po-index)))
		   ((eql nil string) (funcall s0 "" string when-blank-or-eof)(mydebug t "nil:~a~%" (po-index)))
		   ((funcall s0 "#" string when-comment)(mydebug t "comment:~a~%" (po-index)))
		   ((funcall s0 "" string when-blank-or-eof)(mydebug t "empty:~a~%" (po-index)))
		   (t (error (format nil "unexpect:~a~%" string)))))))
    (po-index-save)
    (po-reset-index)
    (do ()
	((po-if-eof) (funcall determined-when (po-read-line))result)
      (funcall determined-when (po-read-line)))))

;; $dbh->do("create table '$t2' (pof text,lname text,lmail text,tname text,tmail text,charset text,pforms text)");
(defun headinfo-sql (table-name po-file-name headinfo)
  (let* ((last-translator (if (car headinfo) (car headinfo) #("" "")))
	 (lang-team (if (cadr headinfo) (cadr headinfo) #("" "")))
	 (charset (if (listp headinfo) (aref (caddr headinfo )0) ""))
	 (plural-forms (if (cadddr headinfo) (aref (cadddr headinfo) 0) ""))
	 (last-translator-name (aref last-translator 0))
	 (last-translator-email (aref last-translator 1))
	 (lang-team-name (aref lang-team 0))
	 (lang-team-email (aref lang-team 1)))
    (escape-and-setf table-name po-file-name lang-team-name lang-team-email last-translator-name last-translator-email charset plural-forms)
    ;; $dbh->do("insert into '$t2' values('$pof','$trans','$trans_e','$team','$team_e','$charset','$pf')");
    (format nil "insert into '~a' values('~a','~a','~a','~a','~a','~a','~a');" table-name po-file-name last-translator-name last-translator-email lang-team-name lang-team-email charset plural-forms)))

;; $dbh->do("create table '$t1' (id integer,msgid text,msgstr text,msgctxt text,fuzzy bool,flag text,pof text)");
(defun po-sql (table-name po-file-name po-parse-result)
  (escape-and-setf table-name po-file-name)
  (loop 
     for i across po-parse-result
     for id from 0
     for msgid = (car i)
     for msgstr = (cadr i)
     for msgctxt = (caddr i)
     for fuzzy = (if (search "fuzzy" (cadddr i)) 1 0)
     for flag = (if (cadddr i)
		    (cl-ppcre:regex-replace-all " *"
						(cl-ppcre:regex-replace "# *, *"
									(cl-ppcre:regex-replace ", *fuzzy" (cadddr i) "")
									"")
						"")
		    "")
     do (escape-and-setf msgid msgstr msgctxt flag)
     collect (format nil
		     ;; $dbh->do("insert into '$t1' values($id,'$msgid','$msgstr','$msgctxt',$fuzzy,'$flag','$pof');");
		     "insert into '~a' values('~a','~a','~a','~a','~a','~a','~a');"
		     table-name id msgid msgstr msgctxt fuzzy flag po-file-name)))

(defun po2sql (po-files output-file headinfo-table-name po-table-name)
  (with-open-file (out output-file :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format out "begin transaction;~%")
    ;; $dbh->do("create table '$t1' (id integer,msgid text,msgstr text,msgctxt text,fuzzy bool,flag text,pof text)");
    ;; $dbh->do("create table '$t2' (pof text,lname text,lmail text,tname text,tmail text,charset text,pforms text)");
    (format out "create table '~a' (id integer,msgid text,msgstr text,msgctxt text,fuzzy bool,flag text,por text);~%" po-table-name)
    (format out "create table '~a' (pof text,lname text,lmail text,tname text,tmail text,charset text,pforms text);~%" headinfo-table-name)
    (if (listp po-files)
	t
	(setf po-files (list po-files)))
    (loop for po in po-files
	 for po-file-name = (namestring po)
	 do (po-read po)
	 do (format out "~a~%" (headinfo-sql headinfo-table-name po-file-name (po-get-headinfo)))
	 do (loop for i in (po-sql po-table-name po-file-name (po-parse))
	       do (format out "~a~%" i)))
    (format out "commit;~%")))
