;; Copyright (C) 2011,2012 Chen Fengyuan (jeova.sanctus.unus+po2db (at) gmail.com)

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.


(defpackage :cfy.po2db
  ;; (:use :common-lisp :sqlite :cl-ppcre)
  (:use :common-lisp :cl-ppcre)
  (:export :po-read :po-get-headinfo :po-parse :flatlist :po-clear :main))

(in-package :cfy.po2db)
(defvar *default-db-file-path* "main.sqlite")
(defvar *default-sql* "sql")
(defvar *default-table-suffix* "default")
(defvar *default-table-prefix* "t_")
(defvar *default-headinfo-prefix* "h_")
(defun concatenate-strings(&rest strings)
  (apply #'concatenate 'string strings))
(defparameter *version* (let* ((arg (cadr #+sbcl sb-ext:*posix-argv*))
			       (file (or (and
					  arg
					  (scan "\\.lisp$" arg)
					  (probe-file arg))
					 (probe-file "po2db.lisp"))))
			  ( if file 
			       (file-write-date file)
			       0)))
(defparameter *version-string*
  (if (plusp *version*)
      (format nil "(~a)" *version*)
      ""))
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
	 (charset (if (caddr headinfo) (aref (caddr headinfo )0) ""))
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
		    (cl-ppcre:regex-replace-all "#? *"
						(cl-ppcre:regex-replace "# *, *"
									(cl-ppcre:regex-replace ", *fuzzy" (cadddr i) "")
									"")
						"")
		    "")
     do (escape-and-setf msgid msgstr msgctxt flag)
     if (not (string= "" msgid))
     collect (format nil
		     ;; $dbh->do("insert into '$t1' values($id,'$msgid','$msgstr','$msgctxt',$fuzzy,'$flag','$pof');");
		     "insert into '~a' values('~a','~a','~a','~a','~a','~a','~a');"
		     table-name id msgid msgstr msgctxt fuzzy flag po-file-name)
     else do (decf id)))

(defun probe-list (string-or-list)
  (if (and string-or-list (not (listp string-or-list)))
      (list string-or-list)
      string-or-list))
(defun com-with-sqlite3(db-filepath sql &key sqlite3-options)
  (let (;; (in (make-string-input-stream input))
	(output (make-string-output-stream )));; :element-type '(unsigned-byte 8)))
    (if (and sqlite3-options (not (listp sqlite3-options)))
	(setf sqlite3-options (list sqlite3-options)))
    (if sqlite3-options
	(progn
	  #+sbcl
	  (sb-ext:run-program "sqlite3" (list sqlite3-options db-filepath sql) :output  output :search t)
	  #+ccl
	  (ccl:run-program "sqlite3" (append sqlite3-options `( ,db-filepath ,sql)) :output output))
	(progn
	  #+sbcl
	  (sb-ext:run-program "sqlite3" (list db-filepath sql) :output output :search t)
	  #+ccl
	  (ccl:run-program "sqlite3" `(,db-filepath ,sql) :output output)))
    (get-output-stream-string output)))

(defun po2sql (po-files output-file headinfo-table-name po-table-name &key pre-sql suf-sql db-filepath)
  (with-open-file (out output-file :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format out "begin transaction;~%")

    ;; pre sql output
    (if pre-sql
	(loop for i in (probe-list pre-sql)
	   do (format out "~a~%" i)))
    
    (if db-filepath 
	(flet ((if-table-exists-rename (db-filepath table-name)
		 (let ((number-of-tables))
		   (if (= 1 (parse-integer (com-with-sqlite3
					    db-filepath
					    (format nil "select count(name) from sqlite_master where name == '~a';" table-name))))
		       (progn
			 (setf number-of-tables
			       (parse-integer
				(com-with-sqlite3
				 db-filepath
				 (format nil "select count(name) from sqlite_master where name like '~a%';" table-name))))
			 ;; $dbh->do("alter table '${t1}_$j1' rename to '${t1}_$j2'");
			 (format nil "alter table '~a' rename to '~:*~a_~a';" table-name (1- number-of-tables)))
		       nil))))
	  (loop for i in `(,po-table-name ,headinfo-table-name)
	     for sql = (if-table-exists-rename db-filepath i)
	     if sql 
	     do (format out "~a~%" sql))))

    ;; $dbh->do("create table '$t1' (id integer,msgid text,msgstr text,msgctxt text,fuzzy bool,flag text,pof text)");
    ;; $dbh->do("create table '$t2' (pof text,lname text,lmail text,tname text,tmail text,charset text,pforms text)");
    (format out "create table '~a' (id integer,msgid text,msgstr text,msgctxt text,fuzzy bool,flag text,pof text);~%" po-table-name)
    (format out "create table '~a' (pof text,lname text,lmail text,tname text,tmail text,charset text,pforms text);~%" headinfo-table-name)
    
    ;; (if (listp po-files)
    ;; 	t
    ;; 	(setf po-files (list po-files)))
    (setf po-files (probe-list po-files))
    (loop for po in po-files
       for po-file-name = (namestring po)
       do (po-read po)
       do (format out "~a~%" (headinfo-sql headinfo-table-name po-file-name (po-get-headinfo)))
       do (loop for i in (po-sql po-table-name po-file-name (po-parse))
	     do (format out "~a~%" i)))

    ;; output index sql
    (if db-filepath
	(let ((index-of-headinfo (concatenate-strings "i_" headinfo-table-name))
	      (index-of-po (concatenate-strings "i_" po-table-name))
	      (number-of-headinfo
	       (parse-integer
		(com-with-sqlite3
		 db-filepath
		 (format nil "select count(name) from sqlite_master where name like '~a%';" headinfo-table-name))))
	      (number-of-po
	       (parse-integer
		(com-with-sqlite3
		 db-filepath
		 (format nil "select count(name) from sqlite_master where name like '~a%';" po-table-name)))))
	  ;; $dbh->do("create index '$i1' on '$t1' (id,msgid,msgstr,msgctxt,fuzzy,flag,pof)");
	  ;; $dbh->do("create index '$i2' on '$t2' (pof,lname,lmail,tname,tmail,charset,pforms)");
	  (format out "create index '~a_~a' on '~a' (id,msgid,msgstr,msgctxt,fuzzy,flag,pof);~%" index-of-po number-of-po po-table-name)
	  (format out "create index '~a_~a' on '~a' (pof,lname,lmail,tname,tmail,charset,pforms);~%" index-of-headinfo number-of-headinfo headinfo-table-name))
	(let ((index-of-headinfo (concatenate-strings "i_" headinfo-table-name))
	      (index-of-po (concatenate-strings "i_" po-table-name)))
	  (format out "create index '~a' on '~a' (id,msgid,msgstr,msgctxt,fuzzy,flag,pof);~%" index-of-po po-table-name)
	  (format out "create index '~a' on '~a' (pof,lname,lmail,tname,tmail,charset,pforms);~%" index-of-headinfo  headinfo-table-name)))


    ;; suffix sql output
    (loop for i in (probe-list suf-sql)
       do (format out "~a~%" i))

    (format out "commit;~%")))

(defun max-string(s1 s2)
  (loop
     with b1 = (length s1)
     with b2 = (length s2)  
     for i from 0 upto b1
     if (or (>= i b2) (char/= (char s1 i)
			      (char s2 i)))
     return (subseq s1 0 i)))

(defun test ()
  (let* ((po-table "t_")
	 (headinfo-table "h_")
	 (table-suffix "default")
	 (output-file "/dev/shm/lisp2sqlite")
	 (po-files (loop for i in (directory "/dev/shm/pos/*.po") collect (namestring i)))
	 (headinfo-table-name (concatenate-strings headinfo-table table-suffix))
	 (po-table-name (concatenate-strings po-table table-suffix))
	 (db-filepath "/dev/shm/main"))
    ;; (with-open-file (out output-file :direction :output :if-exists :supersede :if-does-not-exist :create)
    ;;   (loop for i in sql
    ;; 	   do (format out "~a~%" i)))
    (po2sql po-files output-file headinfo-table-name po-table-name :db-filepath db-filepath)
    (com-with-sqlite3 db-filepath (concatenate-strings ".read " output-file))))

;;; argument parser
#+sbcl
(defun argv (&optional argv-test)
  (let ((argv (or argv-test (cdr sb-ext:*posix-argv*)))
	po-files)
    (multiple-value-bind (db-file-path table-suffix output-file)
	(values-list
	 (loop for i in argv
	    if (scan "\\.po$" i)
	    do (push i po-files)
	    else
	    unless (scan "\\.lisp$" i)
	    collect i into opt
	    finally (return opt)))
      (list (or db-file-path *default-db-file-path*)
	    (or table-suffix *default-table-suffix*)
	    (or output-file *default-sql*)
	    po-files))))
(defun main2 ()
  ;; (format t "hello,world!~%")
  (destructuring-bind
	(db-file-path table-suffix output-file po-files)
      (argv)
    (if (null po-files)
	(format
	 *standard-output*
	 "Usage: ~a~a [dot-lisp-file [db-file-path [table-suffix [sql-file]]]] po-files~%~aReport po2db.lisp bugs to jeova.sanctus.unus~agmail.org~%Git: https://github.com/chenfengyuan/po2db~%"
	 #+sbcl
	 (car sb-ext:*posix-argv*)
	 #-sbcl
	 "lisp"
	 *version-string*
	 (with-output-to-string (out)
	   (loop for (i j)in `(`,("db-file-path" ,*default-db-file-path*)
				 `,("table-suffix" ,*default-table-suffix*)
				 `,("sql-file" ,*default-sql*))
	      do (format out "The default value of ~a is ~a~%" i j)))
	 "@")
	(let ((headinfo-table-name (concatenate-strings *default-headinfo-prefix* table-suffix))
	      (po-table-name (concatenate-strings *default-table-prefix* table-suffix)))
	  (po2sql po-files output-file headinfo-table-name po-table-name :db-filepath db-file-path)
	  (com-with-sqlite3 db-file-path (concatenate-strings ".read " output-file))))))
(defun main ()
  (defun hot-update ()
    (let ((first (cadr
		  #+sbcl
		  sb-ext:*posix-argv*))
	  fasl)
      (if (and first
	       (scan "\\.lisp$" first)
	       (> (file-write-date first) *version*))
	  (progn
	    (setf fasl (replace (copy-seq first) ".fasl" :start1 (- (length first) 5)))
	    (if (probe-file fasl)
		(delete-file fasl))
	    (load first)
	    (main2)
	    (sb-ext:save-lisp-and-die
	     #+sbcl
	     (car sb-ext:*posix-argv*)
	     :toplevel #'cfy.po2db:main :executable t)
	    )
	  (main2))))
  (hot-update))
;; compile as elf
;; (declaim (optimize (speed 3)(debug 0)(space 3)))
;; (load "/home/cfy/gits/po2db/po2db.lisp")
;; (save-lisp-and-die "po2db" :toplevel #'cfy.po2db:main :executable t)
