lisp = /usr/bin/sbcl

.PHONY : clean quicklisp
po2db : po2db.lisp
	$(lisp) --eval "(require 'asdf)" --eval "(require 'cl-ppcre)" --eval '(declaim (optimize (speed 3)(debug 0)(space 3)))' --eval '(load "po2db.lisp")' --eval "(save-lisp-and-die \"po2db\" :toplevel #'cfy.po2db:main :executable t)"

quicklisp : quicklisp.lisp
	$(lisp) --load quicklisp.lisp --eval '(quicklisp-quickstart:install)' --eval '(ql:quickload "cl-ppcre")' --eval '(ql:add-to-init-file)' --eval '(quit)'

quicklisp.lisp :
	wget 'http://beta.quicklisp.org/quicklisp.lisp'
clean :
	rm -f po2db.fasl po2db quicklisp.lisp
	$(info 'if you are not a common lisp user,you may want to rm -rf ~/quicklisp/')
