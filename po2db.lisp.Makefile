lisp = /usr/bin/sbcl

.PHONY : clean
po2db : ~/quicklisp/setup.lisp po2db.lisp
	$(lisp) --no-userinit --load ~/quicklisp/setup.lisp --eval '(declaim (optimize (speed 3)(debug 0)(space 3)))' --eval '(load "/home/cfy/gits/po2db/po2db.lisp")' --eval "(save-lisp-and-die \"po2db\" :toplevel #'cfy.po2db:main :executable t)"

~/quicklisp/setup.lisp : quicklisp.lisp
	$(lisp) --load quicklisp.lisp --eval '(quicklisp-quickstart:install)' --eval '(quit)'

quicklisp.lisp :
	wget 'http://beta.quicklisp.org/quicklisp.lisp'
clean :
	rm -f po2db.fasl po2db quicklisp.lisp
	$(info 'if you are not a common lisp user,you may want to rm -rf ~/quicklisp/')
