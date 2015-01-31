
# This pathname must be less than 27 characters long.
S48VM=/usr/local/lib/scheme48/scheme48vm
# use my enhanced sockets version:
# S48VM=/zu/danw/bin/s48.39
MUSEME=/usr/home/dougo/scheme48/museme

server: *.scm
	@echo ""
	@echo $@
	@(echo ,bench; echo ,keep tabulate; echo ,flush source maps; \
          echo ,config ,load packages.scm; echo ,open server; \
	  echo ,load-package museme-scheme; \
	  echo ,load-package museme-mud-environment; \
	  echo ,flush; \
          echo ,build script-entry $@.image) \
	| $(S48VM) -a batch
	mv server.image server-root/
	@(echo '#!/bin/csh -f'; echo cd $(MUSEME)/server-root; \
	  echo '$(S48VM) -i server.image') > $@
	chmod +x $@
	ps augxww | grep server | grep -v grep

## BJR: try to use the system in single user mode for simpler debuging
single-user: *.scm
	@echo ""
	@echo $@
	@(echo ,bench; echo ,keep tabulate; echo ,flush source maps; \
          echo ,config ,load su-packages.scm; echo ,open single-user; \
	  echo ,load-package museme-scheme; \
	  echo ,load-package museme-mud-environment; \
	  echo ,flush; \
          echo ,build entry-point $@.image) \
	| $(S48VM) -a batch
	mv single-user.image server-root/
	@(echo '#!/bin/csh -f'; echo cd $(MUSEME)/server-root; \
	  echo '$(S48VM) -i single-user.image') > $@
	chmod +x $@

clean:
	rm -f server *~

