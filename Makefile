# -*- Mode: Makefile -*-

# Makefile --
# This file is part of ILISP.
# Version: 5.10.1
#
# Copyright (C) 1990, 1991, 1992, 1993 Chris McConnell
#               1993, 1994 Ivan Vasquez
#               1994, 1995, 1996 Marco Antoniotti and Rick Busdiecker
#               1996-2000 Marco Antoniotti and Rick Campbell
#
# Send mail to 'majordomo@cons.org' to be included in the
# ILISP mailing list.

# Note: this makefile assumes GNU make

#==============================================================================
# Various Variables

Version = 5.10.x

# Use whichever you like most
#EMACS = xemacs
#EMACS = /usr/local/bin/emacs
EMACS = emacs

# The SHELL variable is used only for making the distribution.
SHELL = /bin/csh

# The 'rm' command used (we redefine it mostly because it may be
# aliased
RM = /bin/rm -f


# These are used mostly for packaging the distribution
Ilisp_src_dir = $(shell pwd)
Ilisp_tar_dir = ilisp-$(Version)

OtherFiles = README         \
             HISTORY        \
             Makefile       \
             ilisp.emacs    \
             INSTALLATION   \
             COPYING        \
             GETTING-ILISP  \
             Welcome

LoadFiles = custom-ilisp.elc ilisp-def.elc ilisp-sym.elc \
 ilisp-inp.elc ilisp-ind.elc ilisp-prc.elc ilisp-val.elc ilisp-out.elc \
 ilisp-mov.elc ilisp-key.elc ilisp-prn.elc ilisp-low.elc ilisp-doc.elc \
 ilisp-ext.elc ilisp-mod.elc ilisp-dia.elc ilisp-cmt.elc ilisp-rng.elc \
 ilisp-hnd.elc ilisp-utl.elc ilisp-cmp.elc ilisp-kil.elc ilisp-snd.elc \
 ilisp-xfr.elc ilisp-hi.elc ilisp-aut.elc \
 ilisp-cl.elc ilisp-cmu.elc ilisp-sbcl.elc \
 ilisp-acl.elc ilisp-kcl.elc ilisp-luc.elc ilisp-sch.elc ilisp-hlw.elc \
 ilisp-xls.elc ilisp-chs.elc


DocFiles = docs/Makefile \
	   docs/README \
	   docs/doc-changes.txt \
           docs/ilisp-refcard.tgz \
           docs/ilisp.texi

#==============================================================================
# Rules

compile:
	$(EMACS) -batch -l ilisp-mak.el

tags:
	etags *.el

docs: FORCE
	cd docs; $(MAKE)

clean: 
	-$(RM) *.elc *~ extra/*.elc extra/*~
	(cd docs; $(MAKE) clean)

loadfile:
	touch ilisp-all.elc
	cat $(LoadFiles) > ilisp-all.elc
	$(RM) $(LoadFiles)
# Note that the redirection is done by a Bourne Shell.

compress:
	gzip *.el $(OtherFiles)

FORCE:

#==============================================================================
# The following targets are used only to create a distribution file.

dist: tarring dist_compressing

tarring:
	@echo "ILISP dist: preparing tar file."
	@echo "            source directory: " $(Ilisp_src_dir)
	@echo "            tar directory:    " $(Ilisp_tar_dir)
	(cd $(Ilisp_src_dir)/..;                                        \
         if ( $(notdir $(Ilisp_src_dir)) != $(Ilisp_tar_dir) )          \
            ln -s $(notdir $(Ilisp_src_dir)) $(Ilisp_tar_dir) ;         \
         tar cvf $(Ilisp_tar_dir).tar                                   \
            $(patsubst %,$(Ilisp_tar_dir)/%,$(OtherFiles))              \
            $(Ilisp_tar_dir)/*.el                                       \
            $(Ilisp_tar_dir)/*.lisp                                     \
            $(patsubst %,$(Ilisp_tar_dir)/%,$(DocFiles))                \
            $(Ilisp_tar_dir)/extra/README                               \
            $(Ilisp_tar_dir)/extra/hyperspec-*.el                       \
            $(Ilisp_tar_dir)/extra/hyperspec.el                         \
            $(Ilisp_tar_dir)/pictures/ilisp-icon.*                      \
        )

dist_compressing:
	(cd $(Ilisp_src_dir)/.. ; gzip $(Ilisp_tar_dir).tar)

uuencoding: ../$(Ilisp_tar_dir).tar.gz
	(cd $(Ilisp_src_dir)/.. ;                                           \
         uuencode $(Ilisp_tar_dir).tar.gz $(Ilisp_tar_dir).tar.gz > il.uue)


# end of file -- Makefile --
