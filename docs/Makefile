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

Version = 5.10.1

# The SHELL variable is used only for making the distribution.
SHELL = /bin/sh

# The 'rm' command used (we redefine it mostly because it may be
# aliased
RM = /bin/rm -f

DVIPS = dvips

DocFiles = ilisp.texi

# Debris (also known as temporary files) produced by texi2dvi

DVIDebris = *.aux *.cp *.cps *.fn *.fns *.ky *.kys *.log *.pg *.pgs \
            *.toc *.tp *.vr *.vrs

#==============================================================================
# Rules

docs: ps pdf info html

ps: ilisp.ps

ilisp.ps: ilisp.dvi
	$(DVIPS) -o $@ $<

dvi: ilisp.dvi

info: ilisp.info

html: ilisp.texi
	-texi2html $<

pdf: ilisp.pdf

ilisp.pdf: ilisp.ps
	-ps2pdf ilisp.ps ilisp.pdf

ilisp.dvi: ilisp.texi

clean: 
	-$(RM) $(DVIDebris) \
               ilisp.dvi \
               ilisp.ps \
               ilisp*.html \
               ilisp.info* \
               ilisp.pdf \
               *~

tmpclean:
	-$(RM) $(DVIDebris)


# end of file -- Makefile --
