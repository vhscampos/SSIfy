##===- TEST.ssify.Makefile ------------------------------*- Makefile -*-===##
#
# Usage: 
#     make TEST=ssify summary (short summary)
#     make TEST=ssify (detailed list with time passes, etc.)
#     make TEST=ssify report
#     make TEST=ssify report.html
#
##===----------------------------------------------------------------------===##

CURDIR  := $(shell cd .; pwd)
PROGDIR := $(PROJ_SRC_ROOT)
RELDIR  := $(subst $(PROGDIR),,$(CURDIR))

LIBDIR	:= /home/vhscampos/Research/llvm/Debug+Asserts/lib
FLAGS	:= 1010

$(PROGRAMS_TO_TEST:%=test.$(TEST).%): \
test.$(TEST).%: Output/%.$(TEST).report.txt
	@cat $<

$(PROGRAMS_TO_TEST:%=Output/%.$(TEST).report.txt):  \
Output/%.$(TEST).report.txt: Output/%.linked.rbc $(LOPT) \
	$(PROJ_SRC_ROOT)/TEST.ssify.Makefile 
	$(VERB) $(RM) -f $@
	@echo "---------------------------------------------------------------" >> $@
	@echo ">>> ========= '$(RELDIR)/$*' Program" >> $@
	@echo "---------------------------------------------------------------" >> $@
	@-$(LOPT) -mem2reg -break-crit-edges -instnamer -load $(LIBDIR)/SSIfy.so \
	         -ssify -set $(FLAGS) -stats -time-passes -disable-output $< 2>>$@ 
summary:
	@$(MAKE) TEST=ssify | egrep '======|ssify -'

.PHONY: summary
REPORT_DEPENDENCIES := $(LOPT)
