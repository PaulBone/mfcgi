
MMC=mmc --use-grade-subdirs
MTAGS=mtags

# Currently this is the most suitable parallel grade for this application as
# IO and concurrency don't interact well on the low level C backend.
PARALLEL_GRADE=hlc.gc.par

.PHONY : all
TARGETS = libmfcgi.so mfcgi_test mfcgi_threadtest
all: $(TARGETS)

mfcgi_test : $(wildcard *.m)
	$(MMC) --make mfcgi_test

mfcgi_threadtest : $(wildcard *.m)
	$(MMC) --make --grade $(PARALLEL_GRADE) mfcgi_threadtest

# This library is still built in the default grade.  TODO: build this
# library in mulitple grades.
libmfcgi.so : $(wildcard *.m)
	$(MMC) --make libmfcgi

tags : $(wildcard *.m)
	$(MTAGS) $(wildcard *.m)

.PHONY : clean
clean:
	rm -rf $(TARGETS) *.so *.a *.mh *.err *.init Mercury tags

