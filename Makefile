
MMC=mmc
MTAGS=mtags

.PHONY : all
TARGETS = libmfcgi.so mfcgi_test mfcgi_threadtest
all: $(TARGETS)

mfcgi_test : $(wildcard *.m)
	$(MMC) --make mfcgi_test

mfcgi_threadtest : $(wildcard *.m)
	$(MMC) --make mfcgi_threadtest

libmfcgi.so : $(wildcard *.m)
	$(MMC) --make libmfcgi

tags : $(wildcard *.m)
	$(MTAGS) $(wildcard *.m)

.PHONY : clean
clean:
	rm -rf $(TARGETS) *.so *.a *.mh *.err *.init Mercury tags

