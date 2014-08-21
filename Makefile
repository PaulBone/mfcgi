
MMC=mmc

.PHONY : all
all: libmfcgi.so mfcgi_test mfcgi_threadtest

mfcgi_test : $(wildcard *.m)
	$(MMC) --make mfcgi_test

mfcgi_threadtest : $(wildcard *.m)
	$(MMC) --make mfcgi_threadtest

libmfcgi.so : $(wildcard *.m)
	$(MMC) --make libmfcgi
