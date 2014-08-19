
MMC=mmc

.PHONY : all
all: libmfcgi.so mfcgi_test

mfcgi_test : $(wildcard *.m)
	$(MMC) --make mfcgi_test

libmfcgi.so : $(wildcard *.m)
	$(MMC) --make libmfcgi

