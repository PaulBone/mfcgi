
MMC=mmc

.PHONY : all
all: mfcgi_test

mfcgi_test : $(wildcard *.m)
	$(MMC) --make mfcgi_test

