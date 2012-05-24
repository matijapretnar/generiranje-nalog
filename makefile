WWWDIR = ~/Sites/matija.pretnar.info/mathematica
WWWFILES = generiranje-nalog.zip

ZIPFILE = generiranje-nalog.zip
ZIPFILES = generiranje.nb studentje.csv vzorec.tex

default: $(WWWFILES)
	cp $(WWWFILES) $(WWWDIR)

$(ZIPFILE): $(ZIPFILES)
	zip $(ZIPFILE) $(ZIPFILES)

clean:
	rm -rf *.zip vzorec

.PHONY: default version

