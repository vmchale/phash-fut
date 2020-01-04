.PHONY: clean

phash.py: phash.fut
	futhark python $< --library

clean:
	@rm -rf phash phash.c phash.py
