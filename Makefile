.PHONY: clean

phash.py: phash.fut
	futhark pyopencl $< --library

clean:
	@rm -rf phash phash.c phash.py
