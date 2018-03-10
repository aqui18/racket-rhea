SOEXT=so
OS := $(strip $(shell uname -s | tr '[:upper:]' '[:lower:]'))

ifneq ($(findstring cygwin, $(OS)),)
  SOEXT=dll
endif

ifeq ($(OS), Windows_NT)
  SOEXT=dll
endif

ifeq ($(OS), darwin)
  SOEXT=dylib
endif

default: extern/librheaffi.$(SOEXT)

extern/librheaffi.$(SOEXT): src/rhea_ffi.cpp
	mkdir -p ext
	git clone rhea https://github.com/Nocte-/rhea.git || test -d rhea/rhea
	cp $^ rhea/rhea/
	mkdir -p rhea/build && cd rhea/build && cmake .. && make rhea
	cp -L rhea/build/rhea/librhea.$(SOEXT) extern/librhea.$(SOEXT)

clean:
	rm -f rhea/rhea/rhea_ffi.cpp
	rm -rf rhea/build
	rm -rf extern

update_rhea:
	rm -f rhea/rhea/rhea_ffi.cpp
	cd rhea && git pull
	test -d rhea/rhea
