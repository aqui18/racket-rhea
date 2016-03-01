# configure shared library options and native binary names

# defaults for Linux
SOEXT=so
OS := $(strip $(shell uname -s | tr '[:upper:]' '[:lower:]'))

# Default value of $OS on Windows is Windows_NT
ifneq ($(findstring cygwin, $(OS)),)
  SOEXT=dll
endif

ifeq ($(OS), Windows_NT)
  SOEXT=dll
endif

ifeq ($(OS), darwin)
  SOEXT=dylib
endif

default: ext/librheaffi.$(SOEXT)

ext/librheaffi.$(SOEXT): src/rhea_ffi.cpp
	mkdir -p ext
	git subtree pull -P rhea https://github.com/Nocte-/rhea.git master || test -d rhea/rhea
	cp $^ rhea/rhea/
	mkdir -p rhea/build && cd rhea/build && cmake .. && make rhea
	cp -L rhea/build/rhea/librhea.$(SOEXT) ext/librhea.$(SOEXT)

clean:
	rm -f rhea/rhea/rhea_ffi.cpp
	rm -rf rhea/build
	rm -rf ext

update_rhea:
	rm -f rhea/rhea/rhea_ffi.cpp
	git subtree pull -P rhea https://github.com/Nocte-/rhea.git master || test -d rhea/rhea
