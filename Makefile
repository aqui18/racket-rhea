CXXFLAGS=-std=c++11 -pedantic -Wall -Wno-strict-aliasing -O3 -fPIC
RHEAFLAGS=-Irhea/rhea -Lext -lrhea

# configure shared library options and native binary names

# defaults for Linux
SOEXT=so
LDFLAGS=-shared -Wl,-soname,librheaffi.$(SOEXT)

BUILD_OS := $(strip $(shell uname -s | tr '[:upper:]' '[:lower:]'))
OS ?= $(BUILD_OS)

# Default value of $OS on Windows is Windows_NT
ifneq ($(findstring cygwin, $(BUILD_OS)),)
  OS = Windows_NT
endif

ifeq ($(OS), Windows_NT)
  SOEXT=dll
  LDFLAGS += -static-libgcc \
	-Wl,--enable-auto-image-base,--enable-auto-import,--add-stdcall-alias
endif

ifeq ($(OS), darwin)
  SOEXT=dylib

  CPU ?= $(shell uname -m | sed -e 's/i[345678]86/i386/' | sed -e 's/amd64/x86_64/')
  ARCHES =
  ifneq ($(findstring $(CPU), ppc powerpc),)
    ARCHES += ppc
  endif
  ifneq ($(findstring $(CPU), i386 x86_64),)
    ARCHES += i386 x86_64
  endif

  # First try to locate a legacy Xcode 3 install, so a universal binary (ppc +
  # intel) can be built
  ifneq ($(realpath /Xcode3/usr/bin/gcc),)
    XCODE=/Xcode3
  else
    XCODE=$(shell xcode-select -print-path)
  endif

  UNIVERSAL_SDK = $(firstword $(strip $(realpath $(XCODE)/SDKs/MacOSX10.5.sdk)))
  ifneq ($(UNIVERSAL_SDK),)
    MACSDK = $(UNIVERSAL_SDK)
    ifeq ($(findstring ppc, $(ARCHES)),)
      ARCHES += ppc
    endif
  else
    MACSDK = $(firstword $(strip $(wildcard $(XCODE)/SDKs/MacOSX10.*.sdk)) $(strip $(wildcard $(XCODE)/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.*.sdk)))
  endif

  CXXFLAGS += -DTARGET_RT_MAC_CFM=0
  LDFLAGS = $(foreach arch, $(ARCHES),-arch $(arch)) -dynamiclib \
	-Wl,-syslibroot,$(MACSDK) -mmacosx-version-min=10.5
endif

default: ext/librheaffi.$(SOEXT)

ext/librhea.$(SOEXT): rhea/rhea/*
	mkdir -p ext
	git subtree pull -P rhea https://github.com/Nocte-/rhea.git master || test -d rhea/rhea
	mkdir -p rhea/build && cd rhea/build && cmake .. && make
	cp -L rhea/build/rhea/librhea.$(SOEXT) ext/librhea.$(SOEXT)

ext/librheaffi.so: src/rhea_ffi.cpp ext/librhea.$(SOEXT)
	$(CXX) $(CXXFLAGS) $(LDFLAGS) $(RHEAFLAGS) -o $@ $<

clean:
	rm -rf rhea/build
	rm -rf ext
