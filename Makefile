NAME=bboxada

-include Makefile.conf

STATIC_MAKE_ARGS = $(MAKE_ARGS) -XBBOX_LIBRARY_TYPE=static
SHARED_MAKE_ARGS = $(MAKE_ARGS) -XBBOX_LIBRARY_TYPE=relocatable
SHARED_MAKE_ARGS += -XUTILADA_BASE_BUILD=relocatable -XUTIL_LIBRARY_TYPE=relocatable
SHARED_MAKE_ARGS += -XXMLADA_BUILD=relocatable
SHARED_MAKE_ARGS += -XLIBRARY_TYPE=relocatable

include Makefile.defaults

build:: tools

tools:
	$(GNATMAKE) $(GPRFLAGS) -p -P$(NAME)_tools $(MAKE_ARGS)

$(eval $(call ada_library,$(NAME)))

.PHONY: tools
