

# don't c-compile anything on win32 (yet)
# I don't know how to make a linked in driver using gcc, or any
# other compiler there, and have no intentions of finding out

PRIV_FILES = ../priv/lib/yaws_sendfile_drv.$(DLL)


CFLAGS += -I"${ERLDIR}/usr/include"

all: $(PRIV_FILES)
debug:
	CFLAGS="$(CFLAGS) -g" $(MAKE) all

clean:
	-rm -f yaws_sendfile_drv.$(OBJ) hashtable.$(OBJ)

install:	$(PRIV_FILES)
	$(INSTALL) -d $(DESTDIR)$(PREFIX)/lib/yaws/priv/lib
	$(INSTALL) $(PRIV_FILES) $(DESTDIR)$(PREFIX)/lib/yaws/priv/lib

../priv/lib/yaws_sendfile_drv.$(DLL): yaws_sendfile_drv.$(OBJ) hashtable.$(OBJ)
	$(LD_SHARED) $(OUT) $@ $^ $(DLL_LIBS)
yaws_sendfile_drv.$(OBJ):	yaws_sendfile_drv.c
	$(CC) -c $(FPIC) $(CFLAGS) -DDYNAMIC_DRIVER $<
hashtable.$(OBJ):	hashtable.c
	$(CC) -c $(FPIC) $(CFLAGS) $<

../priv/.foo:
	touch $@
