# interview

## Depending on scapi

1. Update `scapiPath` in `build.sbt` to point to where you have installed scapi—if you already have it installed. If not, see below.

  * Also, make sure, you have all the compiled `.so`’s loadable (e.g. in this app’s `LD_LIBRARY_PATH`).

1. Compiling scapi 2.4.0 failed with openjdk8 for me. With openjdk7, it works.

1. After updating submodules, I had to do this to `lib/OTExtension/` (syntax incompatibility with GNU Make?):

```diff
diff --git a/Makefile b/Makefile
index a1bc51f..18cf055 100644
--- a/Makefile
+++ b/Makefile
@@ -45,13 +45,13 @@ otlib: ${OBJECTS_UTIL} ${OBJECTS_OT}
 otmain: ${OBJECTS_UTIL}  ${OBJECTS_OT} ${OBJECTS_OTMAIN}
        ${CXX} -o ot.exe $(INCLUDE) ${CFLAGS} ${OBJECTS_OTMAIN} ${OBJECTS_UTIL} ${OBJECTS_OT} ${LIBRARIES} ${COMPILER_OPTIONS}

-${OBJECTS_OTMAIN}: ${SOURCES_OTMAIN}$
+${OBJECTS_OTMAIN}: ${SOURCES_OTMAIN}
        @cd mains; ${CXX} -c ${INCLUDE} ${CFLAGS} otmain.cpp

-${OBJECTS_UTIL}: ${SOURCES_UTIL}$
+${OBJECTS_UTIL}: ${SOURCES_UTIL}
        @cd util; ${CXX} -c ${CFLAGS} ${INCLUDE} ${BATCH} *.cpp

-${OBJECTS_OT}: ${SOURCES_OT}$
+${OBJECTS_OT}: ${SOURCES_OT}
        @cd ot; ${CXX} -c ${CFLAGS} ${INCLUDE} ${BATCH} *.cpp

 install:
```
