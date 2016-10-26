# interview

## Potential issues

1. The Cookbook, in “Parties’ inputs”, lists:

      > Common input statement: public key *n* and ciphertexts *c₁*, *c₂*, *c₃*.

   Therefore, to perform verification, all three ciphertexts have to be encrypted with the same public key *n*. For this purpose, all parties are using the `Main.verificationPublicKey`. It’s private counterpart is forgotten and never used.

1. The protocol lacks sending `r_1`, `r_2`, `r_3`, the random values used in encryption of `c_A`, `c_B`, `c_C`. These values are needed for product verification later on. I’m sending them in a product type, along the ciphertexts.

1. The protocol also lacks a method for choosing which users take part in proving a product. Currently, `Main` hardcodes a pool of just two users, for the sake of example.

1. I’m using `ActorRef`s to identify parties (and *not* to directly communicate, bypassing `Environment`). They are already indentified by these values and I didn’t want to unnecessarily complicate the code, by introducing some artificial identifiers. Trivial to fix, if needed.

1. Broker sends its public key to parties during invitation. The public keys are then trusted by the parties… And this is prone to a MiTM attack. The public keys should be signed by some authority. Or, alternatively, we can Trust Upon First Use.

1. Ad point 4 of the spec: to share the secrets in an encrypted form with the broker—after her announcement of both `c_A` and `c_B`—I’ve chosen Damgård–Jurik again, as we already have the keys and ‘infrastructure’ set up.

1. Error handling (`git-grep` for “wicked situation”). Also, if a party gets some malformed ciphertext and then tries to decrypt it, the process might throw.

1. Currently, the parties are not able to perform more than one verification concurrently (within *one* party). Easy to fix, but I don’t know if this is required and fixing would further complicate the code.

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
