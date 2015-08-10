binarium
========

NOTE: This parser is aimed to be only used in networking applications hence its
preference for big endianness is hardcoded at the moment. I might deal with
this later but there's a big chance I won't since more flexibility it has the
slower it becomes. The solution to this problem might be implementing totally
different library with sole purpose of being slower than binarium but more
flexible (and probably suitable for file operations).

Fast binary types encoder and decoder. Aimed to be performant enough to be used
for heavily loaded network applications. At the moment of writing this text it
wasn't yet but it will be some day.

I've took some ideas from binary-types but most of the code was written from
scratch. No, I can't just use binary types because it lacks some important
features that would require a lot more work to implement so I decided to write
types coder by myself. And no, this is *not* reinvention of the wheel. This
specific code suites my task (almost) perfectly and it is also some kind of
exercise for me.

Anyway, don't take it too seriously and *don't* use it in production as it may
be slow, buggy and inefficient.

TODO
====

* [ ] Implement efficient bit flags decoder
* [ ] Implement "enums" supports (value to symbol mapping)
* [ ] Add padding support for composite objects
* [ ] Add support for object mapping to composite types
* [ ] Write more convenience macros
* [ ] Write better benchmarks

Usage
=====

The following types are shipped with binarium:

* Unsigned integer (8-bit, 16-bit, 32-bit, 64-bit, 128-bit)
* Signed integer (8-bit, 16-bit, 32-bit, 64-bit, 128-bit)
* VarNum (VarInt with 32-bit max, VarLong with 64-bit max)
* IEEE floating point numbers (32-bit single precision, 64-bit double precision)
* Char (8-bit)
* String (up to 2^32 8-bit characters with length encoded as VarInt)
* Boolean (8-bit since making it occupy less than one byte is costly)
* UUIDs (128-bit)
* Byte array (up to 2^32 octets with length encoded as VarInt)

Correspondingly, the following symbols are bound to above mentioned types:

* BINTYPE:U8, BINTYPE:U16, BINTYPE:U32, BINTYPE:U64, BINTYPE:U128
* BINTYPE:S8, BINTYPE:S16, BINTYPE:S32, BINTYPE:S64, BINTYPE:S128
* BINTYPE:VAR-INT, BINTYPE:VAR-LONG
* BINTYPE:F32, BINTYPE:F64
* BINTYPE:CHAR
* BINTYPE:STRING
* BINTYPE:BOOL
* BINTYPE:UUID
* BINTYPE:BYTE-ARRAY

In order to use binarium you must first create fast-io buffer. This can be done manually or using the following macros:

```lisp
;; Input buffer
(binarium:with-binary-input (buffer binary-data)
  ;; Your code
  )

;; Output buffer
(binarium:with-binary-output (buffer)
  ;; Your code
  )
```

The encoding and decoding is done though the following calls:

```lisp
;; Encoding
;; * "type" is either one of built-in types or user-defined type
;; * "data" is the data you want to write
(binarium:with-binary-output (buffer)
  (binarium:write-binary-type type data buffer))

;; Decoding
;; * "type" is either one of built-in types or user-defined type
;; * "data" is the data you want to read
(binarium:with-binary-input (buffer data)
  (binarium:read-binary-type type buffer))
```

Composite types
===============

Composite types are a way to pack a few basic (or even other composite) types
together as an atomic type.

Now they aren't much but I plan to make their parsing more efficient by
precompiling parsers for them instead of iterating over their structure.

You can define composite types using BINARIUM:DEFINE-COMPOSITE-TYPE macro:

```lisp
(binarium:define-composite-type my-composite-type
  (bintype:u2 a-number)
  (bintype:string a-string))
```

The type can then be used as usual with an exception that it's represented as
list with elements ordered according to the order of types supplied in
definition.

Currently, the name of field in composite type is unused but I plan to make
composite types decode to instances of the corresponding classes with slots
bound to field values.

If the composite types decoder/encoder encounters invalid composite type
structure, INVALID-STRUCTURE condition is signaled.

Extending default types
=======================

If you want to extend default binarium types, the following classes are available:

*  BINTYPE:UNSIGNED-INTEGER
*  BINTYPE:SIGNED-INTEGER
*  BINTYPE:VAR-NUM
*  BINTYPE:FLOAT
*  BINTYPE:CHARACTER
*  BINTYPE:STRING
*  BINTYPE:BOOLEAN
*  BINTYPE:UUID
*  BINTYPE:BYTE-ARRAY

If you want to define your own types the following classes may aid you:

* BINARIUM:BINARY-TYPE
* BINARIUM:BINARY-ARRAY

* BINARIUM:BASIC-TYPE
* BINARIUM:COMPOSITE-TYPE

The following macros can be used to simplify defining your own types:

* BINARIUM:DEFINE-BINARY-TYPE
* BINARIUM:DEFINE-BINARY-ARRAY

For more information, see sources or documentation strings.

Benchmarks
==========

Currently, only benchmarking of composite type decoder was done. For more
information please refer to source code which is located at benchmark
directory.

Benchmarking itself was done using trivial-benchmark (for which I would like
to thank its developer, Nicolas; it's a great tool).

```
Encoding:
-                COUNT  TOTAL     MINIMUM  MAXIMUM   MEDIAN  AVERAGE   DEVIATION
REAL-TIME        10000  0.193     0        0.001     0       0.000019  0.000138
RUN-TIME         10000  0.14      0        0.007     0       0.000014  0.000235
USER-RUN-TIME    10000  0.13      0        0.006667  0       0.000013  0.000228
SYSTEM-RUN-TIME  10000  0.02      0        0.006667  0       0.000002  0.000094
PAGE-FAULTS      10000  0         0        0         0       0         0.0
GC-RUN-TIME      10000  0         0        0         0       0         0.0
BYTES-CONSED     10000  37969040  0        32768     0       3796.904  10486.686
EVAL-CALLS       10000  0         0        0         0       0         0.0
```

```
Decoding:
-                COUNT  TOTAL     MINIMUM  MAXIMUM   MEDIAN  AVERAGE    DEVIATION
REAL-TIME        10000  0.177     0        0.016     0       0.000018   0.000203
RUN-TIME         10000  0.211     0        0.017     0       0.000021   0.000331
USER-RUN-TIME    10000  0.18667   0        0.016666  0       0.000019   0.000316
SYSTEM-RUN-TIME  10000  0.026665  0        0.006667  0       0.000003   0.000105
PAGE-FAULTS      10000  0         0        0         0       0          0.0
GC-RUN-TIME      10000  0.034     0        0.017     0       0.000003   0.00024
BYTES-CONSED     10000  49794736  0        32768     0       4979.4736  11761.419
EVAL-CALLS       10000  0         0        0         0       0          0.0
```

Some time later I plan to add comparison to other binary decoding libraries and
probably write more advanced tests to put the code into environment close to
real-life situation.
