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

* BINTYPE:U1, BINTYPE:U2, BINTYPE:U4, BINTYPE:U8, BINTYPE:U16
* BINTYPE:S1, BINTYPE:S2, BINTYPE:S4, BINTYPE:S8, BINTYPE:S16
* BINTYPE:VAR-INT, BINTYPE:VAR-LONG
* BINTYPE:F4, BINTYPE:F8
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
