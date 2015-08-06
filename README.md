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
