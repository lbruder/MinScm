MinScm
======

An experimental Scheme subset interpreter in C++, based on SchemeNet.cs

Features: Tail calls, CL style macros, part of SRFI-1

Copyright (c) 2013, Leif Bruder <leifbruder@gmail.com>

This code will eventually evolve into a full-fledged Scheme environment based
on a portable C runtime and, running on top of it, an interpreter/compiler
combo written in Scheme. At the moment it is nothing more than a highly
experimental playground, so beware.

TODO: No reference counting or GC yet. Leaking memory like hell, but usable
for simple tasks.

Permission to use, copy, modify, and/or distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

