# pacotest 0.2.1
## Bug fixes
* #14: removed calls of floating-point functions on integers (caused installation failures on solaris)
* #15 prevent nan's in unsigned int, which is outside the range of representable values (caused memtest note on CRAN)
* #16 added a side argument to calls of grad
* #17 completed the omega matrix for asmpt. with ranks