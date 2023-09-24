# Forth for the 8008 CPU
Scott Baker https://www.smbaker.com/

This is a rewrite of Jonesforth for the 8008 CPU.

Jonesforth is written for the x86 architecture, which has significantly more
registers than the 8008, so a direct port is challenging. 

## Building

This project is buildable in a number of configurations, most of them for my H8 8008 CPU board:

* `forth-scelbi.bin`. Intended to be used with the SIMH scelbi simulator. Starts at offset 0 and a go with PC set to 0 should run it.

* `forth-16450.bin`. For Scott's H8 8008 CPU Board in conjunction with an H8 16450 serial board, like the H8-4 board.

* `forth-8251.bin`. For Scott's H8 8008 CPU Board in conjunction with an H8 8251 serial board, like the H8 Tape Board.

* `forth-bitbang.bin`. For Scott's H8 8008 CPU Board using the onboard bitbang port. Usable in a SBC configuration (i.e. without H8). May also work with Jim Loos's 8008-SBC

To build, you will need the AS assembler. Whereas prebuilt binaries exist for AS for Windows, it also builds easily for Linux.

## Tests

The tests are done using SIMH together with a few hacks to cause it to read terminal input from a file.

## Notes

 1. I thought I would be clever and try to use D and E as my datastack 
    and returnstack pointers. This turned out to be not so clever as I
    probably wasted more time preserving these registers and working with
    a smaller set of registers than it would have taken to simply put
    these pointers in memory variables.

 2. The second argument for multiplication and division must be 8-bits.
    For example "1000 250 /" is fine and "1000 250 *" is fine, but
    "1000 256 /" or "1000 256 *' are a no-no. It's an exercise for the
    reader to do better...
