# spectrum-scubadive-rev
Reversing **Scuba Dive** game for ZX Spectrum, 1983 Durell Software.

Status: work in progress.

## Original

![](images/loading.png)

The original binary used for the disasm is .TAP file from [World of Spectrum](https://worldofspectrum.org/archive/software/games/scuba-dive-durell-software-ltd), 39428 bytes length.

.TAP file contains four blocks:
```
   Name         Type   Addr    Size  SizeHex   RangeHex
  "SCUBA     "  BASIC 
  "DIVE      "  CODE   16384,  7150   $1BEE   $4000-$5BED
  "S         "  CODE   24576, 25856   $6500   $6000-$C4FF
  "D         "  CODE   55552,  6144   $1800   $D900-$F0FF
```

The game labyrinth "warped up":
first, the AC5D table (the "mini-map") contains 32x32 = 256 block indices;
second, blocks at A4DD contains 8x8 tiles each;
and finally, tiles at 9134 are 8x8 pixels each.
So the whole world is 256x256 tiles = 2048x2048 pixels.
We always see only 24x24 tiles on the screen.

Looks like the map created in a procedural way, changing block numbers in the AC5D table.
The map depth depends on the game level 1..4.

There's an interesting way to put a big Octopus on the map.
The Octopus always guarding a way leading down the map.
So when we need an Octopus, we just put blocks number $1C and $1D next to each other, and that's used as a placeholder for the Octopus.


## Tools for the `tools` folder

 - `bas2tap.exe`, `bin2tap.exe` utilities
   https://sourceforge.net/projects/zxspectrumutils/files/

 - `pasmo.exe` cross-assembler
   http://pasmo.speccy.org/

## Links

 - [Scuba Dive disasm in SkoolKit HTML](https://nzeemin.github.io/skoolkit-game-revs/scubadive-zx/scuba/)
 - [nzeemin/skoolkit-game-revs repo for some game disasm using SkoolKit](https://github.com/nzeemin/skoolkit-game-revs)
