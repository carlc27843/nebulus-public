### About

This repository contains snapshots of intermediate lua files from the build process for [PICO-8 Nebulus](https://carlc27843.itch.io/nebulus).

It should give a somewhat readable insight into how Nebulus is coded. However it does not contain the tools, build scripts or source data needed to actually build the game.

Note that PICO-8 uses a customized version of Lua 5.2 with modified syntax such as short-if/whiles. Also PICO-8's token limits, code size constraints and memory mapped APIs inevitably lead to code golfing with heavy use of globals, undeclared variables, _ENV manipulation, obscure pokes, etc that purists wouldn't necessarily consider the "cleanest" of code.

To add to the obscurity, the Nebulus build process extends PICO-8 syntax with macro invocations inside backticks, allowing build steps to insert (or remove) customized code. Further, Nebulus uses [Picoscript](https://carlc27843.github.io/post/picoscript/) to store some of the code logic as data in ROM space to get around PICO-8's token and code size limits, which required a peculiar brand of code gymnastics.

Lastly, this code wasn't written with an audience in mind, so expect a stream of consciousness of FIXMES/TODOS/DEBUGs and temporary code etc.

Good luck!

### PICO-8 Lua Files

[out/nebulus-source.lua](out/nebulus-source.lua)
  - The hand-written code concatenated from all lua source files.

[out/nebulus-expanded.lua](out/nebulus-expanded.lua)
  - The code after macro expansion.

[out/nebulus-minified.lua](out/nebulus-minified.lua)
  - The code after a custom minifier, as it appears in the final cart. 
  - Minification was needed to slide in under PICO-8's code size limit.

