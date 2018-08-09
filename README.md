1. ghc cabal-install
2. cabal update
3. cabal install



## How it works

It's basically a script to invoke cmake.

### Init

It downloads and extracts the Android NDK to the folder 'output'.
It then converts all ndk toolchains specified in 'config.json' to a standalone toolchain.

Targeting 'armeabi-v7a' and 'x86' will run on most devices.

### Build

It will invoke cmake ones for every target specified.

### GDB

It works only on Android.
If you need to debug from the moment the app stops, you could put a breakpoint at the start of the app. Then invoke gdb.

python must be installed for make_standalone_toolchain.py
