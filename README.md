1. Go to root of the project
2. brew install ghc cabal-install

### If you're running on the mac, make sure to follow these instructions
3. ghc-pkg describe rts > rts.pkg
4. open rts.pkg in any text editor: append "/usr/local/lib" to include-dirs
5. ghc-pkg update rts.pkg


6. cabal update
7. cabal install



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
