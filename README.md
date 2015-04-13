1. brew install ghc cabal-install
2. ghc-pkg describe rts > rts.pkg
3. In your favorite editor: append "/usr/local/lib" to include-dirs
4. ghc-pkg update rts.pkg
5. cabal install
