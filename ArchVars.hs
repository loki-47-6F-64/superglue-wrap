{-# LANGUAGE CPP #-}

module ArchVars (osArch, darwin, linux) where


#if linux_HOST_OS == 1 && x86_64_HOST_ARCH == 1
#define OS_ARCH linux
#elif darwin_HOST_OS == 1 && x86_64_HOST_ARCH == 1
#define OS_ARCH darwin
#else
#define OS_ARCH error "Combination of OS and Architecture not supported"
#endif


darwin = "darwin-x86_64"
linux  = "linux-x86_64"

osArch :: String
osArch = OS_ARCH
