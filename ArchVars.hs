{-# LANGUAGE CPP #-}

module ArchVars (osArch) where


#if linux_HOST_OS   == 1 && x86_64_HOST_ARCH == 1
#define OS_ARCH "Linux 64-bit (x86)"
#elif linux_HOST_OS == 1 && i386_ARCH_OS == 1
#define OS_ARCH "Linux 32-bit (x86)"
#else
#define OS_ARCH error "Combination of OS and Architecture not supported"
#endif


osArch :: String
osArch = OS_ARCH
