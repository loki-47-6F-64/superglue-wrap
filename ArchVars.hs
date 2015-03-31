{-# LANGUAGE CPP #-}

module ArchVars (osArch) where


#if linux_HOST_OS == 1 && x86_64_HOST_ARCH == 1
#define OS_ARCH "Linux 64-bit (x86)"
#elif linux_HOST_OS == 1 && i386_HOST_ARCH == 1
#define OS_ARCH "Linux 32-bit (x86)"
#elif darwin_HOST_OS == 1 && x86_64_HOST_ARCH == 1
-- 'Mac OS X 64-bit' is missing a <tr></tr>
#define OS_ARCH "Mac OS X 32-bit"
#elif darwin_HOST_OS == 1 && x86_64_HOST_ARCH == 1
#define OS_ARCH "Mac OS X 32-bit"
#else
#define OS_ARCH error "Combination of OS and Architecture not supported"
#endif


osArch :: String
osArch = OS_ARCH
