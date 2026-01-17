#ifndef CONFIG_H
#define CONFIG_H

/* ================================
   User-configurable options
   ================================ */

/* Debug verbosity:
   0 = off
   1 = errors
   2 = warnings
   3 = info
   4 = trace
*/
#ifndef DEBUG_LEVEL
#define DEBUG_LEVEL 1
#endif

#ifndef WITH_MPI
#define WITH_MPI 0
#endif

#ifndef WITH_OPENMP
#define WITH_OPENMP 0
#endif

/* ================================
   Derived options (internal)
   ================================ */

#define ENABLE_DEBUG         (DEBUG_LEVEL > 0)
#define ENABLE_DEBUG_WARN    (DEBUG_LEVEL >= 2)
#define ENABLE_DEBUG_INFO    (DEBUG_LEVEL >= 3)
#define ENABLE_DEBUG_TRACE   (DEBUG_LEVEL >= 4)

#endif  /* CONFIG_H */
