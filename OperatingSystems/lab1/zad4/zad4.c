#define _GNU_SOURCE

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <unistd.h>
#include <execinfo.h>
#include <ucontext.h>
#include <sys/mman.h>

#ifdef REG_RIP
#define PROGRAM_COUNTER REG_RIP
#define STACK_POINTER REG_RSP
#else
#define PROGRAM_COUNTER REG_EIP
#define STACK_POINTER REG_ESP
#endif


void
segv_handler( int signum __attribute__((unused)), siginfo_t *si, void *ctx_p ) {
    char data[ 0x200 ] = {0};

    ucontext_t *ctx = (ucontext_t*) ctx_p;

    snprintf(
            data, 0xfff,
            "si_addr: 0x%p\n"
            "si_code: 0x%x (%s)\n"
            "Faulty instruction addr.: 0x%llx\n"
            "Stack addr.: 0x%llx\n"
            "\nBacktrace:\n",
            si -> si_addr, si -> si_code,
            si -> si_code == 1 ? "SEGV_MAPERR" : "SEGV_ACCERR",
            ctx -> uc_mcontext . gregs[ PROGRAM_COUNTER ],
            ctx -> uc_mcontext . gregs[ STACK_POINTER ]
    );

    write( 2, data, 0x1ff );

    void *bt[ 16 ];
    char **btsyms;
    int size = backtrace( bt, 16 );
    btsyms = backtrace_symbols( bt, size );

    for( int i = 0; i < size; i ++ ) {
        write( 2, btsyms[ i ], strlen( btsyms[ i ] ) );
        write( 2, "\n", 1 );
    }

    exit( EXIT_FAILURE );
}

void
usage( char *prog ) {
    printf( "Usage %s [0/1]\n", prog );
    printf( "\t0 - unmapped memory\n" );
    printf( "\t1 - read-only memory\n" );
}

int
main( int argc, char **argv ) {
    if( argc < 2 ) {
        usage( argv[ 0 ] );
        return 0;
    }

    struct sigaction new;
    new.sa_sigaction = segv_handler;
    sigemptyset( &new.sa_mask );
    new.sa_flags = SA_SIGINFO;
    sigaction( SIGSEGV, &new, NULL );

    if( ! strcmp( "0", argv[ 1 ] ) ) {
        // Unmapped memory
        int * mem = (int *) mmap( NULL, 16, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0 );
        munmap( mem, 16 );

        int fault __attribute__((unused)) = mem[ 0 ];
    } else if( ! strcmp( "1", argv[ 1 ] ) ) {
        // Read-only memory
        int * ro = (int *) mmap( NULL, 16, PROT_READ, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0 );
        *ro = 0x1234;
    } else usage( argv[ 0 ] );

    return 0;
}
