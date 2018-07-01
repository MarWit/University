#include <stdio.h>
#include <inttypes.h>

#define _64BIT

// inline unsigned int
// rotate( int a )
// __asm__(
//     "glob _rotate\t\n"
//     "_rotate:\t\n"
//     "pop edx\t\n"
//     "ror edx, 8\t\n"
//     "push edx"
//     "ret"
// );

int
main(void) {
#ifndef _64BIT
    unsigned int a = 0x12345678;
    unsigned int b = 0;
    __asm__(
            "push ebx\n\t"
            "mov ebx, %1\n\t" // 0x12345678
            "rol bx, 8\n\t"
            "rol ebx, 16\n\t" // 0x45671234
            "rol bx, 8\n\t"
            "mov %0, ebx\n\t"
            "pop ebx\n\t"
            : "=r"(b)
            : "r"(a)
    );
#else
    uint64_t a = 0x1122334455667788;
    uint64_t b = 0;
    __asm__(
            "push rbx\n\t"
            "mov rbx, %1\n\t"   // 0x1122334455667788
            "add ebx, 1\n\t"
            /* "rol bx, 8\n\t"     // 0x1122334455668877 */
            /* "rol ebx, 16\n\t"   // 0x1122334488775566 */
            /* "rol bx, 8\n\t"     // 0x1122334488776655 */
            /* "rol rbx, 32\n\t"   // 0x8877665511223344 */
            /* "rol bx, 8\n\t"     // 0x8877665511224433 */
            /* "rol ebx, 16\n\t"   // 0x8877665544331122 */
            /* "rol bx, 8\n\t"     // 0x8877665544332211 */
            "mov %0, rbx\n\t"
            "pop rbx\n\t"
            : "=r"(b)
            : "r"(a)
    );
#endif

    printf( "%lx\n", b );

    return 0;
}

