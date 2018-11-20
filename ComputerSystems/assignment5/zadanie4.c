#define R 7
#define S 5
#define T 13

long A[R][S][T];

long
store_elem( long i, long j,         // rdi = i, rsi = j
        long k, long *dest ) {      // rdx = k, rcx = dest

    *dest = A[ i ][ j ][ k ];       // rax = 3 * j
                                    // rax = 4 * rax + j = ( 3 * j ) * 4 + j
                                    // j = i
                                    // j <<= 6
                                    // i += j
                                    // i += rax
                                    // k += i
                                    // rax = 8 * k + A
                                    // *dest = rax
                                    // rax = 3640 = 455 * 8
                                    // R*S*T = 455
    return sizeof( A );
}

// ( 3 * j ) * 4 + j + i + (i << 6) + k
// (i + i << 6) + 13 * j + k
// 65 * i + 13 * j + k
// (T * S * i) + (T * j) + k

// T = 13
// S = 65 / 13 = 5
// R = 455 / 65 = 7
