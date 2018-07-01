#define A 10
#define B 8

typedef struct {
    int x[A][B];    // 0
    long y;         // 10 * 8 * 4
} str1;

typedef struct {
    char array[B];  // 0
    int t;          // 8
    short s[A];     // 8 + 4 = 12
    long u;         // 12 + 2 * A = 32 => A = 10
} str2;

void
set_val( str1 *p, str2 *q ) { // rdi = p, rsi = q
    long v1 = q->t;     // rax = *( q + 8 )
    long v2 = q->u;     // rax += *( q + 32 )
    p -> y = v1 + v2;   // *( p + 184 ) = rax
}
