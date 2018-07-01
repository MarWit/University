#define CNT 7
#define SIZE 4

typedef struct {
    long idx;
    long x[ SIZE ];
} a_struct;

typedef struct {
    int first;          // 0
    a_struct a[CNT];    // 8      --- size = 280
    int last;           // 0x120 = 288
} b_struct;

void
test( long i, b_struct *bp ) {          // rdi = i, rsi = bp
    int n = bp -> first + bp -> last;   // ecx = *(bp + 0x120)  -- ecx = bp -> last
                                        // ecx += *bp           -- ecx = bp -> last + bp -> first
    a_struct *ap = & bp -> a[ i ];      // rax = 5 * i
                                        // rax = rax * 8 + bp = bp + ( 5 * i ) * 8
    ap -> x[ ap -> idx ] = n;           // rdx = *( rax + 8 )   -- & bp -> a[ i ]
                                        // rcx = ecx            -- only first bits
                                        // *( rdx * 8 + rax + 16 ) = rcx
}
