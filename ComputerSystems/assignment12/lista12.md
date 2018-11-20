### zadanie8.py

``` {.python}

#!/usr/bin/env python

def full_adder( a, b ):
    return (((a+b) & 3), (a + b) > 3)

def mult( a, b ):
    x = (a * b) & 15
    return ( x >> 2, x & 3 )

N1 = 0b1101
N2 = 0b1011

(ab,cd) = (N1>>2,N1&3)
(ef,gh) = (N2>>2,N2&3)

A = mult( cd, gh )
B = mult( ab, gh )
C = mult( cd, ef )
D = mult( ab, ef )

R01 = A[ 1 ]

(R23, C23_1) = full_adder( B[ 1 ], A[ 0 ] )
(R23, C23_2) = full_adder( C[ 1 ], R23 )

(C23, _) = full_adder( C23_1, C23_2 )

(R45, C45_1) = full_adder( B[ 0 ], C23 )
(R45, C45_2) = full_adder( C[ 0 ], R45 )
(R45, C45_3) = full_adder( D[ 1 ], R45 )

(C45, _) = full_adder( C45_1, C45_2 )
(C45, _) = full_adder( C45, C45_3 )

(R67, _) = full_adder( C45, D[ 0 ] )

print( bin( (R67 << 6) | (R45 << 4) | (R23 << 2) | R01 ) )
print( bin( (N1 * N2) & 0xff ) )

(R45, C45) = full_adder( B[ 0 ], C23 )
```

Zadanie 9
---------

``` {.verilog}
module moore( A, clk, reset, Y );

input A, clk, reset;
output reg Y;

reg [1:0] state,
          nextState;

always @(A or state) begin
    case(state)
        0:
        begin
            if (A) nextState = 1;
            else nextState = 0;
        end

        1:
        begin
            if(A) nextState = 0;
            else nextState = 2;
        end

        2:
        begin
            if(A) nextState = 1;
            else nextState = 0;
        end
    endcase
end

always @(state) begin
    case (state)
        0: Y = 0;
        1: Y = 0;
        2: Y = 1;
    endcase
end

always @(posedge clk) begin
    if (reset) state <= 0;
    else state <= nextState;
end

endmodule
```
