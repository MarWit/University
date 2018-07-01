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
