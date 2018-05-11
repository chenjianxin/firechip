import "DPI-C" function void input_stream_init
(
    input string filename,
    input int    data_bits
);

import "DPI-C" function void input_stream_tick
(
    output bit     out_valid,
    input  bit     out_ready,
    output longint out_bits
);

module SimInputStream #(DATA_BITS=64) (
    input                  clock,
    input                  reset,
    output                 out_valid,
    input                  out_ready,
    output [DATA_BITS-1:0] out_bits
);

    bit __out_valid;
    longint __out_bits;
    string filename;
    int data_bits;

    reg                 __out_valid_reg;
    reg [DATA_BITS-1:0] __out_bits_reg;

    initial begin
        data_bits = DATA_BITS;
        if ($value$plusargs("instream=%s", filename)) begin
            input_stream_init(filename, data_bits);
        end
    end

    always @(posedge clock) begin
        if (reset) begin
            __out_valid = 0;
            __out_bits = 0;

            __out_valid_reg <= 0;
            __out_bits_reg <= 0;
        end else begin
            input_stream_tick(
                __out_valid,
                out_ready,
                __out_bits);
            __out_valid_reg <= __out_valid;
            __out_bits_reg  <= __out_bits;
        end
    end

    assign out_valid = __out_valid_reg;
    assign out_bits  = __out_bits_reg;

endmodule
