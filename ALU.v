//------------------------------------------------------------------------------
// Modul       : ALU
// Autor       : Pacuraru Alex Marius
// Data        : Iulie 19, 2023
//------------------------------------------------------------------------------
// Descriere   : Arithmetical Logic Unit + Interfaces that involve ALU (RTL)
//------------------------------------------------------------------------------
// Modificari  :
// Iulie 19, 2023: Initial 
//------------------------------------------------------------------------------
module ALU (
input             clk               ,  // clock signal
input             rst_n             ,  // asynchronous reset signal

input             dcd_req           ,  // request, active when we need to do a task
input      [3:0]  dcd_opcode        ,  // decoded sequence to see which instruction to execute
input      [11:0] dcd_mem_addr      ,  // memory address
input      [2:0]  dcd_src_reg_1_addr,  // first register's address
input      [2:0]  dcd_src_reg_2_addr,  // second register's address
input      [2:0]  dcd_dst_reg_addr  ,  // destination register's address
input      [15:0] dcd_const         ,  // constant value
output reg        dcd_ack           ,  // acknowledge, when the requested task is completed

input      [31:0] data_mem_rd_data  ,  // memory read data
output reg        data_mem_cen      ,  // memory chip enable, when active, task starts
output reg        data_mem_rw       ,  // memory read/write
output reg [11:0] data_mem_addr     ,  // memory address
output reg [31:0] data_mem_rw_data  ,  // memory write data

input      [15:0] reg_0_rd_data     ,  // register 0 read data
input             reg_0_ack         ,  // signal active when the requested task is completed
output reg        reg_0_req         ,  // signal active when a task needs to be completed
output            reg_0_rw          ,  // signal active when we read
output reg [2:0]  reg_0_addr        ,  // register address

input      [15:0] reg_1_rd_data     ,  // register 1 read data
input             reg_1_ack         ,  //  signal active when the requested task is completed
output reg        reg_1_req         ,  //  signal active when a task needs to be completed
output reg        reg_1_rw          ,  //  signal active when we read/write
output reg [2:0]  reg_1_addr        ,  //  register address
output reg [15:0] reg_1_wr_data     ,  // register write data

output reg        pc_jump_req       ,  // signal active when a task needs to be completed
output reg [11:0] pc_jump_addr         // address to jump at
);

localparam LOAD = 4'b0000;
localparam LOAD_const = 4'b0001;
localparam STORE = 4'b0010;
localparam STORE_const = 4'b0011;
localparam ADD = 4'b0100;
localparam SUBSTRACT = 4'b0101;
localparam AND = 4'b0110;
localparam OR = 4'b0111;
localparam JUMP = 4'b1000;
localparam JUMP_IF_EQ = 4'b1001;

reg dcd_req_delay; // request signal delayed 1 period
reg cen_delay; // data_mem_cen delayed 1 period
wire alu_start; // signal active at the start of request

always@(posedge clk or negedge rst_n)
if(~rst_n) dcd_req_delay <= 0; else
           dcd_req_delay <= dcd_req;

assign alu_start = dcd_req & ~dcd_req_delay;

always@(posedge clk or negedge rst_n)
if(~rst_n) cen_delay <= 0; else
           cen_delay <= data_mem_cen;

always @(posedge clk or negedge rst_n)
if(~rst_n) data_mem_cen <= 0; else
if(data_mem_cen) data_mem_cen <= 0; else
if (((dcd_opcode == LOAD) & alu_start) | ((dcd_opcode == STORE_const) & alu_start) | ((dcd_opcode == STORE) & (reg_0_req & reg_0_ack))) data_mem_cen <= 1;

always @(posedge clk or negedge rst_n)
if(~rst_n) data_mem_rw <= 0; else
if(alu_start) data_mem_rw <= (dcd_opcode == LOAD);

always @(posedge clk or negedge rst_n)
if(~rst_n) data_mem_addr <= 0; else
if(alu_start) data_mem_addr <= (dcd_mem_addr);

always @(posedge clk or negedge rst_n)
if(~rst_n) data_mem_rw_data <= 0; else
if((alu_start) & (dcd_opcode == STORE_const))       data_mem_rw_data <= dcd_const; else
if((reg_0_req & reg_0_ack) & (dcd_opcode == STORE)) data_mem_rw_data <= reg_0_rd_data;

always @(posedge clk or negedge rst_n)
if(~rst_n) reg_0_req <= 0; else
if(reg_0_ack) reg_0_req <= 0; else
if((dcd_opcode != STORE_const) & (dcd_opcode != JUMP) & (dcd_opcode != LOAD) & (dcd_opcode != LOAD_const) & alu_start) reg_0_req <= 1;

assign reg_0_rw = 1;

always @(posedge clk or negedge rst_n)
if(~rst_n) reg_0_addr <= 0; else
if(alu_start) reg_0_addr <= (dcd_src_reg_1_addr);

always @(posedge clk or negedge rst_n)
if(~rst_n) reg_1_req <= 0; else
if(((dcd_opcode == ADD) | (dcd_opcode == SUBSTRACT) | (dcd_opcode == AND) | (dcd_opcode == OR)) & (reg_1_req & reg_1_ack & reg_1_rw)) reg_1_req <= 1; else
if(reg_1_ack) reg_1_req <= 0; else
if(((dcd_opcode == LOAD_const) | (dcd_opcode == ADD) | (dcd_opcode == SUBSTRACT) | (dcd_opcode == AND) | (dcd_opcode == OR)) & (alu_start | (reg_1_req & reg_1_ack))) reg_1_req <= 1; else
if((dcd_opcode == LOAD) & cen_delay) reg_1_req <= 1;//atentie aici

always @(posedge clk or negedge rst_n)
if(~rst_n) reg_1_rw <= 0; else
if ( ((dcd_opcode == ADD) | (dcd_opcode == SUBSTRACT) | (dcd_opcode == AND) | (dcd_opcode == OR)) & alu_start) reg_1_rw <= 1; else
if (alu_start & (dcd_opcode == LOAD_const)) reg_1_rw <= 0; else
if(((dcd_opcode == ADD) | (dcd_opcode == SUBSTRACT) | (dcd_opcode == AND) | (dcd_opcode == OR)) & reg_1_req & reg_1_ack) reg_1_rw <= 0; else
if((cen_delay) & (dcd_opcode == LOAD)) reg_1_rw <= 0; // 1 este read, 0 este write

always @(posedge clk or negedge rst_n)
if(~rst_n)                                         reg_1_wr_data <= 0; else
if(reg_1_req & reg_1_ack & reg_1_rw) begin
if(dcd_opcode == ADD)                              reg_1_wr_data <= reg_0_rd_data + reg_1_rd_data; else
if(dcd_opcode == SUBSTRACT)                        reg_1_wr_data <= reg_0_rd_data - reg_1_rd_data; else 
if(dcd_opcode == AND)                              reg_1_wr_data <= reg_0_rd_data & reg_1_rd_data; else 
if(dcd_opcode == OR)                               reg_1_wr_data <= reg_0_rd_data | reg_1_rd_data; 
end
else if(alu_start & (dcd_opcode == LOAD_const))                       reg_1_wr_data <= dcd_const; else 
if((cen_delay) & (dcd_opcode == LOAD))             reg_1_wr_data <= data_mem_rd_data; 

always @(posedge clk or negedge rst_n)
if(~rst_n) reg_1_addr <= 0; else
if(((dcd_opcode == ADD) | (dcd_opcode == SUBSTRACT) | (dcd_opcode == AND) | (dcd_opcode == OR)) & (reg_1_req & reg_1_ack & reg_1_rw)) reg_1_addr <= dcd_dst_reg_addr; else
if(alu_start & ((dcd_opcode == LOAD_const) | (dcd_opcode == LOAD))) reg_1_addr <= dcd_dst_reg_addr; else
if(alu_start) reg_1_addr <= (dcd_src_reg_2_addr);

always @(posedge clk or negedge rst_n)
if(~rst_n) pc_jump_req <= 0; else
if(pc_jump_req) pc_jump_req <= 0; else // resetare 
if((alu_start) & (dcd_opcode == JUMP))        pc_jump_req <= 1; else 
if((reg_0_req & reg_0_ack) & (dcd_opcode == JUMP_IF_EQ)) pc_jump_req <= (dcd_const == reg_0_rd_data) ? 1 : 0;

always @(posedge clk or negedge rst_n)
if(~rst_n)    pc_jump_addr <= 0; else
if(alu_start) pc_jump_addr <= (dcd_mem_addr);

always @(posedge clk or negedge rst_n)
if(~rst_n)                                                                                                                       dcd_ack <= 0; else
if(dcd_ack)                                                                                                                      dcd_ack <= 0; else
if(((dcd_opcode == LOAD) | (dcd_opcode == LOAD_const)) & (reg_1_rw == 0) & (reg_1_ack))                                          dcd_ack <= 1; else
if(((dcd_opcode == STORE) | (dcd_opcode == STORE_const)) & (data_mem_cen))                                                       dcd_ack <= 1; else
if(((dcd_opcode == ADD) | (dcd_opcode == SUBSTRACT) | (dcd_opcode == AND) | (dcd_opcode == OR)) & (reg_1_rw == 0) & (reg_1_ack)) dcd_ack <= 1; else
if((dcd_opcode == JUMP) & (alu_start))                                                                                           dcd_ack <= 1; else // la jump se da ack in acelasi tact cu request-ul
if((dcd_opcode == JUMP_IF_EQ) & (reg_0_req & reg_0_ack))                                                                         dcd_ack <= 1; 


endmodule // ALU