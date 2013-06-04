#include "machine.h"

#define WAYS 4
#define SETS 16
/* define values related to operands, all possible combinations are included */
typedef struct{
  int in1;			/* input 1 register number */
  int in2;			/* input 2 register number */
  int in3;			/* input 3 register number */
  int out1;			/* output 1 register number */
  int out2;			/* output 2 register number */
}oprand_t;


/*define buffer between fetch and decode stage*/
struct ifid_buf {
  md_inst_t inst;	    /* instruction that has been fetched */
  md_addr_t PC;	        /* pc value of current instruction */
  md_addr_t NPC;		/* the next instruction to fetch */
};


/*define buffer between decode and execute stage*/
struct idex_buf {
  md_inst_t inst;		/* instruction in ID stage */ 
  md_addr_t PC;
  int opcode;			/* operation number */
  oprand_t oprand;		/* operand */
  
  int Jump;
  md_addr_t Target;

  unsigned RegisterRs;
  unsigned RegisterRt;
  unsigned RegisterRd;
  unsigned ReadData1;
  unsigned ReadData2;
  unsigned Shamt;
  unsigned ExtendedImm;
  
  int RegDst;
  int MemtoReg;
  int RegWrite;
  int MemRead;
  int MemWrite;
  int Branch;
  int Latch;
};


/*define buffer between execute and memory stage*/
struct exmem_buf{
  md_inst_t inst;		/* instruction in EX stage */
  md_addr_t PC;
  int opcode;

  oprand_t oprand;
  int ALUResult;
  unsigned WriteData;
  unsigned WriteTargetRegister;

  int PCSrc;
  int RegDst;
  int MemtoReg;
  int RegWrite;
  int MemRead;
  int MemWrite;

};

/*define buffer between memory and writeback stage*/
struct memwb_buf{
  md_inst_t inst;		/* instruction in MEM stage */
  md_addr_t PC;
  int opcode;

  oprand_t oprand;
  unsigned WriteData;
  unsigned MemReadData;
  unsigned ALUResult;
  unsigned WriteTargetRegister;
/*  PCSrc, RegDst was not needed since it will not be passed to this stage*/
  int MemtoReg;
  int RegWrite;
  int MemRead;
  int MemWrite;

};
  
struct execute_sts{
	int cycle;
	int mem_access;
	int cache_hit;
	int cache_miss;
	int line_replacement;
	int line_writeback;
};

struct cache_line{
	unsigned int valid : 1;
	unsigned int dirty : 1;
	unsigned int tag : 24;
	/*used to implement the FIFO queue*/
	unsigned int ref_count ;
	unsigned int data[4];
};

struct cache_set{
	struct cache_line lines[WAYS];
};

struct cache_block{
	struct cache_set sets[SETS];
};

/*do fetch stage*/
void do_if();

/*do decode stage*/
void do_id();

/*do execute stage*/
void do_ex();

/*do memory stage*/
void do_mem();

/*do write_back to register*/
void do_wb();

/*check if the pipeline should stall a cycle*/
void do_stall();
 
void dump_pipeline();

void show_statistics();

void do_forward();

int read_cache(int addr);

void write_cache(int addr, int value);

void clear_cache();

#define MD_FETCH_INSTI(INST, MEM, PC)					\
  { INST.a = MEM_READ_WORD(mem, (PC));					\
    INST.b = MEM_READ_WORD(mem, (PC) + sizeof(word_t)); }

#define SET_OPCODE(OP, INST) ((OP) = ((INST).a & 0xff)) 

#define RSI(INST)		((INST.b >> 24) & 0xff)		/* reg source #1 */
#define RTI(INST)		((INST.b >> 16) & 0xff)		/* reg source #2 */
#define RDI(INST)		((INST.b >> 8) & 0xff)		/* reg dest */

#define IMMI(INST)	((int)((/* signed */short)(INST.b & 0xffff)))	/*get immediate value*/
#define TARGI(INST)	(INST.b & 0x3ffffff)		/*jump target*/

#define TAG(ADDRESS)	((ADDRESS >> 8) & 0xffffff)
#define INDEX(ADDRESS)	((ADDRESS >> 4) & 0xf)
#define OFFSET(ADDRESS)	((ADDRESS >> 2) & 0x3)
