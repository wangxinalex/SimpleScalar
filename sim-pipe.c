/*========================================================================
#   FileName: sim-pipe.c
#     Author: wangxinalex
#      Email: wangxinalex@gmail.com
#   HomePage: http://singo.10ss.me
# LastChange: 2013-04-21 23:26:15
========================================================================*/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

/* An implementation of 5-stage classic pipeline simulation */

/* don't count instructions flag, enabled by default, disable for inst count */
#undef NO_INSN_COUNT

#include "host.h"
#include "misc.h"
#include "machine.h"
#include "regs.h"
#include "memory.h"
#include "loader.h"
#include "syscall.h"
#include "dlite.h"
#include "sim.h"
#include "sim-pipe.h"

/* simulated registers */
static struct regs_t regs;

/* simulated memory */
static struct mem_t *mem = NULL;

/* register simulator-specific options */
void
sim_reg_options(struct opt_odb_t *odb)
{
  opt_reg_header(odb, 
"sim-pipe: This simulator implements based on sim-fast.\n"
		 );
}

/* check simulator-specific option values */
void
sim_check_options(struct opt_odb_t *odb, int argc, char **argv)
{
  if (dlite_active)
    fatal("sim-pipe does not support DLite debugging");
}

/* register simulator-specific statistics */
void
sim_reg_stats(struct stat_sdb_t *sdb)
{
#ifndef NO_INSN_COUNT
  stat_reg_counter(sdb, "sim_num_insn",
		   "total number of instructions executed",
		   &sim_num_insn, sim_num_insn, NULL);
#endif /* !NO_INSN_COUNT */
  stat_reg_int(sdb, "sim_elapsed_time",
	       "total simulation time in seconds",
	       &sim_elapsed_time, 0, NULL);
#ifndef NO_INSN_COUNT
  stat_reg_formula(sdb, "sim_inst_rate",
		   "simulation speed (in insts/sec)",
		   "sim_num_insn / sim_elapsed_time", NULL);
#endif /* !NO_INSN_COUNT */
  ld_reg_stats(sdb);
  mem_reg_stats(mem, sdb);
}


struct ifid_buf fd;
struct idex_buf de;
struct exmem_buf em;
struct memwb_buf mw, wb;
struct execute_sts sts;
struct cache_block cache;
/*Two special resgisters prepared for MULTU*/
int HI=0;
int LO=0;
int cache_on;

#define DNA			(-1)

/* general register dependence decoders */
#define DGPR(N)			(N)
#define DGPR_D(N)		((N) &~1)

/* floating point register dependence decoders */
#define DFPR_L(N)		(((N)+32)&~1)
#define DFPR_F(N)		(((N)+32)&~1)
#define DFPR_D(N)		(((N)+32)&~1)

/* miscellaneous register dependence decoders */
#define DHI			(0+32+32)
#define DLO			(1+32+32)
#define DFCC		(2+32+32)
#define DTMP		(3+32+32)

/* initialize the simulator */
void
sim_init(void)
{
  /* allocate and initialize register file */
  regs_init(&regs);

  /* allocate and initialize memory space */
  mem = mem_create("mem");
  mem_init(mem);

  /* initialize stage latches*/
 
  /* IF/ID */
  fd.inst.a = NOP;
  fd.inst.b = 0;
  fd.PC = 0;
  fd.NPC = 0;

  /* ID/EX */
  de.inst.a = NOP;
  de.inst.b = 0;
  de.PC = 0;


  // de.latched = 0;
  /* EX/MEM */
  em.inst.a = NOP;
  em.inst.b = 0;
  em.PC = 0;
  em.oprand.out1 = DNA;
  em.oprand.out2 = DNA;


  /* MEM/WB */
  mw.inst.a = NOP;
  mw.inst.b = 0;
  mw.PC = 0;
  mw.oprand.out1 = DNA;
  mw.oprand.out2 = DNA;
}

/* load program into simulated state */
void
sim_load_prog(char *fname,		/* program to load */
	      int argc, char **argv,	/* program arguments */
	      char **envp)		/* program environment */
{
  /* load program text and data, set up environment, memory, and regs */
  ld_load_prog(fname, argc, argv, envp, &regs, mem, TRUE);
}

/* print simulator-specific configuration information */
void
sim_aux_config(FILE *stream)
{  
	/* nothing currently */
}

/* dump simulator-specific auxiliary simulator statistics */
void
sim_aux_stats(FILE *stream)
{  /* nada */}

/* un-initialize simulator-specific state */
void 
sim_uninit(void)
{ /* nada */ }


/*
 * configure the execution engine
 */

/* next program counter */
#define SET_NPC(EXPR)		(regs.regs_NPC = (EXPR))

/* current program counter */
#define CPC			(regs.regs_PC)

/* general purpose registers */
#define GPR(N)			(regs.regs_R[N])
#define SET_GPR(N,EXPR)		(regs.regs_R[N] = (EXPR))
#define DECLARE_FAULT(EXP) 	{;}
#if defined(TARGET_PISA)

/* floating point registers, L->word, F->single-prec, D->double-prec */
#define FPR_L(N)		(regs.regs_F.l[(N)])
#define SET_FPR_L(N,EXPR)	(regs.regs_F.l[(N)] = (EXPR))
#define FPR_F(N)		(regs.regs_F.f[(N)])
#define SET_FPR_F(N,EXPR)	(regs.regs_F.f[(N)] = (EXPR))
#define FPR_D(N)		(regs.regs_F.d[(N) >> 1])
#define SET_FPR_D(N,EXPR)	(regs.regs_F.d[(N) >> 1] = (EXPR))

/* miscellaneous register accessors */
#define SET_HI(EXPR)		(regs.regs_C.hi = (EXPR))
#define HI			(regs.regs_C.hi)
#define SET_LO(EXPR)		(regs.regs_C.lo = (EXPR))
#define LO			(regs.regs_C.lo)
#define FCC			(regs.regs_C.fcc)
#define SET_FCC(EXPR)		(regs.regs_C.fcc = (EXPR))

#endif

/* precise architected memory state accessor macros */
#define READ_BYTE(SRC, FAULT)						\
  ((FAULT) = md_fault_none, MEM_READ_BYTE(mem, (SRC)))
#define READ_HALF(SRC, FAULT)						\
  ((FAULT) = md_fault_none, MEM_READ_HALF(mem, (SRC)))
#define READ_WORD(SRC, FAULT)						\
  ((FAULT) = md_fault_none, MEM_READ_WORD(mem, (SRC)))
#ifdef HOST_HAS_QWORD
#define READ_QWORD(SRC, FAULT)						\
  ((FAULT) = md_fault_none, MEM_READ_QWORD(mem, (SRC)))
#endif /* HOST_HAS_QWORD */

#define WRITE_BYTE(SRC, DST, FAULT)					\
  ((FAULT) = md_fault_none, MEM_WRITE_BYTE(mem, (DST), (SRC)))
#define WRITE_HALF(SRC, DST, FAULT)					\
  ((FAULT) = md_fault_none, MEM_WRITE_HALF(mem, (DST), (SRC)))
#define WRITE_WORD(SRC, DST, FAULT)					\
  ((FAULT) = md_fault_none, MEM_WRITE_WORD(mem, (DST), (SRC)))
#ifdef HOST_HAS_QWORD
#define WRITE_QWORD(SRC, DST, FAULT)					\
  ((FAULT) = md_fault_none, MEM_WRITE_QWORD(mem, (DST), (SRC)))
#endif /* HOST_HAS_QWORD */

/* system call handler macro */
#define SYSCALL(INST)	sys_syscall(&regs, mem_access, mem, INST, TRUE)

#ifndef NO_INSN_COUNT
#define INC_INSN_CTR()	sim_num_insn++
#else /* !NO_INSN_COUNT */
#define INC_INSN_CTR()	/* nada */
#endif /* NO_INSN_COUNT */


/* start simulation, program loaded, processor precise state initialized */
void
sim_main(void)
{
  bzero(&sts,sizeof(struct execute_sts));
  cache_on = 1;
  bzero(&cache,sizeof(struct cache_block));
  clear_cache();
  fprintf(stderr, "sim: ** starting *pipe* functional simulation **\n");

  /* must have natural byte/word ordering */
  if (sim_swap_bytes || sim_swap_words)
    fatal("sim: *pipe* functional simulation cannot swap bytes or words");

  /* set up initial default next PC */
  regs.regs_NPC = regs.regs_PC + sizeof(md_inst_t);
  /* maintain $r0 semantics */
  regs.regs_R[MD_REG_ZERO] = 0;
  fd.PC = regs.regs_PC - sizeof(md_inst_t);
 
  while (TRUE)
  {
	  //handle the pipeline in reverse so that the instruction could be handled in ascending order
	  sim_num_insn++;
	  sts.cycle++;
	  do_stall();
	  do_forward();
	  do_wb();
	  do_mem();
	  do_ex();
	  do_id();
	  do_if();
	  /*dump_pipeline();*/
  }
  clear_cache();
}
void show_statistics(){
	printf("Total Instruction Number: %d\n",(int)sim_num_insn);
	printf("Total Clock Cycle Number: %d\n",sts.cycle);
	printf("Total Cache Access Number: %d\n",sts.mem_access);
	printf("Total Cache Hit Number: %d\n",sts.cache_hit);
	printf("Total Cache Miss Number: %d\n",sts.cache_miss);
	printf("Total Cache Replacement Number: %d\n",sts.line_replacement);
	printf("Total Cache WriteBack Number: %d\n",sts.line_writeback);
}

/*dump the pipeline*/
void dump_pipeline(){
	enum md_fault_type _fault;
	printf("[Cycle %5d]---------------------------------\n",(int)sim_num_insn);
	printf("[IF]  ");md_print_insn(fd.inst, fd.PC, stdout);printf("\n");
	printf("[ID]  ");md_print_insn(de.inst, de.PC, stdout);printf("\n");
	printf("[EX]  ");md_print_insn(em.inst, em.PC, stdout);printf("\n");
	printf("[MEM] ");md_print_insn(mw.inst, mw.PC, stdout);printf("\n");
	printf("[WB]  ");md_print_insn(wb.inst, wb.PC, stdout);printf("\n");
	printf("[REGS]r2=%d r3=%d r4=%d r5=%d r6=%d mem = %d\n", 
			GPR(2),GPR(3),GPR(4),GPR(5),GPR(6),(int)READ_WORD(GPR(30)+16, _fault));
	printf("----------------------------------------------\n");
}

/*
 *Check the Load-use Hazard only, which cannot be solved by forwarding
 */
void do_stall(){
   if(em.inst.a != NOP && em.MemRead == 1 && 
		   (em.oprand.out1 == de.oprand.in1 || em.oprand.out1 == de.oprand.in2)){
   		fd.inst = de.inst;
		fd.PC = de.PC;
		de.inst.a = NOP;
   } 
}

/*
 *Check the forwarding condition
 */
void do_forward(){
/*MEM Hazard, the "else if" is imprtant for we should not let the MEM forwarding rewite the result of EX forwarding*/
	if(mw.inst.a != NOP && 
			(mw.oprand.out1 == de.oprand.in1 || mw.oprand.out1 == de.oprand.in2)){
		if(mw.oprand.out1 == de.RegisterRs){
			/*printf("ForwardA = 01\n");*/
			de.ReadData1 = mw.MemtoReg == 1?mw.MemReadData:mw.ALUResult;
		}
		if(de.RegDst==1||de.RegWrite==0){
		 	if(mw.oprand.out1 == de.RegisterRt){
				/*printf("ForwardA = 01\n");*/
				de.ReadData2 = mw.MemtoReg == 1?mw.MemReadData:mw.ALUResult;
			}
		}
		
	}

/*EX Hazard*/
	if(em.inst.a != NOP && !em.MemRead && 
			(em.oprand.out1 == de.oprand.in1 || em.oprand.out1 == de.oprand.in2 )){
		/*the value of out1 is not determined*/
		if(em.oprand.out1 == de.RegisterRs){
			/*printf("ForwardA = 10, Forward Result = %d\n",em.ALUResult);*/
			de.ReadData1 = em.ALUResult;
		}
		if(de.RegDst==1||de.RegWrite==0){
	   		if(em.oprand.out1 == de.RegisterRt){
				/*printf("ForwardB = 10 ,Forward Result = %d\n",em.ALUResult);*/
				de.ReadData2 = em.ALUResult;
			}
		}
	}
} 

void do_if()
{
/*In this stage, the pipeline need to get the PC of the next instruction,
 * hence it should get the type of the previous instruction in order to 
 * determine whether to 
 * */
  md_inst_t new_inst;
/*  direct jump*/
  if (de.Jump==1){
  	fd.NPC = de.Target;
/*  branch taken*/
  }else if(de.inst.a != NOP && de.Branch == 1){
  	fd.NPC = de.Target;
/*  PC = PC + 4*/
  }else{
  	fd.NPC = fd.PC + sizeof(md_inst_t);
  }
  fd.PC =fd.NPC;
/*Fetch the next instruction*/
  read_cache((int)fd.PC);
  read_cache((int)(fd.PC + sizeof(word_t)));
  MD_FETCH_INSTI(new_inst, mem, fd.PC);
  fd.inst = new_inst;
}

void do_id()
{
    de.inst = fd.inst;
	if(de.inst.a == NOP){
		return;
	}
    MD_SET_OPCODE(de.opcode, de.inst);
    de.PC = fd.PC;
	md_inst_t inst  = de.inst;
#define DEFINST(OP,MSK,NAME,OPFORM,RES,FLAGS,O1,O2,I1,I2,I3)\
  if (OP==de.opcode){\
    de.oprand.out1 = O1;\
    de.oprand.out2 = O2;\
    de.oprand.in1 = I1;\
    de.oprand.in2 = I2;\
    de.oprand.in3 = I3;\
    goto READ_OPRAND_VALUE;\
  }
#define DEFLINK(OP,MSK,NAME,MASK,SHIFT)
#define CONNECT(OP)
#include "machine.def"
READ_OPRAND_VALUE:
	de.Jump = 0;
	de.Target = 0;
	de.RegisterRs = RS;
	de.RegisterRt = RT;
	de.RegisterRd = RD;

	de.ReadData1 = GPR(de.RegisterRs);
	de.ReadData2 = GPR(de.RegisterRt);
	de.Shamt = 0;
	de.ExtendedImm = UIMM;

	de.RegDst = 0;
	de.RegWrite = 0;
	de.MemRead = 0;
	de.MemWrite = 0;
	de.MemtoReg = 0;
	de.Branch = 0;
	de.Latch = 0;
/* The setting of the control lines is completely determined by the opcode fields
 * With reference to SimpleScalarToolSet Ver2.0*/

    switch(de.opcode){
	case ADD:
		de.Branch = 0;
		/*Write destination is RD*/
		de.RegDst = 1;
		de.MemtoReg = 0;
		de.RegWrite = 1;
		de.MemRead = 0;
		de.MemWrite = 0;
		break;
    case ADDU:
	    de.Branch = 0;
		de.RegDst = 1;
		de.MemtoReg = 0;
		de.RegWrite = 1;
		de.MemRead = 0;
		de.MemWrite = 0;
		break;
    case SUBU:
	    de.Branch = 0;
		de.RegDst = 1;
		de.MemtoReg = 0;
		de.RegWrite = 1;
		de.MemRead = 0;
		de.MemWrite = 0;
		break;
		/*Strange...*/
	case MULTU:
		de.Branch = 0;
		de.RegDst = 0;
		de.MemtoReg = 0;
		de.RegWrite = 0;
		de.MemRead = 0;
		de.MemWrite = 0;
    case MFLO:
		de.Branch = 0;
		/*Target is RD*/
		de.RegDst = 1;
		de.MemtoReg = 0;
		de.RegWrite = 1;
		de.MemRead = 0;
		de.MemWrite = 0;
    case ADDIU:
	    de.Branch = 0;
		/*Write desination is RT*/
		de.RegDst = 0;
		de.MemtoReg = 0;
		de.RegWrite = 1;
		de.MemRead = 0;
		de.MemWrite = 0;
		de.ExtendedImm = IMM;
		break;
	case ADDI:
	    de.Branch = 0;
		de.RegDst = 0;
		de.MemtoReg = 0;
		de.RegWrite = 1;
		de.MemRead = 0;
		de.MemWrite = 0;
		de.ExtendedImm = IMM;
		break;
	case ANDI:
	    de.Branch = 0;
		de.RegDst = 0;
		de.MemtoReg = 0;
		de.RegWrite = 1;
		de.MemRead = 0;
		de.MemWrite = 0;
		de.ExtendedImm = IMM;
		break;
	case BNE:
		de.RegWrite = 0;
		de.MemRead = 0;
		de.MemWrite = 0;
		de.Latch = 1;
		de.ExtendedImm = OFS;
		de.Target = (de.PC + 8 + (OFS<<2));
		/*This do_forward is important, if we want to get the result of Branch comparison in decode stage, we must forward the result again in decode stage */
		do_forward();
		de.Branch = (de.ReadData1 != de.ReadData2);
		/*printf("%i %i %s\n",de.ReadData1,de.ReadData2,de.Branch?"Branch Taken":"Not Taken");*/
		break;
	case BEQ:
		de.RegWrite = 0;
		de.MemRead = 0;
		de.MemWrite = 0;
		de.Latch = 1;
		de.ExtendedImm = OFS;
		de.Target = (de.PC + 8 + (OFS<<2));
		/*This do_forward is important, if we want to get the result of Branch comparison in decode stage, we must forward the result again in decode stage */
		do_forward();
		de.Branch = (de.ReadData1 == de.ReadData2);
		/*printf("%i %i %s\n",de.ReadData1,de.ReadData2,de.Branch?"Branch Taken":"Not Taken");*/
		break;

	case JUMP:
	    de.Branch = 0;
		de.RegWrite = 0;
		de.MemRead = 0;
		de.MemWrite = 0;
		de.Jump = 1;
		de.Target = (de.PC & 0xF0000000)|(TARG<<2);
		break;
	case LUI:
	    de.Branch = 0;
		de.RegDst = 0;
		de.RegWrite = 1;
		de.ExtendedImm = UIMM << 16;
		break;
	case LW:
	    de.Branch = 0;
		de.RegDst = 0;
		de.RegWrite = 1;
		de.ExtendedImm = OFS;
		de.MemRead = 1;
		de.MemtoReg = 1;
		break;
	case SLL:
	    de.Branch = 0;
		de.RegDst = 1;
		de.RegWrite = 1;
		de.Shamt = SHAMT;
		break;
	case SW:
	    de.Branch = 0;
		de.RegWrite = 0;
		de.MemRead = 0;
		de.MemWrite = 1;
		de.ExtendedImm = IMM;
		break;
	case SLTI:
	    de.Branch = 0;
	    de.RegDst = 0;
/*		With regard to this instruction, the SimpleScalar Tool Set is WRONG. It should be SET_GPR(RT,(GPR(RS)<IMM)?1:0)*/
		de.MemRead = 0;
	    de.RegWrite = 1;
        de.ExtendedImm = IMM;
	    break;
  }
  
}

void do_ex()
{
  em.inst = de.inst;
  if(em.inst.a == NOP){
  	return;
  }
  em.PC = fd.PC;
  em.opcode = de.opcode;
  em.oprand = de.oprand;
  em.MemRead = de.MemRead;
  em.MemtoReg = de.MemtoReg;
  em.MemWrite = de.MemWrite;
  em.RegWrite = de.RegWrite;
  em.WriteData = de.ReadData2;
  em.RegDst = de.RegDst;
  em.PCSrc = 0; 
/*Determine the WriteTargetRegister this time hence it does not need to pass two Register number in the following stage*/
  em.WriteTargetRegister = em.RegDst?de.RegisterRd:de.RegisterRt;

  switch(em.opcode){
	  case ADD:
		  em.ALUResult = (int)de.ReadData1 + (int)de.ReadData2;
		  break;
	  case ADDU:
		 em.ALUResult = (unsigned)de.ReadData1 + (unsigned)de.ReadData2;
		 break;
	  case SUBU:
		 em.ALUResult = (unsigned)de.ReadData1 - (unsigned)de.ReadData2;
		 break;
	  case MULTU:
		 HI = (int)(((unsigned long)de.ReadData1 * (unsigned long)de.ReadData2)>>32);
		 LO = (int)((unsigned)de.ReadData1 * (unsigned)de.ReadData2);
		 break;
	  case MFLO:
		 em.ALUResult = LO;
		 break;
	  case ADDIU:
		 em.ALUResult = (unsigned)de.ReadData1 + (unsigned)de.ExtendedImm;
		 break;
		 /*Added in lab3*/
	  case ADDI:
		 em.ALUResult = de.ReadData1 + de.ExtendedImm;
		 break;
	  case ANDI:
		 em.ALUResult = de.ReadData1|de.ReadData2;
		 break;
	  case BNE:
		 break;
	  case BEQ:
		 break;
	  case JUMP:
		break;
	  case LUI:
		em.ALUResult = de.ExtendedImm;
		break;
	  case LW:
		em.ALUResult = de.ReadData1 + de.ExtendedImm;
		break;
	  case SLL:
		em.ALUResult = de.ReadData2<<de.Shamt;
		break;
	  case SW:
/*ALUResult is the target address of the memory*/
		em.ALUResult = de.ReadData1 + de.ExtendedImm;
		break;
	  case SLTI:
		em.ALUResult = (int)de.ReadData1 <(int)de.ExtendedImm?1:0;
		break;
  }

}

void do_mem()
{
  enum md_fault_type _fault;
  mw.inst = em.inst;
  if(mw.inst.a == NOP){
  	return;
  }
  mw.PC = em.PC;
  mw.opcode = em.opcode;
  mw.oprand = em.oprand;
  mw.ALUResult = em.ALUResult;
  mw.WriteData = em.WriteData;
  mw.WriteTargetRegister = em.WriteTargetRegister;

  mw.RegWrite = em.RegWrite;
  mw.MemtoReg = em.MemtoReg;
  mw.MemRead = em.MemRead;
  mw.MemWrite = em.MemWrite;
  if(mw.opcode == LW){
	/*sts.mem_access++; 	*/
	  read_cache(mw.ALUResult);
  	  mw.MemReadData = READ_WORD(mw.ALUResult,_fault);
  }else if(mw.opcode == SW){
      /*sts.mem_access++;*/
	  write_cache(mw.ALUResult, mw.WriteData);
      WRITE_WORD(mw.WriteData,mw.ALUResult,_fault);
  }

    
}                                                                                        

void do_wb()
{
	wb = mw;
	if(wb.inst.a == NOP){
		return;
	}
 	if(wb.inst.a == SYSCALL){
		printf("Loop terminated. Result = %d\n",GPR(6));
		show_statistics();
		SET_GPR(2,SS_SYS_exit);
		SYSCALL(wb.inst);

	}else if(wb.inst.a != NOP && wb.RegWrite == 1){
		SET_GPR(wb.WriteTargetRegister, wb.MemtoReg?wb.MemReadData:wb.ALUResult);
	}
}

/*Clear the cache blocks. If the cache line is dirty, it should be written back  */
void clear_cache(){
	int i = 0, j = 0;
	for(i = 0; i < SETS; i++){
		for(j = 0; j < WAYS; j++){
			if(cache.sets[i].lines[j].dirty == 1){
				sts.line_writeback++;
				cache.sets[i].lines[j].dirty = 0;
			}
			cache.sets[i].lines[j].tag = 0;
			cache.sets[i].lines[j].valid = 0;
			cache.sets[i].lines[j].ref_count = 0;
			cache.sets[i].lines[j].dirty = 0;
			bzero(&(cache.sets[i].lines[j].data),sizeof(unsigned int)*WAYS);
			
		}
	}
}

int read_cache(int addr){
	int tag = TAG(addr);
	int index = INDEX(addr);
	int i, j, k, latest, max_ref_count;
	sts.mem_access++;
	/*printf("Read Cache: tag = %d\tindex = %d\toffset = %d\n", tag, index, offset);*/
	if(cache_on){
		for(i = 0; i < WAYS; i++ ){
			if(cache.sets[index].lines[i].valid == 1){
				cache.sets[index].lines[i].ref_count ++;
				/*printf("%-20s: tag = %d\tindex = %d\tref_count = %d\n","Ref_count", tag, index, cache.sets[index].lines[i].ref_count);*/
			}
		}
	
		/*cache hit*/
		for(i = 0; i < WAYS; i++){
				if(cache.sets[index].lines[i].tag == tag && cache.sets[index].lines[i].valid == 1){
				/*printf("%-20s: tag = %d\tindex = %d\toffset = %d\n","Cache Hit", tag, index, i);*/
				sts.cache_hit ++;
				sts.cycle ++;
				return i; 
			}
		}
		/*cache miss*/
		if(i >= 4){
			sts.cache_miss ++;
			sts.cycle += 10;
			/*printf("%-20s: tag = %d\tindex = %d\n","Cache Miss", tag, index);*/
			/*find the first "empty" cache line */
			for(j = 0; j < WAYS; j++){
				if(cache.sets[index].lines[j].valid == 0){
					cache.sets[index].lines[j].tag = tag;
					cache.sets[index].lines[j].valid = 1;
					cache.sets[index].lines[j].ref_count = 0;
					cache.sets[index].lines[j].dirty = 0;
					return j; 
				}
			}
			if(j >= 4){
				latest = 0;
				max_ref_count = 0;
				/*find the cache line which was first swapped in */
				for(k = 0; k < WAYS; k++){
					if(cache.sets[index].lines[k].ref_count > max_ref_count){
						latest = k;
						max_ref_count = cache.sets[index].lines[k].ref_count;
					}
				}
				/*If the cache line was dirty, it should be written back before being replaced*/
				if(cache.sets[index].lines[latest].dirty == 1){
					sts.line_writeback ++;
					/*printf("%-20s: tag = %d\tindex = %d\toffset = %d\tref_count = %d\n","Cache WriteBack", tag, index, latest, max_ref_count);*/
				}
				cache.sets[index].lines[latest].tag = tag;
				cache.sets[index].lines[latest].valid = 1;
				cache.sets[index].lines[latest].ref_count = 0;
				cache.sets[index].lines[latest].dirty = 0;
				/*printf("%-20s: tag = %d\tindex = %d\toffset = %d\tref_count = %d\n","Cache Replacement", tag, index, latest, max_ref_count);*/
				sts.line_replacement ++;
				return latest;
			}
		}
	}else{
		sts.cycle+=10;
	}
	return -1;
}

void write_cache(int addr, int value){
	/*int tag = TAG(addr);*/
	int index = INDEX(addr);
	/*printf("%-20s: tag = %d\tindex = %d\toffset = %d\tvalue = %d\n","Write Cache", tag, index, offset, value);*/
	if(cache_on){
		/*get the exactly way from cache*/
		int way = read_cache(addr);
		/*mark the cache line "dirty"*/
		cache.sets[index].lines[way].dirty = 1;
	}else{
		sts.mem_access ++;
		sts.cycle+=10;
	}
}
