#include <stdio.h>
#include <stdlib.h>
#include <netinet/in.h>
#include "computer.h"
#undef mips			/* gcc already has a def for mips */

unsigned int endianSwap(unsigned int);

void PrintInfo(int changedReg, int changedMem);
unsigned int Fetch(int);
void Decode(unsigned int, DecodedInstr*, RegVals*);
int Execute(DecodedInstr*, RegVals*);
int Mem(DecodedInstr*, int, int *);
void RegWrite(DecodedInstr*, int, int *);
void UpdatePC(DecodedInstr*, int);
void PrintInstruction(DecodedInstr*);

/*Globally accessible Computer variable*/
Computer mips;
RegVals rVals;

//r
int addu = 0x21;
int subu = 0x23;
int sll = 0x00;
int srl = 0x02;
int and = 0x20;
int or = 0x25;
int slt = 0x2a;
int jr = 0x08;

//i
int addiu = 0x9;
int andi = 0xc;
int ori = 0xd;
int lui = 0xf;
int beq = 0x4;
int bne = 0x5;
int lw = 0x23;
int sw = 0x2b;

//j
int j = 0x2;
int jal = 0x3;




/*
 *  Return an initialized computer with the stack pointer set to the
 *  address of the end of data memory, the remaining registers initialized
 *  to zero, and the instructions read from the given file.
 *  The other arguments govern how the program interacts with the user.
 */
void InitComputer(FILE* filein, int printingRegisters, int printingMemory,
	int debugging, int interactive) {
	int k;
	unsigned int instr;

	/* Initialize registers and memory */

	for (k = 0; k < 32; k++) {
		mips.registers[k] = 0;
	}

	/* stack pointer - Initialize to highest address of data segment */
	mips.registers[29] = 0x00400000 + (MAXNUMINSTRS + MAXNUMDATA) * 4;

	for (k = 0; k < MAXNUMINSTRS + MAXNUMDATA; k++) {
		mips.memory[k] = 0;
	}

	k = 0;
	while (fread(&instr, 4, 1, filein)) {
		/*swap to big endian, convert to host byte order. Ignore this.*/
		mips.memory[k] = ntohl(endianSwap(instr));
		k++;
		if (k > MAXNUMINSTRS) {
			fprintf(stderr, "Program too big.\n");
			exit(1);
		}
	}

	mips.printingRegisters = printingRegisters;
	mips.printingMemory = printingMemory;
	mips.interactive = interactive;
	mips.debugging = debugging;
}

unsigned int endianSwap(unsigned int i) {
	return (i >> 24) | (i >> 8 & 0x0000ff00) | (i << 8 & 0x00ff0000) | (i << 24);
}

/*
 *  Run the simulation.
 */
void Simulate() {
	char s[40];  /* used for handling interactive input */
	unsigned int instr;
	int changedReg = -1, changedMem = -1, val;
	DecodedInstr d;

	/* Initialize the PC to the start of the code section */
	mips.pc = 0x00400000;
	while (1) {
		if (mips.interactive) {
			printf("> ");
			fgets(s, sizeof(s), stdin);
			if (s[0] == 'q') {
				return;
			}
		}

		/* Fetch instr at mips.pc, returning it in instr */
		instr = Fetch(mips.pc);

		printf("Executing instruction at %8.8x: %8.8x\n", mips.pc, instr);

		/*
	 * Decode instr, putting decoded instr in d
	 * Note that we reuse the d struct for each instruction.
	 */
		Decode(instr, &d, &rVals);

		/*Print decoded instruction*/
		PrintInstruction(&d);

		/*
	 * Perform computation needed to execute d, returning computed value
	 * in val
	 */
		val = Execute(&d, &rVals);

		UpdatePC(&d, val);

		/*
	 * Perform memory load or store. Place the
	 * address of any updated memory in *changedMem,
	 * otherwise put -1 in *changedMem.
	 * Return any memory value that is read, otherwise return -1.
		 */
		val = Mem(&d, val, &changedMem);

		/*
	 * Write back to register. If the instruction modified a register--
	 * (including jal, which modifies $ra) --
		 * put the index of the modified register in *changedReg,
		 * otherwise put -1 in *changedReg.
		 */
		RegWrite(&d, val, &changedReg);

		PrintInfo(changedReg, changedMem);
	}
}

/*
 *  Print relevant information about the state of the computer.
 *  changedReg is the index of the register changed by the instruction
 *  being simulated, otherwise -1.
 *  changedMem is the address of the memory location changed by the
 *  simulated instruction, otherwise -1.
 *  Previously initialized flags indicate whether to print all the
 *  registers or just the one that changed, and whether to print
 *  all the nonzero memory or just the memory location that changed.
 */
void PrintInfo(int changedReg, int changedMem) {
	int k, addr;
	printf("New pc = %8.8x\n", mips.pc);
	if (!mips.printingRegisters && changedReg == -1) {
		printf("No register was updated.\n");
	}
	else if (!mips.printingRegisters) {
		printf("Updated r%2.2d to %8.8x\n",
			changedReg, mips.registers[changedReg]);
	}
	else {
		for (k = 0; k < 32; k++) {
			printf("r%2.2d: %8.8x  ", k, mips.registers[k]);
			if ((k + 1) % 4 == 0) {
				printf("\n");
			}
		}
	}
	if (!mips.printingMemory && changedMem == -1) {
		printf("No memory location was updated.\n");
	}
	else if (!mips.printingMemory) {
		printf("Updated memory at address %8.8x to %8.8x\n",
			changedMem, Fetch(changedMem));
	}
	else {
		printf("Nonzero memory\n");
		printf("ADDR	  CONTENTS\n");
		for (addr = 0x00400000 + 4 * MAXNUMINSTRS;
			addr < 0x00400000 + 4 * (MAXNUMINSTRS + MAXNUMDATA);
			addr = addr + 4) {
			if (Fetch(addr) != 0) {
				printf("%8.8x  %8.8x\n", addr, Fetch(addr));
			}
		}
	}
}

/*
 *  Return the contents of memory at the given address. Simulates
 *  instruction fetch.
 */
unsigned int Fetch(int addr) {
	return mips.memory[(addr - 0x00400000) / 4];
}

/* Decode instr, returning decoded instruction. */
void Decode(unsigned int instr, DecodedInstr* d, RegVals* rVals) {
	/* Your code goes here */
	//get opcode

	d->op = (instr) >> 26;
	if (d->op == 0) {
		// R-type:
			   // opcode	rs	rt	rd	shamt	funct
			   // 31-26	25-21	20-16	15-11	10-6	5-0	
		d->type = R;
		d->regs.r.rs = (unsigned)(instr) << 6;
		d->regs.r.rs = (unsigned)d->regs.r.rs >> 27;
		d->regs.r.rt = (unsigned)(instr) << 11;
		d->regs.r.rt = (unsigned)d->regs.r.rt >> 27;
		d->regs.r.rd = (instr) << 16;
		d->regs.r.rd = d->regs.r.rd >> 27;
		d->regs.r.shamt = (instr) << 21;
		d->regs.r.shamt = d->regs.r.shamt >> 27;
		d->regs.r.funct = instr & 0x0000003f;

		rVals->R_rs = mips.registers[d->regs.r.rs];
		rVals->R_rt = mips.registers[d->regs.r.rt];
		rVals->R_rd = mips.registers[d->regs.r.rd];

	}
	else if (d->op == 0x2 || d->op == 0x3) {
		d->type = J;
		d->regs.j.target = instr << 6;
		d->regs.j.target = d->regs.j.target >> 4;

	}
	else {
		d->type = I;
		d->regs.i.rs = (instr) << 6;
		d->regs.i.rs = d->regs.i.rs >> 27;
		d->regs.i.rt = (instr) << 11;
		d->regs.i.rt = d->regs.i.rt >> 27;
		d->regs.i.addr_or_immed = (instr) << 16;
		d->regs.i.addr_or_immed = d->regs.i.addr_or_immed >> 16;
		//if(d->regs.i.addr_or_immed >> 15 == 1){
		//	d->regs.i.addr_or_immed = d->regs.i.addr_or_immed | 0xFFFF0000;
		//} else {
		//	d->regs.i.addr_or_immed = d->regs.i.addr_or_immed & 0x0000FFFF;
		//}



		rVals->R_rs = mips.registers[d->regs.i.rs];
		rVals->R_rt = mips.registers[d->regs.i.rt];


	}



	//printf("%d, %d", d->regs.r.rs, rVals->R_rs);
	//printf("%d, %d", d->regs.r.rt, rVals->R_rt);
	//printf("%d, %d", d->regs.r.rd, rVals->R_rd);

}







/*
 *  Print the disassembled version of the given instruction
 *  followed by a newline.
 */
void PrintInstruction(DecodedInstr* d) {
	/* Your code goes here */
	char *x;

	switch ((d->op))
	{
	case 0:
		/*
		printf("%d,", d->regs.r.rs);
		printf("%d,", d->regs.r.rt);
		printf("%d,", d->regs.r.rd);
		printf("%d,", d->regs.r.funct);
		*/
		//printf("%d,", d->regs.r.rs);
		switch (d->regs.r.funct) {
		case 0x21:
			x = "addu";
			break;
		case 0x23:
			x = "subu";
			break;
		case 0:
			x = "sll";
			break;
		case 0x02:
			x = "srl";
			break;
		case 0x20:
			x = "and";
			break;
		case 0x25:
			x = "or";
			break;
		case 0x2a:
			x = "slt";
			break;
		case 0x08:
			x = "jr";
			break;
		default:
			exit(0);
		} break;
		//j
	case 2:
		x = "j";
		break;
	case 3:
		x = "jal";
		break;

		//i 
	case 9:
		x = "addiu";
		break;
	case 12:
		x = "andi";
		break;
	case 13:
		x = "ori";
		break;
	case 15:
		x = "lui";
		break;
	case 4:
		x = "beq";
		break;
	case 5:
		x = "bne";
		break;
	case 35:
		x = "lw";
		break;
	case 43:
		x = "sw";
		break;

	default:
		exit(0);
	}


	if (d->type == R) {
		if (d->regs.r.funct == 0 || d->regs.r.funct == 2) {
			printf("%s\t$%d, $%d, %d\n", x, d->regs.r.rd, d->regs.r.rs, d->regs.r.shamt);
		}
		else if (d->regs.r.funct == 8) {
			printf("%s\t$%d\n", x, d->regs.r.rs);
		}
		else {
			printf("%s\t$%d, $%d, $%d\n", x, d->regs.r.rd, d->regs.r.rs, d->regs.r.rt);
		}

	}


	else if (d->type == J) {
		printf("%s\t0x%08x\n", x, d->regs.j.target);
	}


	else if (d->type == I) {
		if (d->op == 4 || d->op == 5) {
			printf("%s\t$%d, $%d, 0x%.8x\n", x,  d->regs.i.rs, d->regs.i.rt, mips.pc + 4 + (d->regs.i.addr_or_immed << 2)); 
		}
		else if (d->op == 35 || d->op == 43) {
			printf("%s\t$%d, %d($%d)\n", x, d->regs.i.rt, d->regs.i.addr_or_immed, d->regs.i.rs);
		}
		else if (d->op == 12 || d->op == 13 || d->op == 15) {
			printf("%s\t$%d, %d, 0x%x\n", x, d->regs.i.rt, d->regs.i.rs, (unsigned short)d->regs.i.addr_or_immed);
		}
		else {
			printf("%s\t$%d, %d, %d\n", x, d->regs.i.rt, d->regs.i.rs, d->regs.i.addr_or_immed);
		}

	}


}

/* Perform computation needed to execute d, returning computed value */
int Execute(DecodedInstr* d, RegVals* rVals) {
	/* Your code goes here */
	int result;
	switch (d->op)
	{
	case 0:
		switch (d->regs.r.funct) {
		case 0x21: //addu
			result = rVals->R_rs + rVals->R_rt;
			return result;


		case 35: //subu
			result = rVals->R_rs - rVals->R_rt;
			return result;
		case 0: //sll 
			result = rVals->R_rt << d->regs.r.shamt;
			return result;
		case 2: //srl
			result = rVals->R_rt >> d->regs.r.shamt;
			return result;
		case 36: //and 
			result = rVals->R_rs & rVals->R_rt;
			return result;
		case 37:
			result = rVals->R_rs | rVals->R_rt;
			return result;
		case 42: //slt
			result = rVals->R_rt < d->regs.r.shamt;
			return result;
		case 8: //jr
			result = rVals->R_rs;
			return result;

		} break;

		//j
	case 2:
		break;
	case 3:
		result = mips.pc + 4;
		return result;

		//i 
	case 9: //addiu
		result = rVals->R_rs + d->regs.i.addr_or_immed;
		return result;
	case 12: //andi
		result = rVals->R_rs & d->regs.i.addr_or_immed;
		return result;
	case 13: //ori
		result = rVals->R_rs | d->regs.i.addr_or_immed;
		return result;
	case 15: //lui
		result = d->regs.i.addr_or_immed << 16;
		return result;
	case 4: //beq
		result = rVals->R_rs == rVals->R_rt;
		return result;
	case 5: //bne
		result = rVals->R_rs != rVals->R_rt;
		return result;
	case 35: //lw
		result = rVals->R_rs + d->regs.i.addr_or_immed;
		return result;
	case 43: //sw
		result = rVals->R_rs + d->regs.i.addr_or_immed;
		return result;

	}
	return 0;
}

/*
 * Update the program counter based on the current instruction. For
 * instructions other than branches and jumps, for example, the PC
 * increments by 4 (which we have provided).
 */
void UpdatePC(DecodedInstr* d, int val) {
	mips.pc += 4;
	if (d->regs.r.funct == 8)
		mips.pc = mips.registers[d->regs.r.rs];
	else if (d->op == 4 || d->op == 5) {
		if (val)
			mips.pc += d->regs.i.addr_or_immed << 2;
	}
	else if (d->type == J) {
		if (d->op == 3) {
			mips.registers[31] = mips.pc + 8;
			mips.pc = d->regs.j.target;
		}
		mips.pc = d->regs.j.target;
	}
}

/*
 * Perform memory load or store. Place the address of any updated memory
 * in *changedMem, otherwise put -1 in *changedMem. Return any memory value
 * that is read, otherwise return -1.
 *
 * Remember that we're mapping MIPS addresses to indices in the mips.memory
 * array. mips.memory[0] corresponds with address 0x00400000, mips.memory[1]
 * with address 0x00400004, and so forth.
 *
 */
int Mem(DecodedInstr* d, int val, int *changedMem) {
	/* Your code goes here */
	if (d->op == 43) {
		if ((val < 0x00401000 || val >= 0x00403fff || val % 4 != 0)) {
			printf("Memory Access Exception at [0x%08x]: address [0x%08x]\n", mips.pc, val);
			exit(0);
		}
		mips.memory[(val - 0x00400000) / 4] = mips.registers[d->regs.i.rt];
		*changedMem = val;
	}
	else if (d->op == 35) {
		if ((val < 0x00401000 || val >= 0x00403fff || val % 4 != 0)) {
			printf("Memory Access Exception at [0x%08x]: address [0x%08x]\n", mips.pc, val);
			exit(0);
		}
		mips.registers[d->regs.i.rt] = Fetch(val);
		*changedMem = -1;
	}

	return val; 

	}
	


/*
 * Write back to register. If the instruction modified a register--
 * (including jal, which modifies $ra) --
 * put the index of the modified register in *changedReg,
 * otherwise put -1 in *changedReg.
 */
void RegWrite(DecodedInstr* d, int val, int *changedReg) {
	/* Your code goes here */
	*changedReg = -1;
	if (d->op == 0) {
		switch (d->regs.r.funct) {
		case 0x21: //addu
		case 0x23: //subu
		case 0: //sll 
		case 0x02: //srl
		case 0x20: //and 
		case 0x25:
		case 0x2a: //slt
		case 0x08: //jr

			*changedReg = d->regs.r.rd;
			mips.registers[d->regs.r.rd] = val;
			return;


		}

		return;
	}
	else if (d->op == 3) {
		*changedReg = 31;
		mips.registers[31] = val;
		return;
	}
	else if(d->op == 9 || d->op == 12 || d->op == 13 || d->op == 15 || d->op == 35 || d->op == 43){
		*changedReg = d->regs.i.rt;
		mips.registers[d->regs.i.rt] = val;
		return;
	}




}