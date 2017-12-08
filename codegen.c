/* codgen.c       Generate Assembly Code for x86         11 Oct 17   */

/* Copyright (c) 2017 Gordon S. Novak Jr. and The University of Texas at Austin
    */

/* Starter file for CS 375 Code Generation assignment.           */
/* Written by Gordon S. Novak Jr.                  */

/* This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License (file gpl.text) for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA. */

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include "token.h"
#include "symtab.h"
#include "lexan.h"
#include "genasm.h"
#include "codegen.h"

void genc(TOKEN code);

/* Set DEBUGGEN to 1 for debug printouts of code generation */
#define DEBUGGEN 1
#define REGISTERS 32

int nextlabel;    /* Next available label number */
int stkframesize;   /* total stack frame size */
int registertable[REGISTERS]; /* Register Table */

/* Top-level entry for code generator.
   pcode    = pointer to code:  (program foo (output) (progn ...))
   varsize  = size of local storage in bytes
   maxlabel = maximum label number used so far

Add this line to the end of your main program:
    gencode(parseresult, blockoffs[blocknumber], labelnumber);
The generated code is printed out; use a text editor to extract it for
your .s file.
         */

void gencode(TOKEN pcode, int varsize, int maxlabel)
  {  TOKEN name, code;
     name = pcode->operands;
     code = name->link->link;
     nextlabel = maxlabel + 1;
     stkframesize = asmentry(name->stringval,varsize);
     genc(code);
     asmexit(name->stringval);
  }

int getreg(int kind)
  {
    int chosen_register;

    if (kind == 1) {  
      for (int i = RBASE; i <= RMAX; i ++) {
        if (registertable[i] == 0) {
          used(i);
          chosen_register =  i; 
          break;
        }
      }
    } else {
      for (int i = FBASE; i <= FMAX; i ++) {
        if (registertable[i] == 0) {
          used(i);
          chosen_register = i;
          break;
        }
      }
    }

    if (DEBUGGEN) {
      printf("getreg\n");
      printf("chosen %d\n", chosen_register);
    }
    return chosen_register;
  }

/* Generate code for arithmetic expression, return a register number */
int genarith(TOKEN code)
  { 
    int num, reg, offs;
    SYMBOL sym;
    float value;
    TOKEN lhs, rhs;

    if (DEBUGGEN) { 
      printf("genarith\n");
	    dbugprinttok(code);
    };

    switch ( code->tokentype ) { 
      case NUMBERTOK:   switch (code->datatype) { 
                            case INTEGER: num = code->intval;
                                          reg = getreg(1);
                                     		  if ( num >= MINIMMEDIATE && num <= MAXIMMEDIATE )
                                   		    asmimmed(MOVL, num, reg);
                                     		  break;
    	                      case REAL:    value = code->realval;
                                          reg = getreg(2);
                                          int label = nextlabel ++;
                                          makeflit(value, label);
                                          asmldflit(MOVSD, label, reg);
                                          break;
              	        }
                        break;

      case IDENTIFIERTOK: //If not a function
                          sym =  code->symentry;
                          offs = sym->offset - stkframesize;
                          switch (code->datatype) {
                            case INTEGER: reg = getreg(1);
                                          asmld(MOVL, offs, reg, code->stringval);
                                          break;
                            case REAL:    reg = getreg(2);
                                          asmld(MOVSD, offs, reg, code->stringval); 
                                          break;
                            case POINTER: break; //TODO (MOVQ)
                          }
                          break;

      case OPERATOR:  lhs = code->operands;
                      rhs = lhs->link;
                      int reg1 = genarith(lhs);     //HANDLE POINT, DOT AND AREF HERE?
                      int reg2 = genarith(rhs);     //ASK PROF IF THIS IS THE CORRECT WAY TO DO THIS
                      switch (code->whichval) {     //ASK PROF how to decide if ADDL or ADDSD
                        case PLUSOP: asmrr(ADDL, reg1, reg2); break;
                        case MINUSOP: asmrr(SUBL, reg1, reg2); break; /* fill more */
                        case TIMESOP: break;
                        case DIVIDEOP: break;


                        case EQOP: case NEOP: case LTOP: case LEOP: case GEOP: case GTOP: asmrr(CMPL, reg1, reg2); break;
                        case AREFOP: break;

                      }
                      reg = reg2;
                      unused(reg1);
                      break;
    };

     return reg;
  }


/* Generate code for a Statement from an intermediate-code form */
void genc(TOKEN code)
  {  TOKEN tok, lhs, rhs;
     int reg, offs;
     SYMBOL sym;
     clearreg();

     if (DEBUGGEN) {
      printf("genc\n");3
	    dbugprinttok(code);
     };

     if ( code->tokentype != OPERATOR ) { 
          printf("Bad code token");
      	  dbugprinttok(code);
      };

     switch ( code->whichval ) {
      case PROGNOP:  tok = code->operands;
                	   while ( tok != NULL ) {
                       genc(tok);
                		   tok = tok->link;
                	   }; 
                	   break;

                    // Modify this to shift this lower trivial case into genarith
                    //ASK PROF ABOUT THIS
      case ASSIGNOP: lhs = code->operands;      /* Trivial version: handles I := e */
                	   rhs = lhs->link;
                	   reg = genarith(rhs);              /* generate rhs into a register */
                	   sym = lhs->symentry;              /* assumes lhs is a simple var  */
                	   offs = sym->offset - stkframesize; /* net offset of the var   */
                     switch (code->datatype) {          /* store value into lhs  */
                       case INTEGER: asmst(MOVL, reg, offs, lhs->stringval);
                                     break;
                                 /* ...  */
                     };
                     break; 

      case GOTOOP: asmjump(JMP, code->operands->intval);
                   break;

      case LABELOP: asmlabel(code->operands->intval);
                    break;

      case IFOP:  tok = code->operands;
                  reg = genarith(tok);        //ASK PROF PAGE 236 237

                  break;

      case FUNCALLOP: break;
	   };
  }


/* Generate code for a function call */
int genfun(TOKEN code);

/* find the correct MOV op depending on type of code */
int moveop(TOKEN code);

  void clearreg() {
    for (int i = 0; i < REGISTERS; i ++) {
      unused(i);
    }
  }

  void unused(int reg) {
    registertable[reg] = 0;
  }

  void used(int reg){
    registertable[reg] = 1; 
  }
