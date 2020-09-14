#include "cgen.h"
#include "dast.h"
#include "instructions.h"
#include "decorate.h"
#include "utils.h"
#include "cgen-helpers.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


string_label_map* s = NULL;

void GenerateCode(DAST* dast, char* filename) {
  Decl** globals = global->children;
  int global_len = global->children_size;
  // indicate we expect a main function
  emitGLOBLMAIN ();
  // set up globals (vars, strings)
  emitDATA ();
  // First sort data based on offset
  for (int i = 0; i < global_len; i++) {
    Decl* curr = globals[i];
    // Allocating for a string requires a label
    if (curr->group == DECL_STR) {
      // if we're declaring a string
      char* label = generateStringLabel(curr->contents.str_info.str_literal);
      emitLABEL(label);
      generateString(curr);
    // Allocating for a global variable just requires initializing a value
    } else if (curr->group == DECL_VAR) {
      if (curr->is_string) {
        // if our global var has a str value
        generateDataLabel (generateStringLabel (curr->contents.str_info.str_literal), curr->data_size);
      } else {
        // if any other value
        int64_t value = 0;
        for (int k = WORDSIZE; k >= 0; k--) {
          value = value << 8;
          value += curr->contents.value[k];
        }
        generateDataValue (value, curr->data_size);
      }
    }
  }
  emitTEXT ();
  dispatch(dast, NULL, NULL);

  freeStringList(s);
}

void processDefault(DAST* dast, char* startLabel, char* endLabel) {
  for (int i = 0; i < dast->size; i++) {
    dispatch(dast->children[i], startLabel, endLabel);
  }
}

void processID(DAST* dast, char* startLabel, char* endLabel) {
  enum DeclLevel level = dast->decl->level;
  int offset = dast->decl->offset;
  if (level == FUNCTION) {
    emitADDI(S1, FP, offset);
    emitLW(S1, 0, S1);
  } else if (level == GLOBAL) {
    emitADDI(S1, GP, offset);
    emitLW(S1, 0, S1);
  }
  // struct processing is done in dot and arrow expr
}

void processConstTrue(DAST* dast, char* startLabel, char* endLabel) {
  emitADDI(S1, x0, 1);
}

void processConstFalse(DAST* dast,
                       char* startLabel,
                       char* endLabel) {
  emitADDI(S1, x0, 0);
}

void processConstInt(DAST* dast, char* startLabel, char* endLabel) {
  emitADDI(S1, x0, dast->data.integer);
}

void processConstNull(DAST* dast, char* startLabel, char* endLabel) {
  emitADDI(S1, x0, 0);
}

void processConstChar(DAST* dast, char* startLabel, char* endLabel) {
  char c = dast->data.character;
  emitADDI(S1, x0, (int)c);
}

void processConstStr(DAST* dast, char* startLabel, char* endLabel) {
  emitLA(S1, getStringLabel(s, dast->data.string));
}

void processExprBinaryAdd(DAST* dast,
                          char* startLabel,
                          char* endLabel) {
  /* YOUR CODE HERE */
	DAST* LHS = dast->children[0];
	DAST* RHS = dast->children[1];

	dispatch(LHS, startLabel, endLabel);
	emitADDI(SP, SP, -1 * WORDSIZE); // make room on the stack/move pointer
	emitSW(S1, 0, SP); // save value to stack

  dispatch(RHS, startLabel, endLabel); // get right value
	emitLW(T0, 0, SP); // load left expr result from stack
  emitADD(S1, T0, S1); // add values and place result in S1 to keep invariant true
	emitADDI(SP, SP, WORDSIZE); // erase value from stack/restore pointer
}

void processExprBinarySub(DAST* dast,
                          char* startLabel,
                          char* endLabel) {
  /* YOUR CODE HERE */
	DAST* LHS = dast->children[0];
	DAST* RHS = dast->children[1];

	dispatch(LHS, startLabel, endLabel);
	emitADDI(SP, SP, -1 * WORDSIZE); // make room on the stack/move pointer
	emitSW(S1, 0, SP); // save value to stack

  dispatch(RHS, startLabel, endLabel); // get right value
  emitLW(T0, 0, SP); // load left expr result from stack
  emitSUB(S1, T0, S1); // subtract values and place result in S1 to keep invariant true
  emitADDI(SP, SP, WORDSIZE); // erase value from stack/restore pointer
}

void processExprBinaryMul(DAST* dast,
                          char* startLabel,
                          char* endLabel) {
  /* YOUR CODE HERE */
	DAST* LHS = dast->children[0];
	DAST* RHS = dast->children[1];

	dispatch(LHS, startLabel, endLabel);
	emitADDI(SP, SP, -1 * WORDSIZE); // make room on the stack/move pointer
	emitSW(S1, 0, SP); // save value to stack

	dispatch(RHS, startLabel, endLabel); // get right value
	emitLW(T0, 0, SP); // load left expr result from stack
	emitMUL(S1, T0, S1); // multiply values and place result in S1 to keep invariant true
	emitADDI(SP, SP, WORDSIZE); // erase value from stack/restore pointer
}

void processExprBinaryDiv(DAST* dast,
                          char* startLabel,
                          char* endLabel) {
  /* YOUR CODE HERE */
	DAST* LHS = dast->children[0];
	DAST* RHS = dast->children[1];

	dispatch(LHS, startLabel, endLabel);
	emitADDI(SP, SP, -1 * WORDSIZE); // make room on the stack/move pointer
	emitSW(S1, 0, SP); // save value to stack

  dispatch(RHS, startLabel, endLabel); // get right value
	emitLW(T0, 0, SP); // load left expr result from stack
  emitDIV(S1, T0, S1); // divide on values and place result in S1 to keep invariant true
	emitADDI(SP, SP, WORDSIZE); // erase value from stack/restore pointer
}

void processExprBinaryEq(DAST* dast,
                         char* startLabel,
                         char* endLabel) {
  /* YOUR CODE HERE */
	DAST* LHS = dast->children[0];
	DAST* RHS = dast->children[1];

	dispatch(LHS, startLabel, endLabel);
	emitADDI(SP, SP, -1 * WORDSIZE); // make room on the stack/move pointer
	emitSW(S1, 0, SP); // save value to stack

  dispatch(RHS, startLabel, endLabel); // get right value
  emitLW(T0, 0, SP); // load left expr result from stack
  emitSLT(T1, T0, S1); // check if LHS < RHS, place result in T1
  emitSLT(T0, S1, T0); // check if RHS < LHS, place result in T0
	emitOR(T0, T0, T1); // check if values equal and place result in T0
	emitADDI(S1, T0, -1); // negate T0 and place result in S1
	emitADDI(SP, SP, WORDSIZE); // erase value from stack/restore pointer
}

void processExprBinaryNotEq(DAST* dast,
                            char* startLabel,
                            char* endLabel) {
  /* YOUR CODE HERE */
	DAST* LHS = dast->children[0];
	DAST* RHS = dast->children[1];

	dispatch(LHS, startLabel, endLabel);
	emitADDI(SP, SP, -1 * WORDSIZE); // make room on the stack/move pointer
	emitSW(S1, 0, SP); // save value to stack

  dispatch(RHS, startLabel, endLabel); // get right value
  emitLW(T0, 0, SP); // load left expr result from stack
  emitSUB(S1, T0, S1); // subtract values and place result in S1
	emitADDI(SP, SP, WORDSIZE); // erase value from stack/restore pointer
}

void processExprBinaryGTEq(DAST* dast,
                           char* startLabel,
                           char* endLabel) {
 /* YOUR CODE HERE */
	DAST* LHS = dast->children[0];
	DAST* RHS = dast->children[1];

	dispatch(LHS, startLabel, endLabel);
	emitADDI(SP, SP, -1 * WORDSIZE); // make room on the stack/move pointer
	emitSW(S1, 0, SP); // save value to stack

  dispatch(RHS, startLabel, endLabel); // get right value
  emitLW(T0, 0, SP); // load left expr result from stack
  emitSLT(T0, T0, S1); // compare if R1 is SLT R2 and place result in T0
  emitSEQZ(S1, T0);
	emitADDI(SP, SP, WORDSIZE); // erase value from stack/restore pointer
}

void processExprBinaryGT(DAST* dast,
                         char* startLabel,
                         char* endLabel) {
  /* YOUR CODE HERE */
	DAST* LHS = dast->children[0];
	DAST* RHS = dast->children[1];

	dispatch(LHS, startLabel, endLabel);
	emitADDI(SP, SP, -1 * WORDSIZE); // make room on the stack/move pointer
	emitSW(S1, 0, SP); // save value to stack

  dispatch(RHS, startLabel, endLabel); // get right value
	emitLW(T0, 0, SP); // load left expr result from stack
	emitSLT(S1, S1, T0); // compare T0 and S1, and place result in S1
  emitADDI(SP, SP, WORDSIZE); // erase value from stack/restore pointer
}

void processExprBinaryLTEq(DAST* dast,
                           char* startLabel,
                           char* endLabel) {
  /* YOUR CODE HERE */
	DAST* LHS = dast->children[0];
	DAST* RHS = dast->children[1];

	dispatch(LHS, startLabel, endLabel);
	emitADDI(SP, SP, -1 * WORDSIZE); // make room on the stack/move pointer
	emitSW(S1, 0, SP); // save value to stack

  dispatch(RHS, startLabel, endLabel); // get right value
  emitLW(T0, 0, SP); // load left expr result from stack
	emitSLT(T0, S1, T0); // compare values and place result in T0
  emitADDI(S1, T0, -1); // negate T0 and place result in S1
	emitADDI(SP, SP, WORDSIZE); // erase value from stack/restore pointer
}

void processExprBinaryLT(DAST* dast,
                         char* startLabel,
                         char* endLabel) {
  /* YOUR CODE HERE */
	DAST* LHS = dast->children[0];
	DAST* RHS = dast->children[1];

	dispatch(LHS, startLabel, endLabel);
	emitADDI(SP, SP, -1 * WORDSIZE); // make room on the stack/move pointer
	emitSW(S1, 0, SP); // save value to stack

  dispatch(RHS, startLabel, endLabel); // get right value
  emitLW(T0, 0, SP); // load left expr result from stack
  emitSLT(S1, T0, S1); // compare if R1 is SLT R2 and place result in S1
	emitADDI(SP, SP, WORDSIZE); // erase value from stack/restore pointer
}

void processExprBinaryLogicAnd(DAST* dast,
                               char* startLabel,
                               char* endLabel) {
  /* YOUR CODE HERE */
	DAST* LHS = dast->children[0];
	DAST* RHS = dast->children[1];

	dispatch(LHS, startLabel, endLabel);
	emitADDI(SP, SP, -1 * WORDSIZE); // make room on the stack/move pointer
	emitSW(S1, 0, SP); // save value to stack

  dispatch(RHS, startLabel, endLabel); // get right value
	emitLW(T0, 0, SP); // load left expr result from stack
  emitSNEZ(T0, T0);
  emitSNEZ(S1, S1);
  emitAND(S1, T0, S1);
	emitADDI(SP, SP, WORDSIZE); // erase value from stack/restore pointer
}

void processExprBinaryLogicOr(DAST* dast,
                              char* startLabel,
                              char* endLabel) {
  /* YOUR CODE HERE */
	DAST* LHS = dast->children[0];
	DAST* RHS = dast->children[1];

	dispatch(LHS, startLabel, endLabel);
	emitADDI(SP, SP, -1 * WORDSIZE); // make room on the stack/move pointer
	emitSW(S1, 0, SP); // save value to stack

  dispatch(RHS, startLabel, endLabel); // get right value
	emitLW(T0, 0, SP); // load left expr result from stack
  emitSNEZ(T0, T0);
  emitSNEZ(S1, S1);
  emitOR(S1, T0, S1);
	emitADDI(SP, SP, WORDSIZE); // erase value from stack/restore pointer
}

void processExprBinaryBitAnd(DAST* dast,
                             char* startLabel,
                             char* endLabel) {
  /* YOUR CODE HERE */
	DAST* LHS = dast->children[0];
	DAST* RHS = dast->children[1];

	dispatch(LHS, startLabel, endLabel);
	emitADDI(SP, SP, -1 * WORDSIZE); // make room on the stack/move pointer
	emitSW(S1, 0, SP); // save value to stack

  dispatch(RHS, startLabel, endLabel); // get right value
	emitLW(T0, 0, SP); // load left expr result from stack
	emitAND(S1, T0, S1); // AND values and place result in S1 to keep invariant true
	emitADDI(SP, SP, WORDSIZE); // erase value from stack/restore pointer
}

void processExprBinaryBitOr(DAST* dast,
                            char* startLabel,
                            char* endLabel) {
  /* YOUR CODE HERE */
	DAST* LHS = dast->children[0];
	DAST* RHS = dast->children[1];

	dispatch(LHS, startLabel, endLabel);
	emitADDI(SP, SP, -1 * WORDSIZE); // make room on the stack/move pointer
	emitSW(S1, 0, SP); // save value to stack

  dispatch(RHS, startLabel, endLabel); // get right value
	emitLW(T0, 0, SP); // load left expr result from stack
  emitOR(S1, T0, S1); // OR values and place result in S1 to keep invariant true
	emitADDI(SP, SP, WORDSIZE); // erase value from stack/restore pointer
}

void processExprBinaryBitXor(DAST* dast,
                             char* startLabel,
                             char* endLabel) {
  /* YOUR CODE HERE */
	DAST* LHS = dast->children[0];
	DAST* RHS = dast->children[1];

	dispatch(LHS, startLabel, endLabel);
	emitADDI(SP, SP, -1 * WORDSIZE); // make room on the stack/move pointer
	emitSW(S1, 0, SP); // save value to stack

  dispatch(RHS, startLabel, endLabel); // get right value
	emitLW(T0, 0, SP); // load left expr result from stack
	emitXOR(S1, T0, S1); // XOR values and place result in S1 to keep invariant true
	emitADDI(SP, SP, WORDSIZE); // erase value from stack/restore pointer
}

void processExprPrefixNegate(DAST* dast,
                             char* startLabel,
                             char* endLabel) {
  DAST* child1 = dast->children[0];

  // cgen and load arguments
  dispatch(child1, startLabel, endLabel);
  // negate
  emitADDI(T0, x0, -1);
  emitMUL(S1, S1, T0);
}

void processExprPrefixPlusPlus(DAST* dast, char* startLabel, char* endLabel) {
  // Fetch variable's address
  dispatchLeft (dast->children[0], startLabel, endLabel);

  // Move address into s2
  emitMV (S2, S1);
  
  // Load and change value
  emitLW (S1, 0, S2);
  emitADDI (S1, S1, 1);

  // Store the new value
  emitSW (S1, 0, S2);
}

void processExprPrefixMinusMinus(DAST* dast, char* startLabel, char* endLabel) {
  // Fetch variable's address
  dispatchLeft (dast->children[0], startLabel, endLabel);

  // Move address into s2
  emitMV (S2, S1);
  
  // Load and change value
  emitLW (S1, 0, S2);
  emitADDI (S1, S1, -1);

  // Store the new value
  emitSW (S1, 0, S2);
}

void processExprPrefixBitNot(DAST* dast,
                             char* startLabel,
                             char* endLabel) {
  DAST* child1 = dast->children[0];

  dispatch(child1, startLabel, endLabel);
  // do bitwise negate
  emitXORI(S1, S1, -1);  // get all ones mask
}

void processExprPrefixLogicNot(DAST* dast,
                               char* startLabel,
                               char* endLabel) {
  DAST* child1 = dast->children[0];

  dispatch(child1, startLabel, endLabel);

  emitSLTIU(
      S1, S1,
      1);  // return if the value is less than 1 unsigned aka 1 if 0 else 0
}

void processExprPostfixPlusPlus(DAST* dast, char* startLabel, char* endLabel) {
  // Fetch variable's address
  dispatchLeft (dast->children[0], startLabel, endLabel);

  // Move address into s2
  emitMV (S2, S1);
  
  // Load and change value
  emitLW (S1, 0, S2);
  emitADDI (S3, S1, 1);

  // Store the new value
  emitSW (S3, 0, S2);
}

void processExprPostfixMinusMinus(DAST* dast, char* startLabel, char* endLabel) {
  // Fetch variable's address
  dispatchLeft (dast->children[0], startLabel, endLabel);

  // Move address into s2
  emitMV (S2, S1);
  
  // Load and change value
  emitLW (S1, 0, S2);
  emitADDI (S3, S1, -1);

  // Store the new value
  emitSW (S3, 0, S2);
}

void processFuncDecl(DAST* dast, char* startLabel, char* endLabel) {
  if (dast->size == 4) {
    // only cgen on functions with bodies
    DAST* func_id = dast->children[1];
    DAST* func_body = dast->children[3];

    emitLABEL(func_id->data.identifier);
    /*
      YOUR CODE HERE
      - Prologue
      - Generate code for function body
      - Set up stack and frame
    */
	  emitADDI(SP, SP, -1 * WORDSIZE); // make room on the stack/move pointer
	  emitSW(FP, 0, SP); // save the value of FP
    emitMV(FP, SP);
    emitADDI(SP, SP, -12 * WORDSIZE);

    emitSW(RA, -1 * WORDSIZE, FP);
    emitSW(S2, -2 * WORDSIZE, FP);
    emitSW(S3, -3 * WORDSIZE, FP);
    emitSW(S4, -4 * WORDSIZE, FP);
    emitSW(S5, -5 * WORDSIZE, FP);
    emitSW(S6, -6 * WORDSIZE, FP);
    emitSW(S7, -7 * WORDSIZE, FP);
    emitSW(S8, -8 * WORDSIZE, FP);
    emitSW(S9, -9 * WORDSIZE, FP);
    emitSW(S10, -10 * WORDSIZE, FP);
    emitSW(S11, -11 * WORDSIZE, FP);

    dispatch(func_body, startLabel, endLabel);
    
    // produce a label for return statements to come back to
    char *total_string = generateFunctionEndLabel (func_id->data.identifier);
    emitLABEL (total_string);
    free (total_string);

    /*
      YOUR CODE HERE
      - Epilogue
      - Restore stack and frame
    */
    emitLW(RA, -1 * WORDSIZE, FP);
    emitLW(S2, -2 * WORDSIZE, FP);
    emitLW(S3, -3 * WORDSIZE, FP);
    emitLW(S4, -4 * WORDSIZE, FP);
    emitLW(S5, -5 * WORDSIZE, FP);
    emitLW(S6, -6 * WORDSIZE, FP);
    emitLW(S7, -7 * WORDSIZE, FP);
    emitLW(S8, -8 * WORDSIZE, FP);
    emitLW(S9, -9 * WORDSIZE, FP);
    emitLW(S10, -10 * WORDSIZE, FP);
    emitLW(S11, -11 * WORDSIZE, FP);

    emitMV(SP, FP);
    emitLW(FP, 0, SP);
    emitADDI(SP, SP, WORDSIZE);

    if (strcmp ("main", func_id->data.identifier) == 0) {
      // If we are the main function we want to exit to
      // be compatible with venus.
      emitMV (A1, A0);
      emitADDI (A0, x0, 17);
      emitECALL ();
    } else {
      // Jump out of the function
      emitJR(RA);
    }
  }
}

void processExprCall(DAST* dast, char* startLabel, char* endLabel) {
  /* YOUR CODE HERE */
  if (dast->size == 2) {
    DAST* expr_id = dast->children[0];
    DAST* arg_list= dast->children[1];

    emitADDI(SP, SP, -1 * WORDSIZE * arg_list->size); // make room on the stack/move pointer

    for(int i = 0; i < arg_list->size; i++) {
      dispatch(arg_list->children[arg_list->size - i - 1], startLabel, endLabel);
      emitSW(S1, (arg_list->size - i - 1) * WORDSIZE, SP);
    }
    emitJAL(RA, expr_id->data.identifier);
    emitADDI(SP, SP, WORDSIZE * arg_list->size);
  }
}

void processBlock(DAST* dast, char* startLabel, char* endLabel) {
  // figure out how much to move stack by
  int bytes_used = dast->decl->data_size;
  if (bytes_used > 0) {
    emitADDI(SP, SP, -1 * bytes_used);
  }

  // dispatch on all children
  for (int i = 0; i < dast->size; i++) {
    dispatch(dast->children[i], startLabel, endLabel);
  }
  if (bytes_used > 0) {
    emitADDI(SP, SP, bytes_used);
  }
}

void processIfElse(DAST* dast, char* startLabel, char* endLabel) {
  /* YOUR CODE HERE */
  if (dast->size == 2) {
    DAST* condition = dast->children[0];
    DAST* if_case = dast->children[1];

    char* if_true = generateLocalLabel();
    char* if_false = generateLocalLabel();

    dispatch(condition, startLabel, endLabel);
    emitBNEZ(S1, if_true);
    emitJ(if_false);

    emitLABEL(if_true);
    dispatch(if_case, startLabel, endLabel);
    
    emitLABEL(if_false);
  }

  else if (dast->size == 3) {
    DAST* condition = dast->children[0];
    DAST* if_case = dast->children[1];
    DAST* else_case = dast->children[2];

    char* if_true = generateLocalLabel();
    char* else_true = generateLocalLabel();
    char* end = generateLocalLabel();

    dispatch(condition, startLabel, endLabel);
    emitBNEZ(S1, if_true);
    emitJ(else_true);

    emitLABEL(if_true);
    dispatch(if_case, startLabel, endLabel);
    emitJ(end);

    emitLABEL(else_true);
    dispatch(else_case, startLabel, endLabel);
    emitJ(end);

    emitLABEL(end);
  }
}

void processFor(DAST* dast, char* startLabel, char* endLabel) {
  char* start_for = generateLocalLabel();
  char* middle_for = generateLocalLabel();
  char* end_for = generateLocalLabel();

  // Initialize condition
  dispatch(dast->children[0], start_for, end_for);

  // Jump to condition
  emitJ(middle_for);

  // Label for start of the loop
  emitLABEL(start_for);

  //Update condition on loop
  dispatch(dast->children[2], start_for, end_for);

  // Label for cond. This is to skip first update
  emitLABEL(middle_for);

  // Loop condition bounding
  dispatch(dast->children[1], start_for, end_for);
  emitBEQZ (S1, end_for);
  dispatch(dast->children[3], start_for, end_for);

  // Jump back to update
  emitJ (start_for);

  emitLABEL(end_for);

}

void processVarDecl(DAST* dast, char* startLabel, char* endLabel) {
  // Only need to initialize a var if it has an expr
  if (dast->size > 3 && dast->decl->level == FUNCTION) {
    // dispatch on expr
    dispatch(dast->children[3], startLabel, endLabel);

    int offset = dast->decl->offset;
    emitSW(S1, offset, FP);
  }
}

void processReturn(DAST* dast, char* startLabel, char* endLabel) {
  dispatch(dast->children[0], startLabel, endLabel);
  emitMV(A0, S1);
  // Generate the return label for the function
  char *total_string = generateFunctionEndLabel (dast->decl->identifier);
  // Jump to the return label
  emitJ(total_string);
  free(total_string);
}

void processExprAssign(DAST* dast,
                       char* startLabel,
                       char* endLabel) {
  DAST* LHS = dast->children[0];
  DAST* RHS = dast->children[1];

  // get address to store at
  dispatchLeft(LHS, startLabel, endLabel);

  emitADDI(SP, SP, -1 * WORDSIZE);
  emitSW(S1, 0, SP);
  // get value
  dispatch(RHS, startLabel, endLabel);
  emitLW(T0, 0, SP);

  // store value in expr location
  emitSW(S1, 0, T0);
  emitADDI(SP, SP, WORDSIZE);
}

void processAddr(DAST* dast, char* startLabel, char* endLabel) {

  // treat as LHS expr to get address
  dispatchLeft(dast->children[0], startLabel, endLabel);
}

void processDeref(DAST *dast, char* startLabel, char* endLabel) {
  // Evaluate expression
  dispatch(dast->children[0], startLabel, endLabel);
  
  // Dereference that pointer
  emitLW (S1, 0, S1);
}

void processDot(DAST* dast, char* startLabel, char* endLabel) {
  dispatchLeft(dast, startLabel, endLabel);
  // we have gotten the address of our field, now use to load
  emitLW(S1, 0, S1);
}

void processArrow(DAST* dast, char* startLabel, char* endLabel) {
  dispatchLeft(dast, startLabel, endLabel);
  // we have gotten the address of our field, now use to load
  emitLW(S1, 0, S1);
}

void processBreak(DAST* dast, char* startLabel, char* endLabel) {
  emitJ(endLabel);
}

void processContinue(DAST* dast, char* startLabel, char* endLabel) {
  emitJ(startLabel);
}

void dispatch(DAST* dast, char* startLabel, char* endLabel) {
  switch (dast->type) {
    case (NODETYPE_CONSTANT_INTEGER):
      processConstInt(dast, startLabel, endLabel);
      break;
    case (NODETYPE_CONSTANT_STRING):
      processConstStr(dast, startLabel, endLabel);
      break;
    case (NODETYPE_CONSTANT_CHARACTER):
      processConstChar(dast, startLabel, endLabel);
      break;
    case (NODETYPE_CONSTANT_TRUE):
      processConstTrue(dast, startLabel, endLabel);
      break;
    case (NODETYPE_CONSTANT_FALSE):
      processConstFalse(dast, startLabel, endLabel);
      break;
    case (NODETYPE_CONSTANT_NULL):
      processConstNull(dast, startLabel, endLabel);
      break;
    case (NODETYPE_CONTROL_IF_ELSE):
      processIfElse(dast, startLabel, endLabel);
      break;
    case (NODETYPE_CONTROL_FOR):
      processFor(dast, startLabel, endLabel);
      break;
    case (NODETYPE_VAR_DECL):
      processVarDecl(dast, startLabel, endLabel);
      break;
    case (NODETYPE_FUNC_DECL):
      processFuncDecl(dast, startLabel, endLabel);
      break;
    case (NODETYPE_EXPR_BINARY_ADD):
      processExprBinaryAdd(dast, startLabel, endLabel);
      break;
    case (NODETYPE_EXPR_BINARY_SUB):
      processExprBinarySub(dast, startLabel, endLabel);
      break;
    case (NODETYPE_EXPR_BINARY_MULT):
      processExprBinaryMul(dast, startLabel, endLabel);
      break;
    case (NODETYPE_EXPR_BINARY_DIV):
      processExprBinaryDiv(dast, startLabel, endLabel);
      break;
    case (NODETYPE_EXPR_BINARY_EQUAL):
      processExprBinaryEq(dast, startLabel, endLabel);
      break;
    case (NODETYPE_EXPR_BINARY_NOTEQUAL):
      processExprBinaryNotEq(dast, startLabel, endLabel);
      break;
    case (NODETYPE_EXPR_BINARY_GREATEREQUAL):
      processExprBinaryGTEq(dast, startLabel, endLabel);
      break;
    case (NODETYPE_EXPR_BINARY_GREATER):
      processExprBinaryGT(dast, startLabel, endLabel);
      break;
    case (NODETYPE_EXPR_BINARY_LESSEQUAL):
      processExprBinaryLTEq(dast, startLabel, endLabel);
      break;
    case (NODETYPE_EXPR_BINARY_LESS):
      processExprBinaryLT(dast, startLabel, endLabel);
      break;
    case (NODETYPE_EXPR_BINARY_ANDAND):
      processExprBinaryLogicAnd(dast, startLabel, endLabel);
      break;
    case (NODETYPE_EXPR_BINARY_OROR):
      processExprBinaryLogicOr(dast, startLabel, endLabel);
      break;
    case (NODETYPE_EXPR_BINARY_AND):
      processExprBinaryBitAnd(dast, startLabel, endLabel);
      break;
    case (NODETYPE_EXPR_BINARY_OR):
      processExprBinaryBitOr(dast, startLabel, endLabel);
      break;
    case (NODETYPE_EXPR_BINARY_XOR):
      processExprBinaryBitXor(dast, startLabel, endLabel);
      break;
    case (NODETYPE_EXPR_PREFIX_PLUSPLUS):
      processExprPrefixPlusPlus(dast, startLabel, endLabel);
      break;
    case (NODETYPE_EXPR_PREFIX_MINUSMINUS):
      processExprPrefixMinusMinus(dast, startLabel, endLabel);
      break;
    case (NODETYPE_EXPR_PREFIX_NEGATE):
      processExprPrefixNegate(dast, startLabel, endLabel);
      break;
    case (NODETYPE_EXPR_PREFIX_DEREFERENCE):
      processDeref(dast, startLabel, endLabel);
      break;
    case (NODETYPE_EXPR_PREFIX_ADDRESS):
      processAddr(dast, startLabel, endLabel);
      break;
    case (NODETYPE_EXPR_PREFIX_NOTNOT):
      processExprPrefixLogicNot(dast, startLabel, endLabel);
      break;
    case (NODETYPE_EXPR_PREFIX_NOT):
      processExprPrefixBitNot(dast, startLabel, endLabel);
      break;
    case (NODETYPE_EXPR_POSTFIX_PLUSPLUS):
      processExprPostfixPlusPlus(dast, startLabel, endLabel);
      break;
    case (NODETYPE_EXPR_POSTFIX_MINUSMINUS):
      processExprPostfixMinusMinus(dast, startLabel, endLabel);
      break;
    case (NODETYPE_EXPR_STRUCT_DOT):
      processDot(dast, startLabel, endLabel);
      break;
    case (NODETYPE_EXPR_STRUCT_ARROW):
      processArrow(dast, startLabel, endLabel);
      break;
    case (NODETYPE_EXPR_CALL):
      processExprCall(dast, startLabel, endLabel);
      break;
    case (NODETYPE_EXPR_ASSIGN):
      processExprAssign(dast, startLabel, endLabel);
      break;
    case (NODETYPE_ID):
      processID(dast, startLabel, endLabel);
      break;
    case (NODETYPE_BREAK):
      processBreak(dast, startLabel, endLabel);
      break;
    case (NODETYPE_CONTINUE):
      processContinue(dast, startLabel, endLabel);
      break;
    case (NODETYPE_RETURN):
      processReturn(dast, startLabel, endLabel);
      break;
    case (NODETYPE_BLOCK):
      processBlock(dast, startLabel, endLabel);
      break;
    case (NODETYPE_STRUCT_DEF):
      break;
    default:
      processDefault(dast, startLabel, endLabel);
      break;
  }
}

void processLeftID(DAST* dast, char* startLabel, char* endLabel) {
  enum DeclLevel level = dast->decl->level;
  int offset = dast->decl->offset;
  if (level == FUNCTION) {
    emitADDI(S1, FP, offset);
  } else if (level == GLOBAL) {
    emitADDI(S1, GP, offset);
  }
}

void processLeftPren(DAST* dast, char* startLabel, char* endLabel) {
  // This seems like it would work in theory....
  dispatchLeft(dast->children[0], startLabel, endLabel);
}

void processLeftDeref(DAST* dast, char* startLabel, char* endLabel) {
  // get address of variable in memory
  dispatch(dast->children[0], startLabel, endLabel);
}

void processLeftDot(DAST* dast, char* startLabel, char* endLabel) {
  DAST* struct_instance = dast->children[0];
  // get the address of the struct instance
  dispatchLeft(struct_instance, startLabel, endLabel);

  int offset = dast->children[1]->decl->offset;
  // get addr of struct field
  emitADDI(S1, S1, offset);
}

void processLeftArrow(DAST* dast, char* startLabel, char* endLabel) {
  DAST* struct_instance = dast->children[0];
  // get the address of the struct instance
  dispatch(struct_instance, startLabel, endLabel);

  int offset = dast->children[1]->decl->offset;
  // get addr of struct field
  emitADDI(S1, S1, offset);
}

void dispatchLeft(DAST* dast, char* startLabel, char* endLabel) {
  switch (dast->type) {
    case (NODETYPE_EXPR_STRUCT_DOT):
      processLeftDot(dast, startLabel, endLabel);
      break;
    case (NODETYPE_EXPR_STRUCT_ARROW):
      processLeftArrow(dast, startLabel, endLabel);
      break;
    case (NODETYPE_ID):
      processLeftID(dast, startLabel, endLabel);
      break;
    case (NODETYPE_EXPR_PREN):
      processLeftPren(dast, startLabel, endLabel);
      break;
    case (NODETYPE_EXPR_PREFIX_DEREFERENCE):
      processLeftDeref(dast, startLabel, endLabel);
      break;
    default:
      dispatch(dast, startLabel, endLabel);
      break;
  }
}
