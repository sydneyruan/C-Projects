#include <stdlib.h>
#include <string.h>
#include "ast.h"
#include "utils.h"

/*
   Creates a new AST from a given type filename and linenum. You should not
   assume that filename will remain a legal pointer after this function
   terminates.
*/
AST* MakeAST(enum NodeType type, char* filename, int linenum) {
  /* YOUR CODE HERE. */
  AST* new_AST = (AST*)calloc(1, sizeof(AST));
  if (new_AST == NULL) {
    allocation_failed();
  }
  new_AST->filename = (char*)malloc(strlen(filename) + 1);
  if (new_AST->filename == NULL) {
    allocation_failed();
  }
  new_AST->filename[0] ='\0';
  strcpy(new_AST->filename, filename);

  new_AST->children = (AST**) malloc(sizeof(AST*));
  new_AST->size = 0;
  new_AST->type = type;
  new_AST->capacity = INITIAL_CAPACITY;
  new_AST->linenum = linenum;
  
  return new_AST;
}

/*
   Takes in a given AST and mallocs a new AST with the exact same details as the
   original. Again you should assume the original may be freed at any point
   after this function terminates.
*/
AST* CopyAST(AST* original) {
  AST* ast = MakeAST(original->type, original->filename, original->linenum);
  ast->size = original->size;
  ast->capacity = original->capacity;
  ast->children = (AST**)realloc(ast->children, sizeof(AST*) * ast->capacity);
  if (ast->children == NULL) {
    allocation_failed();
  }
  for (int i = 0; i < ast->size; i++) {
    ast->children[i] = CopyAST(original->children[i]);
  }

  /* Start Unique to Copy */
  if (ast->type == NODETYPE_ID) {
    ast->data.identifier =
        (char*)malloc(sizeof(char) * (strlen(original->data.identifier) + 1));
    if (ast->data.identifier == NULL) {
      allocation_failed();
    }
    strcpy(ast->data.identifier, original->data.identifier);
  } else if (ast->type == NODETYPE_CONSTANT_STRING) {
    ast->data.string =
        (char*)malloc(sizeof(char) * (strlen(original->data.string) + 1));
    if (ast->data.string == NULL) {
      allocation_failed();
    }
    strcpy(ast->data.string, original->data.string);
  }
  /* End of Unique to Copy */
  return ast;
}

/*
   Takes in an two ASTs and concatenates the two by making node a child
   of tree.
*/
void AppendAST(AST* tree, AST* node) {
  /* YOUR CODE HERE */
  if (tree == NULL || node == NULL)
    return;
  tree->capacity += node->capacity;
  tree->children = (AST**)realloc(tree->children, sizeof(AST*) * (tree->size + 1));
  if (tree->children == NULL)
    allocation_failed();
  tree->children[tree->size] = node;
  tree->size++;
}

/*
   Frees the memory allocated by a single AST node.
*/
void FreeNode(AST* ast) {
  /* YOUR CODE HERE */
  if (ast != NULL) {
    if (ast->filename != NULL) {
      free(ast->filename);
    }
    if (ast->type == NODETYPE_ID)
      free(ast->data.identifier);
    else if (ast->type == NODETYPE_CONSTANT_STRING)
      free(ast->data.string);
    if (ast->children != NULL) {
      free(ast->children);
    }
    free(ast);
  }
}

/*
   Frees all the memory allocated by an AST.
*/
void FreeAST(AST* ast) {
  /* YOUR CODE HERE */
  if (ast->size != 0) {
    for (int i = 0; i < ast->size; i++) 
      FreeAST(ast->children[i]);
  }
  FreeNode(ast);
}
