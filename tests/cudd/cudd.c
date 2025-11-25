#include "cudd.h"
#include "stddef.h"

extern DdManager *Cudd_Init(unsigned int numVars, unsigned int numVarsZ, unsigned int numSlots, unsigned int cacheSize, size_t maxMemory)
{
    return 0;
}

extern void Cudd_Quit(DdManager *unique) {}
extern void Cudd_RecursiveDeref(DdManager *table, DdNode *n) {}
extern DdNode *Cudd_ReadOne(DdManager *dd) { return 0; }
extern DdNode *Cudd_ReadZero(DdManager *dd) { return 0; }
extern DdNode *Cudd_bddAnd(DdManager *dd, DdNode *f, DdNode *g) { return 0; }
extern DdNode *Cudd_bddOr(DdManager *dd, DdNode *f, DdNode *g) { return 0; }
extern DdNode *Cudd_bddNewVar(DdManager *dd) { return 0; }
extern void Cudd_Ref(DdNode *n) {}
extern int Cudd_PrintMinterm(DdManager *manager, DdNode *node) { return 0; }
extern int Cudd_IsConstant(DdNode *node) { return 0; }
extern DdNode *Cudd_T(DdNode *node) { return 0; }
extern DdNode *Cudd_E(DdNode *node) { return 0; }
extern unsigned int Cudd_NodeReadIndex(DdNode *node) { return 0; }
extern DdNode *Cudd_ReadLogicZero(DdManager *dd) { return 0; }
extern DdNode *Cudd_bddIthVar(DdManager *dd, int i) { return 0; }
extern int Cudd_bddLeq(DdManager *dd, DdNode *f, DdNode *g) { return 0; }
