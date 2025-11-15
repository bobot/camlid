  #include <inttypes.h>
  typedef struct DdNode DdNode;
  typedef struct DdManager DdManager;
  #define CUDD_UNIQUE_SLOTS	256	/**< Initial size of subtables */
  #define CUDD_CACHE_SLOTS	262144	/**< Default size of the cache */
  extern DdManager * Cudd_Init(unsigned int numVars, unsigned int numVarsZ, unsigned int numSlots, unsigned int cacheSize, size_t maxMemory);
  extern void Cudd_Quit(DdManager *unique);
  extern void Cudd_RecursiveDeref(DdManager *table, DdNode *n);
  extern DdNode * Cudd_ReadOne(DdManager *dd);
  extern DdNode * Cudd_ReadZero(DdManager *dd);
  extern DdNode * Cudd_bddAnd(DdManager *dd, DdNode *f, DdNode *g);
  extern DdNode * Cudd_bddOr(DdManager *dd, DdNode *f, DdNode *g);
  extern DdNode * Cudd_bddNewVar(DdManager *dd);
  #define Cudd_Not(node) ((DdNode *)((uintptr_t)(node) ^ (uintptr_t) 01))
  extern void Cudd_Ref(DdNode *n);
  extern int Cudd_PrintMinterm(DdManager *manager, DdNode *node);
  #define Cudd_IsComplement(node) ((int) ((uintptr_t) (node) & (uintptr_t) 01))
  extern int Cudd_IsConstant(DdNode *node);
  extern DdNode * Cudd_T(DdNode *node);
  extern DdNode * Cudd_E(DdNode *node);
  extern unsigned int Cudd_NodeReadIndex(DdNode *node);
  extern DdNode * Cudd_ReadLogicZero(DdManager *dd);
  extern DdNode * Cudd_bddIthVar(DdManager *dd, int i);
  extern int Cudd_bddLeq(DdManager *dd, DdNode *f, DdNode *g);
  