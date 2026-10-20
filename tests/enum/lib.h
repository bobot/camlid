#include <inttypes.h>
#include <stddef.h>

enum color { RED = 10, GREEN = 20, BLUE = 30 };
enum color next_color(enum color c);
int bad_color(void);

#define F_READ 1
#define F_WRITE 2
#define F_EXEC 4
int flags_add_exec(int flags);

/* returns NULL if s is NULL or empty */
char *non_empty(char *s);
