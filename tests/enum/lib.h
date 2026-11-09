#include <inttypes.h>
#include <stddef.h>

enum color { RED = 10, GREEN = 20, BLUE = 30 };
enum color next_color(enum color c);
enum color bad_color(void); /* a value outside the enum */

#define F_READ 1
#define F_WRITE 2
#define F_EXEC 4
int flags_add_exec(int flags);

/* returns NULL if s is NULL or empty */
char *non_empty(char *s);
