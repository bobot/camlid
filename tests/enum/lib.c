#include "lib.h"

enum color next_color(enum color c) {
  switch (c) {
  case RED: return GREEN;
  case GREEN: return BLUE;
  default: return RED;
  }
}
int bad_color(void) { return 42; }
int flags_add_exec(int flags) { return flags | F_EXEC; }
char *non_empty(char *s) { return (s == NULL || s[0] == 0) ? NULL : s; }
