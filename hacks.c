#include <ncurses.h>
#include <stdio.h>
void getStdscr (void * * get)
  {
    * get = stdscr;
  }
void formatExp (double * exp, char * * buf)
  {
    (* buf) [0] = sprintf ((* buf) + 1, "%g", * exp);
  }
