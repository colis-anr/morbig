#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include <morbig.h>

void dump (cst_t cst);
void dump_location (cst_t cst);
void dump_node (cst_t cst);
void dump_data (cst_t cst);

int shift = 0;
void indent () {
  char* s = (char*)malloc(shift + 1);
  unsigned int i = 0;
  for (i = 0; i < shift; i++) s[i] = ' ';
  s[shift] = 0;
  printf ("%s", s);
}

void more_indent () {
  shift += 2;
}

void less_indent () {
  shift -= 2;
}

void dump_position (position_t p) {
  indent ();
  printf ("%s %d %d %d\n",
	  get_filename_of_position (p),
	  get_lnum_of_position (p),
	  get_bol_of_position (p),
	  get_cnum_of_position (p));
}

void dump_location (cst_t cst) {
  indent ();
  printf ("LOCATION\n");
  more_indent ();
  indent ();
  printf ("START\n");
  more_indent ();
  dump_position (get_start_position (cst));
  less_indent ();
  indent ();
  printf ("END\n");
  more_indent ();
  dump_position (get_start_position (cst));
  less_indent ();
  dump (get_located_value (cst));
}

void dump_node (cst_t cst) {
  unsigned int i = 0;
  indent ();
  printf ("NODE\n");
  more_indent ();
  indent ();
  printf ("CONSTRUCTOR %s\n", get_rule_name (cst));
  indent ();
  printf ("CHILDREN\n");
  for (i = 0; i < get_number_of_children (cst); i++) {
    dump (get_children (cst, i));
  }
  less_indent ();
}

void dump_data (cst_t cst) {
  indent ();
  printf ("DATA\n");
  more_indent ();
  indent ();
  printf ("%s\n", get_data (cst));
  less_indent ();
}

void dump (cst_t cst) {
  switch (get_kind (cst)) {
    case LOCATION: dump_location (cst); break;
    case NODE: dump_node (cst); break;
    case DATA: dump_data (cst); break;
  }
}

int main (int argc, char** argv) {
  if (argc <= 1) {
    fprintf (stderr, "usage: dump filename.sh\n");
    exit (EXIT_FAILURE);
  }
  initialize_morbig (argv);
  cst_t cst = parse_file (argv[1]);
  dump (cst);
  dispose_cst (cst);
  exit (EXIT_SUCCESS);
}
