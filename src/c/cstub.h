/*

  Copyright (C) 2017-2021 Yann Régis-Gianas, Nicolas Jeannerod,
  Ralf Treinen.

  This is free software: you can redistribute it and/or modify it
  under the terms of the GNU General Public License, version 3.

  Additional terms apply, due to the reproduction of portions of
  the POSIX standard. Please refer to the file COPYING for details.

*/

#include <caml/mlvalues.h>

/*
 * Initialize morbig. This function must be called before any other
 * calls to other functions of this API.
 */
void initialize_morbig (char** argv);

/*
 * The type for concrete syntax trees.
 */
typedef value cst_t;

/*
 * Parse `filename` and return the corresponding syntax tree if
 * `filename` contains a syntactically valid shell script. If
 * an error is detected, `NULL` is returned and `get_error ()`
 * returns a human readable error message.
 */
cst_t parse_file (char* filename);

/*
 * If a call to `parse_file (filename)` produced an error,
 * `get_error_message` returns a human readable error message
 *
 */
char* get_error_message ();

/*
 * Let OCaml GC reclaim the concrete syntax tree.
 */
void  dispose_cst (cst_t cst);

/*
 * A concrete syntax tree is a tree with three kind of
 * nodes:
 *
 * - LOCATION nodes annotate a tree with two positions that characterize
 *   the portion of the source file that produced this tree.
 *
 * - NODE nodes correspond to the application of a POSIX grammar rule.
 *   This application is specified by a rule name and a possibly empty
 *   list of producers (i.e. concrete syntax subtrees).
 *
 * - DATA nodes correspond to literals.
 *
 */
typedef enum kind {
  LOCATION,
  NODE,
  DATA
} kind_t;

/* Return the kind of the node of the cst root. */
kind_t get_kind (cst_t cst);

/* The type for position in a source file. */
typedef value position_t;

/* Return the position filename. */
char* get_filename_of_position (position_t p);

/* Return the line number of the position. */
int get_lnum_of_position (position_t p);

/* Return the column number of the position. */
int get_bol_of_position (position_t p);

/* Return the offset of the position. */
int get_cnum_of_position (position_t p);

/* Return the starting position of the concrete syntax tree.
 * Precondition: `get_kind(cst) == LOCATION`.
 */
position_t get_start_position (value cst);

/* Return the ending position of the concrete syntax tree.
 * Precondition: `get_kind(cst) == LOCATION`.
 */
position_t get_end_position (value cst);

/* Return the located concrete syntax tree.
 * Precondition: `get_kind(cst) == LOCATION`.
 */
cst_t get_located_value (value cst);

/* Return the rule name.
 * Precondition: `get_kind(cst) == NODE`.
 */
char* get_rule_name (value cst);

/* Return the number of concrete subtrees.
 * Precondition: `get_kind(cst) == NODE`.
 */
int get_number_of_children (value cst);

/* Return the number of concrete subtrees.
 * Precondition:
 * `get_kind(cst) == NODE && k < get_number_of_children(cst)`.
 */
cst_t get_children (value cst, int k);

/* Return the literal string representation.
 * Precondition: `get_kind(cst) == DATA`
 */
char* get_data (value cst);


