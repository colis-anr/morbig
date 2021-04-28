/*

  Copyright (C) 2017-2021 Yann Régis-Gianas, Nicolas Jeannerod,
  Ralf Treinen.

  This is free software: you can redistribute it and/or modify it
  under the terms of the GNU General Public License, version 3.

  Additional terms apply, due to the reproduction of portions of
  the POSIX standard. Please refer to the file COPYING for details.

*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/alloc.h>

typedef value cst_t;
typedef value position_t;

static char* error_msg = NULL;

cst_t parse_file (char* filename)
{
  static value* closure = NULL;
  value result;
  if (closure == NULL)
    closure = caml_named_value ("untyped_parse_file");
  result = caml_callback_exn (*closure, caml_copy_string (filename));
  if (Is_exception_result (result)) {
    result = Extract_exception (result);
    error_msg = String_val (Field (result, 1));
    return (cst_t)NULL;
  }
  error_msg = NULL;
  return (cst_t)result;
}

char* get_error_message () {
  return error_msg;
}

void dispose_cst (value cst)
{
  static value* closure = NULL;
  if (closure == NULL)
    closure = caml_named_value ("dispose_cst");
  caml_callback (*closure, cst);
}

typedef enum kind {
  LOCATION,
  NODE,
  DATA
} kind_t;

kind_t get_kind (value cst) {
  switch (Tag_val (cst)) {
  case 0: return LOCATION;
  case 1: return NODE;
  case 2: return DATA;
  default:
    fprintf (stderr, "Invalid tag: %d\n", Tag_hd(cst));
    exit (EXIT_FAILURE);
  }
}

void must_be_location (value cst) {
  if (get_kind (cst) != LOCATION) {
    fprintf (stderr, "Not a location at %p.\n", (void*)cst);
    exit (EXIT_FAILURE);
  }
}

position_t get_start_position (value cst) {
  must_be_location (cst);
  return Field (cst, 0);
}

position_t get_end_position (value cst) {
  must_be_location (cst);
  return Field (cst, 1);
}

cst_t get_located_value (value cst) {
  must_be_location (cst);
  return Field (cst, 2);
}

char* get_filename_of_position (position_t p) {
  return String_val (Field (p, 0));
}

int get_lnum_of_position (position_t p) {
  return Int_val (Field (p, 1));
}

int get_bol_of_position (position_t p) {
  return Int_val (Field (p, 2));
}

int get_cnum_of_position (position_t p) {
  return Int_val (Field (p, 3));
}

void must_be_node (value cst) {
  if (get_kind (cst) != NODE) {
    fprintf (stderr, "Not a node at %p.\n", (void*)cst);
    exit (EXIT_FAILURE);
  }
}

char* get_rule_name (value cst) {
  must_be_node (cst);
  return String_val (Field (cst, 0));
}

int get_number_of_children (value cst) {
  must_be_node (cst);
  return Wosize_val (Field (cst, 1));
}

cst_t get_children (value cst, int k) {
  must_be_node (cst);
  return Field (Field (cst, 1), k);
}

void must_be_data (value cst) {
  if (get_kind (cst) != DATA) {
    fprintf (stderr, "Not a data at %p.\n", (void*)cst);
    exit (EXIT_FAILURE);
  }
}

char* get_data (value cst) {
  must_be_data (cst);
  return String_val (Field (cst, 0));
}

void dummy_external () {
  return; // See CAPI.ml
}

void initialize_morbig (char** argv) {
  caml_startup (argv);
}
