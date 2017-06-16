(**

   A shell script can define aliases with the following command:

   ``` alias x='foo bar' ```

   Alias substitution are specified in the standard as follows:

*)

(*specification

   After a token has been delimited, but before applying the
   grammatical rules in Shell Grammar, a resulting word that is
   identified to be the command name word of a simple command shall be
   examined to determine whether it is an unquoted, valid alias
   name. However, reserved words in correct grammatical context shall
   not be candidates for alias substitution. A valid alias name (see
   XBD Alias Name) shall be one that has been defined by the alias
   utility and not subsequently undefined using
   unalias. Implementations also may provide predefined valid aliases
   that are in effect when the shell is invoked. To prevent infinite
   loops in recursive aliasing, if the shell is not currently
   processing an alias of the same name, the word shall be replaced by
   the value of the alias; otherwise, it shall not be replaced.

*)

open CST

type t = (string * string) list

let empty = []

let bind_alias x v aliases = (x, v) :: aliases

let unbind_alias x aliases = List.filter (fun (y, _) -> x <> y) aliases

exception NestedAliasingCommand
exception InvalidAliasArguments
exception InvalidUnaliasArguments

type alias_related_command =
  | Alias of string * string
  | Unalias of string

let binder_from_alias = function
  | CmdSuffix_Word { value = Word a } ->
    let s = Str.(split (regexp "=") a) in
    List.hd s, String.concat "" (List.tl s)

  | _ ->
    raise InvalidAliasArguments

let unalias_argument = function
  | CmdSuffix_Word { value = Word x } ->
    x
  | _ ->
    raise InvalidUnaliasArguments

let rec as_aliasing_related_command = function
  | SimpleCommand_CmdName_CmdSuffix ({ value = CmdName_Word w }, suffix) ->
    begin match w.value with
    | Word "alias" ->
      let x, v = binder_from_alias suffix.value in
      Some (Alias (x, v))
    | Word "unalias" ->
      let x = unalias_argument suffix.value in
      Some (Unalias x)
    | _ ->
      None
    end
  | SimpleCommand_CmdName _
  | SimpleCommand_CmdPrefix_CmdWord_CmdSuffix _
  | SimpleCommand_CmdPrefix_CmdWord _
  | SimpleCommand_CmdPrefix _ ->
    None

(** [interpret aliases cst] traverses [cst] to check that there is no
    alias or unalias definitions in an nested command, in which case an
    error is issued. Then, for all alias and unalias toplevel invokation,
    this function updates [aliases]. *)
let interpret aliases cst =
  let aliases = ref aliases in
  let level = ref 0 in
  let analyzer = object (self : 'self)
      inherit [_] CST.iter as super
      method! visit_compound_command env cmd =
        incr level;
        super # visit_compound_command env cmd;
        decr level

      method! visit_simple_command _ cmd =
        match as_aliasing_related_command cmd with
        | Some alias_command ->
          if !level = 0 then match alias_command with
            | Alias (x, value) -> aliases := bind_alias x value !aliases
            | Unalias x -> aliases := unbind_alias x !aliases
          else
            raise NestedAliasingCommand
        | None ->
          ()
    end
  in
  analyzer#visit_complete_command () cst;
  !aliases

let substitute aliases w =
  try
    List.assoc w aliases
  with Not_found ->
    w
