(**

    An abstract syntax tree for POSIX shell.

 *)

(* FIXME: Abstract representation of words *)

type name = string
let parse_name (SemanticValues.Name n) = n
let pp_name ppf = Format.fprintf ppf "%s"
                
type word = string
let parse_word (SemanticValues.Word w) = w
let pp_word ppf = Format.fprintf ppf "%s"

let pp_io_number ppf (SemanticValues.IONumber s) =
  Format.fprintf ppf "%s" s
                
type assignment = {
    name : name ;
    contents : word
  }
let parse_assignment (SemanticValues.AssignmentWord (n, w)) =
  { name = parse_name n ;
    contents = parse_word w }
let pp_assignment ppf a =
  Format.fprintf ppf "%a=%a" pp_name a.name pp_word a.contents
  
(* AST *)

type complete_command = seq_async list

 and seq_async =
   | Seq   of cmd_and_or
   | Async of cmd_and_or

 and cmd_and_or =
   | Cmd     of pipeline
   | Cmd_And of pipeline * cmd_and_or
   | Cmd_Or  of pipeline * cmd_and_or

 and pipeline =
   | Pipe     of command list
   | Pipe_Not of command list

 and command =
   | Simple   of simple_command
   | Compound of compound_command * redirection list
   | Function of function_definition

 and compound_command =
   | Brace    of complete_command
   | Subshell of complete_command
   | For      of for_clause
   | Case     of case_clause
   | If       of if_clause
   | While    of while_clause
   | Until    of until_clause

 and for_clause = {
     fc_name : name ;
     fc_list : (word list) option ;
     (* Some [] : empty list
        None    : no list (iterating over script's arguments) *)
     fc_body : complete_command
   }

 and case_clause = {
     cc_word : word ;
     cc_items : case_item list
   }

 and case_item = {
     ci_pattern : word list ;
     ci_body : complete_command option
   }

 and if_clause = {
     ic_test : complete_command ;
     ic_body : complete_command ;
     ic_else : else_part
   }

 and else_part =
   | Elif of if_clause
   | Else of complete_command
   | Empty

 and while_clause = {
     wc_test : complete_command ;
     wc_body : complete_command
   }

 and until_clause = {
     uc_test : complete_command ;
     uc_body : complete_command
   }

 and function_definition = {
     fd_name : name ;
     fd_body : compound_command ;
     fd_redirect : redirection list
   }

 and simple_command = {
     sc_assignments : assignment list ;
     sc_redirections : redirection list ;
     sc_words : word list
   }

 and redirection = {
     r_number : SemanticValues.io_number option ;
     r_content : redirection_content
   }

 and redirection_content =
   | Redirection_File of file_redirection
   | Redirection_Here of io_here

 and file_redirection = {
     fr_kind : file_redirection_kind ;
     fr_filename : word
   }

 and file_redirection_kind =
   | Io_Less
   | Io_LessAnd
   | Io_Great
   | Io_GreatAnd
   | Io_DGreat
   | Io_LessGreat
   | Io_Clobber

 and io_here = {
     ih_kind : io_here_kind ;
     ih_end_word : word
   }

 and io_here_kind =
   | Io_DLess
   | Io_DLessDash

(******************************************************************************)

module FromCST =
  struct
    open CST

    let separator_op_is_async = function
      | SeparatorOp_Uppersand -> true
      | SeparatorOp_Semicolon -> false

    let separator_is_async = function
      | Separator_SeparatorOp_LineBreak (s, _) ->
         separator_op_is_async s
      | Separator_NewLineList _ -> false

    let rec complete_command = function
      | CompleteCommand_CList_Separator (l, s) ->
         clist [] (separator_is_async s) l
      | CompleteCommand_CList l ->
         clist [] false l

    and clist acc async = function
      | CList_CList_SeparatorOp_AndOr (c, s, a) ->
         let a = and_or a in
         let a = if async then Async a else Seq a in
         clist (a :: acc) (separator_op_is_async s) c
      | CList_AndOr a ->
         let a = and_or a in
         let a = if async then Async a else Seq a in
         a :: acc

    and and_or a =
      (* The POSIX standard defines lists from right to left. But we
         would like to work on left-to-right lists, because it's more
         natural. We thus need to reverse them before. *)
      let rec aux = function
        | AndOr_Pipeline p ->
           (fun x -> x), (pipeline p)
        | AndOr_AndOr_AndIf_LineBreak_Pipeline (a, _, p) ->
           let (f, y) = aux a in
           (fun x -> f (Cmd_And (y, x))), (pipeline p)
        | AndOr_AndOr_OrIf_LineBreak_Pipeline (a, _, p) ->
           let (f, y) = aux a in
           (fun x -> f (Cmd_Or (y, x))), (pipeline p)
      in
      let (f, y) = aux a in
      f (Cmd y)
      
    and pipeline = function
      | Pipeline_PipeSequence p ->
         Pipe (pipe_sequence [] p)
      | Pipeline_Bang_PipeSequence p ->
         Pipe_Not (pipe_sequence [] p)

    and pipe_sequence acc = function
      | PipeSequence_Command c ->
         (command c) :: acc
      | PipeSequence_PipeSequence_Pipe_LineBreak_Command (p, _, c) ->
         pipe_sequence ((command c) :: acc) p

    and command = function
      | Command_SimpleCommand s ->
         Simple (simple_command s)
      | Command_CompoundCommand c ->
         Compound (compound_command c, [])
      | Command_CompoundCommand_RedirectList (c, r) ->
         Compound (compound_command c, redirect_list [] r)
      | Command_FunctionDefinition f ->
         Function (function_definition f)

    and compound_command = function
      | CompoundCommand_BraceGroup (BraceGroup_LBrace_CompoundList_RBrace l) ->
         Brace (compound_list l)
      | CompoundCommand_Subshell (Subshell_Lparen_CompoundList_Rparen l) ->
         Subshell (compound_list l)
      | CompoundCommand_ForClause f ->
         For (for_clause f)
      | CompoundCommand_CaseClause c ->
         Case (case_clause c)
      | CompoundCommand_IfClause i ->
         If (if_clause i)
      | CompoundCommand_WhileClause w ->
         While (while_clause w)
      | CompoundCommand_UntilClause u ->
         Until (until_clause u)

    and compound_list = function
      | CompoundList_Term t
      | CompoundList_NewLineList_Term (_, t) ->
         term [] false t
      | CompoundList_Term_Separator (t, s)
      | CompoundList_NewLineList_Term_Separator (_, t, s) ->
         term [] (separator_is_async s) t
        
    and term acc async = function
      | Term_Term_Separator_AndOr (t, s, a) ->
         let a = and_or a in
         let a = if async then Async a else Seq a in
         term (a :: acc) (separator_is_async s) t
      | Term_AndOr a ->
         let a = and_or a in
         let a = if async then Async a else Seq a in
         a :: acc
         
    and for_clause = function
      | ForClause_For_Name_LineBreak_DoGroup (Name n, _, DoGroup_Do_CompoundList_Done l)
      | ForClause_For_Name_LineBreak_In_SequentialSep_DoGroup (Name n, _, _, DoGroup_Do_CompoundList_Done l) ->
         { fc_name = parse_name n ;
           fc_list = None ;
           fc_body = compound_list l }
      | ForClause_For_Name_LineBreak_In_WordList_SequentialSep_DoGroup (Name n, _, w, _, DoGroup_Do_CompoundList_Done l) ->
         { fc_name = parse_name n ;
           fc_list = Some (wordlist [] w) ;
           fc_body = compound_list l }
        
    and wordlist acc = function
      | WordList_WordList_Word (wl, w) ->
         wordlist ((parse_word w) :: acc) wl
      | WordList_Word w ->
         (parse_word w) :: acc

    and case_clause = function
      | CaseClause_Case_Word_LineBreak_In_LineBreak_CaseList_Esac (w, _, _, c) ->
         { cc_word = parse_word w ;
           cc_items = case_list [] c }
      | CaseClause_Case_Word_LineBreak_In_LineBreak_CaseListNS_Esac (w , _, _, c) ->
         { cc_word = parse_word w ;
           cc_items = case_list_ns [] c }
      | CaseClause_Case_Word_LineBreak_In_LineBreak_Esac (w, _, _) ->
         { cc_word = parse_word w ;
           cc_items = [] }

    and case_list_ns acc = function
      | CaseListNS_CaseList_CaseItemNS (c, ci) ->
         case_list ((case_item_ns ci) :: acc) c
      | CaseListNS_CaseItemNS ci ->
         (case_item_ns ci) :: acc

    and case_list acc = function
      | CaseList_CaseList_CaseItem (c, ci) ->
         case_list ((case_item ci) :: acc) c;
      | CaseList_CaseItem ci ->
         (case_item ci) :: acc

    and case_item_ns = function
      | CaseItemNS_Pattern_Rparen_LineBreak (p, _)
      | CaseItemNS_Lparen_Pattern_Rparen_LineBreak (p, _) ->
         { ci_pattern = pattern [] p ;
           ci_body = None }
      | CaseItemNS_Pattern_Rparen_CompoundList_LineBreak (p, c, _)
      | CaseItemNS_Lparen_Pattern_Rparen_CompoundList_LineBreak (p, c, _) ->
         { ci_pattern = pattern [] p ;
           ci_body = Some (compound_list c) }

    and case_item = function
      | CaseItem_Pattern_Rparen_LineBreak_Dsemi_LineBreak (p, _, _)
      | CaseItem_Lparen_Pattern_Rparen_LineBreak_Dsemi_LineBreak (p, _, _) ->
         { ci_pattern = pattern [] p ;
           ci_body = None }
      | CaseItem_Pattern_Rparen_CompoundList_Dsemi_LineBreak (p, c, _)
      | CaseItem_Lparen_Pattern_Rparen_CompoundList_Dsemi_LineBreak (p, c, _) ->
         { ci_pattern = pattern [] p ;
           ci_body = Some (compound_list c) }

    and pattern acc = function
      | Pattern_Word w ->
         (parse_word w) :: acc
      | Pattern_Pattern_Pipe_Word (p, w) ->
         pattern ((parse_word w) :: acc) p

    and if_clause = function
      | IfClause_If_CompoundList_Then_CompoundList_ElsePart_Fi (c1, c2, e) ->
         { ic_test = compound_list c1 ;
           ic_body = compound_list c2 ;
           ic_else = else_part e }
      | IfClause_If_CompoundList_Then_CompoundList_Fi (c1, c2) ->
         { ic_test = compound_list c1 ;
           ic_body = compound_list c2 ;
           ic_else = Empty }

    and else_part = function
      | ElsePart_Elif_CompoundList_Then_CompoundList (c1, c2) ->
         Elif { ic_test = compound_list c1 ;
                ic_body = compound_list c2 ;
                ic_else = Empty }
      | ElsePart_Elif_CompoundList_Then_CompoundList_ElsePart (c1, c2, e) ->
         Elif { ic_test = compound_list c1 ;
                ic_body = compound_list c2 ;
                ic_else = else_part e }
      | ElsePart_Else_CompoundList c ->
         Else (compound_list c)

    and while_clause = function
      | WhileClause_While_CompoundList_DoGroup (c, (DoGroup_Do_CompoundList_Done l)) ->
         { wc_test = compound_list c ;
           wc_body = compound_list l }

    and until_clause = function
      | UntilClause_Until_CompoundList_DoGroup (c, (DoGroup_Do_CompoundList_Done l)) ->
         { uc_test = compound_list c ;
           uc_body = compound_list l }

    and function_definition = function
      | FunctionDefinition_Fname_Lparen_Rparen_LineBreak_FunctionBody ((Fname_Name n), _, b) ->
         let (fd_body, fd_redirect) = function_body b in
         { fd_name = parse_name n ; fd_body ; fd_redirect }

    and function_body = function
      | FunctionBody_CompoundCommand c ->
         (compound_command c, [])
      | FunctionBody_CompoundCommand_RedirectList (c, r) ->
         (compound_command c, redirect_list [] r)

    and simple_command = function
      | SimpleCommand_CmdPrefix_CmdWord_CmdSuffix (cp, (CmdWord_Word w), cs) ->
         let sc_words, sc_redirections = cmd_suffix [] [] cs in
         let sc_assignments, sc_redirections = cmd_prefix [] sc_redirections cp in
         { sc_assignments ;
           sc_redirections ;
           sc_words = (parse_word w) :: sc_words }

      | SimpleCommand_CmdPrefix_CmdWord (cp, (CmdWord_Word w)) ->
         let sc_assignments, sc_redirections = cmd_prefix [] [] cp in
         { sc_assignments ;
           sc_redirections ;
           sc_words = [parse_word w] }
         
      | SimpleCommand_CmdPrefix cp ->
         let sc_assignments, sc_redirections = cmd_prefix [] [] cp in
         { sc_assignments ;
           sc_redirections ;
           sc_words = [] }
        
      | SimpleCommand_CmdName_CmdSuffix ((CmdName_Word w), cs) ->
         let sc_words, sc_redirections = cmd_suffix [] [] cs in
         { sc_assignments = [] ;
           sc_redirections ;
           sc_words = (parse_word w) :: sc_words }

      | SimpleCommand_CmdName (CmdName_Word w) ->
         { sc_assignments = [] ;
           sc_redirections = [] ;
           sc_words = [parse_word w] }

    and cmd_prefix acc_assignments acc_redirections = function
      | CmdPrefix_IoRedirect i ->
         acc_assignments, ((io_redirect i) :: acc_redirections)
      | CmdPrefix_CmdPrefix_IoRedirect (cp, i) ->
         cmd_prefix acc_assignments ((io_redirect i) :: acc_redirections) cp
      | CmdPrefix_AssignmentWord a ->
         ((parse_assignment a) :: acc_assignments), acc_redirections
      | CmdPrefix_CmdPrefix_AssignmentWord (cp, a) ->
         cmd_prefix ((parse_assignment a) :: acc_assignments) acc_redirections cp

    and cmd_suffix acc_words acc_redirections = function
      | CmdSuffix_IoRedirect i ->
         acc_words, ((io_redirect i) :: acc_redirections)
      | CmdSuffix_CmdSuffix_IoRedirect (cs, i) ->
         cmd_suffix acc_words ((io_redirect i) :: acc_redirections) cs
      | CmdSuffix_Word w ->
         ((parse_word w) :: acc_words), acc_redirections
      | CmdSuffix_CmdSuffix_Word (cs, w) ->
         cmd_suffix ((parse_word w) :: acc_words) acc_redirections cs

    and redirect_list acc = function
      | RedirectList_IoRedirect i ->
         (io_redirect i) :: acc
      | RedirectList_RedirectList_IoRedirect (r, i) ->
         redirect_list ((io_redirect i) :: acc) r

    and io_redirect = function
      | IoRedirect_IoFile i ->
         { r_number = None ;
           r_content = Redirection_File (io_file i) }
      | IoRedirect_IoNumber_IoFile (n, i) ->
         { r_number = Some n ;
           r_content = Redirection_File (io_file i) }
      | IoRedirect_IoHere i ->
         { r_number = None ;
           r_content = Redirection_Here (io_here i) }
      | IoRedirect_IoNumber_IoHere (n, i) ->
         { r_number = Some n ;
           r_content = Redirection_Here (io_here i) }

    and io_file = function
      | IoFile_Less_FileName (Filename_Word w) ->
         { fr_kind = Io_Less ; fr_filename = parse_word w }
      | IoFile_LessAnd_FileName (Filename_Word w) ->
         { fr_kind = Io_LessAnd ; fr_filename = parse_word w }
      | IoFile_Great_FileName (Filename_Word w) ->
         { fr_kind = Io_Great ; fr_filename = parse_word w }
      | IoFile_GreatAnd_FileName (Filename_Word w) ->
         { fr_kind = Io_GreatAnd ; fr_filename = parse_word w }
      | IoFile_DGreat_FileName (Filename_Word w) ->
         { fr_kind = Io_DGreat ; fr_filename = parse_word w }
      | IoFile_LessGreat_FileName (Filename_Word w) ->
         { fr_kind = Io_LessGreat ; fr_filename = parse_word w }
      | IoFile_Clobber_FileName (Filename_Word w) ->
         { fr_kind = Io_Clobber ; fr_filename = parse_word w }

    and io_here = function
      | IoHere_DLess_HereEnd (HereEnd_Word w) ->
         { ih_kind = Io_DLess ; ih_end_word = parse_word w }
      | IoHere_DLessDash_HereEnd (HereEnd_Word w) ->
         { ih_kind = Io_DLessDash ; ih_end_word = parse_word w }
  end

type t = complete_command
       
let from_CST = FromCST.complete_command

module PrettyPrinter =
  struct
    open Format

    let rec pp_concat_last s f f' ppf = function
      | [] -> ()
      | [x] -> fprintf ppf "%a" f' x
      | x :: l ->
         fprintf ppf "%a%s%a" f x s (pp_concat_last s f f') l

    let pp_concat s f ppf = pp_concat_last s f f ppf

    let pp_to_string pp arg =
      let b = Buffer.create 16 in
      let ppf = formatter_of_buffer b in
      pp ppf arg;
      pp_print_flush ppf ();
      Buffer.contents b
        
    let rec complete_command ppf cc =
      fprintf ppf "%a" (pp_concat_last "" (seq_async false) (seq_async true)) cc

    and complete_command_onliner cc =
      let s = pp_to_string complete_command cc in
      try
        let _ = String.index s '\n' in
        (false, s)
      with
        Not_found -> (true, s)
      
    and seq_async last ppf sa =
      match sa with
      | Seq cao | Async cao ->
         fprintf ppf "@[<h>%a%s@]"
                 cmd_and_or cao
                 (match sa with Seq _ -> "" | Async _ -> " &");
         if not last then
           fprintf ppf "@\n"

    and cmd_and_or ppf = function
      | Cmd p ->
         fprintf ppf "%a" pipeline p
      | Cmd_And (p, cao) ->
         fprintf ppf "%a && %a" pipeline p cmd_and_or cao
      | Cmd_Or (p, cao) ->
         fprintf ppf "%a || %a" pipeline p cmd_and_or cao

    and pipeline ppf = function
      | Pipe cl ->
         fprintf ppf "%a" (pp_concat " | " command) cl
      | Pipe_Not cl ->
         fprintf ppf "! %a" (pp_concat " | " command) cl

    and command ppf = function
      | Simple sc ->
         fprintf ppf "%a"
                 simple_command sc
      | Compound (cc, rl) ->
         fprintf ppf "%a" compound_command cc;
         if rl <> [] then
           fprintf ppf " %a" (pp_concat " " redirection)  rl
      | Function fd ->
         fprintf ppf "%a"
                 function_definition fd

    and compound_command ppf = function
      | Brace cc ->
         let (ol, s) = complete_command_onliner cc in
         fprintf ppf (if ol then "{ %s }" else "{@\n@[<h 4>    %s@]@\n}") s
      | Subshell cc ->
         let (ol, s) = complete_command_onliner cc in
         fprintf ppf (if ol then "( %s )" else "(@\n@[<h 4>    %s@]@\n)") s
      | For fc ->
         fprintf ppf "%a" for_clause fc
      | Case cc ->
         fprintf ppf "%a" case_clause cc
      | If ic ->
         fprintf ppf "%a" (if_clause true) ic
      | While wc ->
         fprintf ppf "%a" while_clause wc
      | Until uc ->
         fprintf ppf "%a" until_clause uc

    and for_clause ppf fc =
      (* TODO: onliner. but how to know the last separator? *)
      match fc.fc_list with
      | None ->
         fprintf ppf "@{<keyword>for@} %a@\n@{<keyword>do@}@\n@[<h 4>    %a@]@\n@{<keyword>done@}"
                 pp_name fc.fc_name
                 complete_command fc.fc_body
      | Some fc_list ->
         fprintf ppf "@{<keyword>for@} %a @{<keyword>in@} %a@\n@{<keyword>do@}@\n@[<h 4>    %a@]@\n@{<keyword>done@}"
                 pp_name fc.fc_name
                 (pp_concat " " pp_word) fc_list
                 complete_command fc.fc_body

    and case_clause ppf cc =
      fprintf ppf "@{<keyword>case@} %a @{<keyword>in@}@\n@[<h 4>    %a@]@\n@{<keyword>esac@}"
              pp_word cc.cc_word
              (pp_concat_last "" (case_item false) (case_item true)) cc.cc_items

    and case_item last ppf ci =
      match ci.ci_body with
      | None ->
         fprintf ppf "%a) ;;" (pp_concat "|" pp_word) ci.ci_pattern;
         if not last then fprintf ppf "@\n"
      | Some cc ->
         fprintf ppf "%a)@\n@[<h 4>    %a@\n;;"
                 (pp_concat "|" pp_word) ci.ci_pattern
                 complete_command cc;
         fprintf ppf (if last then "@]" else "@]@\n")

    and if_clause first_if ppf ic =
      fprintf ppf "@{<keyword>%s@} @[%a@]@\n@{<keyword>then@}@\n@[<h 4>    %a@]@\n%a"
              (if first_if then "if" else "elif")
              complete_command ic.ic_test
              complete_command ic.ic_body
              else_part ic.ic_else

    and else_part ppf = function
      | Elif ic ->
         fprintf ppf "%a" (if_clause false) ic
      | Else cc ->
         fprintf ppf "@{<keyword>else@}@\n@[<h 4>    %a@]@\n@{<keyword>fi@}"
                 complete_command cc
      | Empty ->
         fprintf ppf "@{<keyword>fi@}"

    and while_clause ppf wc =
      fprintf ppf "@{<keyword>while@} @[%a@]@\n@{<keyword>do@}@\n@[<h 4>    %a@]@\n@{<keyword>done@}"
              complete_command wc.wc_test
              complete_command wc.wc_body

    and until_clause ppf uc =
      fprintf ppf "@{<keyword>until@} @[%a@]@\n@{<keyword>do@}@\n@[<h 4>    %a@]@\n@{<keyword>done@}"
              complete_command uc.uc_test
              complete_command uc.uc_body

    and function_definition ppf fd =
      fprintf ppf "%a () %a %a"
              pp_name fd.fd_name
              compound_command fd.fd_body
              (pp_concat " " redirection) fd.fd_redirect

    and simple_command ppf sc =
      let need_space = ref false in

      if sc.sc_assignments <> [] then
        (                       
          fprintf ppf "%a" (pp_concat " " pp_assignment) sc.sc_assignments;
          need_space := true
        );
      
      if sc.sc_words <> [] then
        (
          fprintf ppf "%s%a"
                  (if !need_space then " " else "")
                  (pp_concat " " pp_word) sc.sc_words;
          need_space := true
        );
      
      if sc.sc_redirections <> [] then
        (
          fprintf ppf
                  "%s%a"
                  (if !need_space then " " else "")
                  (pp_concat " " redirection) sc.sc_redirections
        )

    and redirection ppf r =
      match r.r_number with
      | None ->
         fprintf ppf "%a" redirection_content r.r_content
      | Some ion ->
         fprintf ppf "%a%a"
                 pp_io_number ion
                 redirection_content r.r_content

    and redirection_content ppf = function
      | Redirection_File fr ->
         fprintf ppf "%a" file_redirection fr
      | Redirection_Here _ ->
         eprintf "AST.PrettyPrinter: No implementation for Here redirection@.";
         fprintf ppf "[NI]"

    and file_redirection ppf fr =
      fprintf ppf "%a%a"
              file_redirection_kind fr.fr_kind
              pp_word fr.fr_filename

    and file_redirection_kind ppf frc =
      fprintf ppf "%s"
              (match frc with
               | Io_Less -> "<"
               | Io_LessAnd -> "<&"
               | Io_Great -> ">"
               | Io_GreatAnd -> ">&"
               | Io_DGreat -> ">>"
               | Io_LessGreat -> "<>"
               | Io_Clobber -> "[NI]")
      
    and pattern ppf p =
      eprintf "AST.PrettyPrinter: No implementation for pattern@.";
      fprintf ppf "[NI]"
  end

let pp = PrettyPrinter.complete_command
let to_string = PrettyPrinter.pp_to_string pp
