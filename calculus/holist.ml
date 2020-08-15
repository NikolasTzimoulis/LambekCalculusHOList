
(* ------------------------------------------------------------------------- *)
(* Printer for types.                                                        *)
(* ------------------------------------------------------------------------- *)
  
  
type sexp =
  | Sleaf of string
  | Snode of sexp list
  
let rec sexp_type ty = match ty with
    Tyvar str  -> Sleaf str
  | Tyapp (str,tyl) -> Snode (Sleaf str :: map sexp_type tyl)
  
(* ------------------------------------------------------------------------- *)
(* Parseable S-Expression printers                                           *)
(* ------------------------------------------------------------------------- *)    

let rec sexp_term tm =
  if false then Sleaf ("`" ^ string_of_term tm ^ "`") else  (* For debugging purposes *)
  match tm with
    Var (str, ty) -> Snode [Sleaf "v"; sexp_type ty; Sleaf str]
  | Const (str, ty) -> Snode [Sleaf "c"; sexp_type ty; Sleaf str]
  | Comb (t1, t2) -> Snode [Sleaf "a"; sexp_term t1; sexp_term t2]
  | Abs (t1, t2) -> Snode [Sleaf "l"; sexp_term t1; sexp_term t2];;
  
let rec sexp_print fmt s = match s with
    Sleaf s -> pp_print_string fmt s
  | Snode [] -> pp_print_string fmt "()"
  | Snode (s::sl) ->
      pp_print_char fmt '(';
      sexp_print fmt s;
      List.iter (fun s -> pp_print_char fmt ' '; sexp_print fmt s) sl;
      pp_print_char fmt ')';;
	  
let str_of_sexp s = sexp_print Format.str_formatter s; flush_str_formatter();;

let sexp_memoize : ('a -> sexp) -> ('a -> sexp) =
  let next = ref 1 in
  fun raw ->
    let memo = Hashtbl.create 100 in
    fun x ->
      try Sleaf (Hashtbl.find memo x)
      with Not_found ->
        let n = Printf.sprintf "_%d" (!next) in
        next := !next + 1;
        Hashtbl.add memo x n;
        Snode [Sleaf "memo"; Sleaf n; raw x]

let sexp_thm =
  (* don't memoize (set condition to true) when generating training data *)
  if true then (fun th ->
    let tls, tm = dest_thm th in
    Snode [Sleaf "h"; Snode (map sexp_term tls); sexp_term tm]) else
  sexp_memoize (fun th ->
      let tls, tm = dest_thm th in
      Snode [Sleaf "h"; Snode (map sexp_term tls); sexp_term tm]);;
      
let print_sexp_pb_field
    (fmt: Format.formatter) (field: string) (sexp: sexp) : unit =
  pp_print_string fmt (" " ^ field ^ ": \"");
  pp_print_string fmt (str_of_sexp sexp);
  pp_print_string fmt "\"";;


	  
let toSexpTerm tm = str_of_sexp (sexp_term tm);;
let toSexp th = str_of_sexp (sexp_thm th);;

let print_int_pb (fmt: Format.formatter) (field_name: string) i : unit =
  pp_print_string fmt (Printf.sprintf (" %s: %d ") field_name i)
  
let pb_print_library_tags fmt : unit =
  List.iter (fun library ->
    pp_print_string fmt (" library_tag: \"" ^ library ^ "\"")) [];;
  
 
let print_goal_pb (fmt: Format.formatter)
    ((assumptions, conclusion): term list * term) (tag: string)
    (definition_printer : Format.formatter -> unit) : unit =
  print_int_pb fmt "fingerprint" (999999999);
  List.iter
      (fun asm ->
        print_sexp_pb_field fmt "hypotheses" (sexp_term asm))
      assumptions;
  print_sexp_pb_field fmt " conclusion" (sexp_term conclusion);
  pp_print_string fmt (" tag: " ^ tag);
  match tag with
    "DEFINITION" -> (
      pp_print_string fmt " definition {";
      definition_printer fmt;
      pp_print_string fmt "}")
  | "TYPE_DEFINITION" -> (
      pp_print_string fmt " type_definition {";
      definition_printer fmt;
      pp_print_string fmt "}")
  | _ -> ();;
  
let print_thm_pb (fmt: Format.formatter)
    (th:  thm) (tag: string)
    (definition_printer : Format.formatter -> unit) : unit =
  (if tag == "GOAL" then failwith "Trying to print GOAL with print_thm_pb");
  print_goal_pb fmt (dest_thm th) tag definition_printer;;
  
(* ---------------------------------------------------------------------------*)
(* Print functions for theorem database.                                      *)
(*                                                                            *)
(* ---------------------------------------------------------------------------*)

let print_definition
    (definition_type: string) (term: term option)
    (recursion_thm: thm option) (constants: string list)
    : Format.formatter -> unit =
  fun fmt ->
      pp_print_string fmt (" definition_type: \"" ^ definition_type ^ "\"");
      (match term with
        None -> ()
      | Some term -> print_sexp_pb_field fmt " definition_term" (sexp_term term));
      List.iter
        (fun c -> pp_print_string fmt (" constants: \"" ^ c ^ "\"")) constants;
      (match recursion_thm with
        None -> ()
      | Some recursion_thm -> print_int_pb fmt "theorem_arg"
          (999999999));;

let thm_db_print_definition (log: bool) (definition_type: string) (th: thm)
    (term: term) (recursion_thm: thm option) (constants: string list)
    : unit =
    let fmt = Format.str_formatter in
    pp_print_string fmt "theorems {";
    print_thm_pb fmt th "DEFINITION"
      (print_definition
          definition_type (Some term) recursion_thm (constants));
    pp_print_string fmt (" pretty_printed: \"" ^ string_of_thm th ^ "\"");
    pb_print_library_tags fmt;
    pp_print_string fmt "}\n";
    Format.pp_print_flush fmt ();;

 
let thm_db_print_theorem (th: thm)
    (source: string) (goal_fingerprint : int option) : unit =
  let fmt = Format.str_formatter in
  pp_print_string fmt "theorems {";
  print_thm_pb fmt th "THEOREM" (fun _ -> ());
  pp_print_string fmt (" pretty_printed: \"" ^ string_of_thm th ^ "\"");
  pp_print_string fmt (" proof_function: \"" ^ source ^ "\"");
  (match goal_fingerprint with
    Some goal_fingerprint ->
      print_int_pb fmt "goal_fingerprint" goal_fingerprint;
  | None -> ());
  pp_print_string fmt "}\n";
  Format.pp_print_flush fmt ();;

let print_type_definition (tyname: string) (absname: string) (repname: string)
    (th_arg: thm) : Format.formatter -> unit =
  fun fmt ->
      pp_print_string fmt (" type_name: \"" ^ tyname ^ "\"");
      pp_print_string fmt (" abs_name: \"" ^ absname ^ "\"");
      pp_print_string fmt (" rep_name: \"" ^ repname ^ "\"");
      print_int_pb fmt "theorem_arg" (999999999);;

let thm_db_print_type_definition (tyname: string)
    (absname: string) (repname: string) (th_arg: thm) (th_result: thm) : unit =
  thm_db_print_theorem th_arg "type_definition_helper" None;
  let fmt = Format.str_formatter in
  pp_print_string fmt "theorems {";
  pp_print_string fmt (" pretty_printed: \"" ^ string_of_thm th_result ^ "\"");
  print_thm_pb fmt th_result "TYPE_DEFINITION"
      (print_type_definition tyname absname repname th_arg);
  pb_print_library_tags fmt;
  pp_print_string fmt "}\n";
  Format.pp_print_flush fmt ();;
  