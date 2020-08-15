set_jrh_lexer;;
open Lib;;
open Printer;;
open Theorem_fingerprint;;
open Import_proofs;;
open Tactics;;

Printer.current_encoding := Printer.Sexp;;


(* "|--(ff ((X /. Y) ** Y) --> gg X)" *)

register_proof 366514966443620809 (
  fun () ->
    decode_goal [] "(a (c (fun (scs_v39) (bool)) RC) (a (a (c (fun (stable_sy) (fun (tri_sy) (scs_v39))) <=.) (a (c (fun (loop) (stable_sy)) mark_term) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (v (loop) X)) (v (loop) Y))) (v (loop) Y)))) (a (c (fun (loop) (tri_sy)) TAGB) (v (loop) X))))",
    Parse_tactic.parse "REWRITE_TAC [ THM 205278854658789786 ]") false;;

(* "|--(ff (Y ** (Y \. X)) --> gg X)" *)

register_proof 2272701142723828279 (
  fun () ->
    decode_goal [] "(a (c (fun (scs_v39) (bool)) RC) (a (a (c (fun (stable_sy) (fun (tri_sy) (scs_v39))) <=.) (a (c (fun (loop) (stable_sy)) mark_term) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (v (loop) Y)) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (v (loop) Y)) (v (loop) X))))) (a (c (fun (loop) (tri_sy)) TAGB) (v (loop) X))))",
    Parse_tactic.parse "REWRITE_TAC [ THM 205278854658789786 ]") false;;

(* "|--(ff Y --> gg ( X /. (Y \. X)))" *)

register_proof 525707826793370528 (
  fun () ->
    decode_goal [] "(a (c (fun (scs_v39) (bool)) RC) (a (a (c (fun (stable_sy) (fun (tri_sy) (scs_v39))) <=.) (a (c (fun (loop) (stable_sy)) mark_term) (v (loop) Y))) (a (c (fun (loop) (tri_sy)) TAGB) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (v (loop) X)) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (v (loop) Y)) (v (loop) X))))))",
    Parse_tactic.parse "REWRITE_TAC [ THM 205278854658789786 ]") false;;

(* "|--(ff Y --> gg (((X /. Y)) \. X))" *)

register_proof 3007168780424280166 (
  fun () ->
    decode_goal [] "(a (c (fun (scs_v39) (bool)) RC) (a (a (c (fun (stable_sy) (fun (tri_sy) (scs_v39))) <=.) (a (c (fun (loop) (stable_sy)) mark_term) (v (loop) Y))) (a (c (fun (loop) (tri_sy)) TAGB) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (v (loop) X)) (v (loop) Y))) (v (loop) X)))))",
    Parse_tactic.parse "REWRITE_TAC [ THM 205278854658789786 ]") false;;

(* "|--(ff ( ( A /. (B \. A)) \. A) --> gg (B \. A))" *)

register_proof 1530828594983725155 (
  fun () ->
    decode_goal [] "(a (c (fun (scs_v39) (bool)) RC) (a (a (c (fun (stable_sy) (fun (tri_sy) (scs_v39))) <=.) (a (c (fun (loop) (stable_sy)) mark_term) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (v (loop) A)) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (v (loop) B)) (v (loop) A)))) (v (loop) A)))) (a (c (fun (loop) (tri_sy)) TAGB) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (v (loop) B)) (v (loop) A)))))",
    Parse_tactic.parse "REWRITE_TAC [ THM 205278854658789786 ]") false;;

(* "|--(ff (B \. A) --> gg ( ( A /. (B \. A)) \. A))" *)

register_proof 442425133913280399 (
  fun () ->
    decode_goal [] "(a (c (fun (scs_v39) (bool)) RC) (a (a (c (fun (stable_sy) (fun (tri_sy) (scs_v39))) <=.) (a (c (fun (loop) (stable_sy)) mark_term) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (v (loop) B)) (v (loop) A)))) (a (c (fun (loop) (tri_sy)) TAGB) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (v (loop) A)) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (v (loop) B)) (v (loop) A)))) (v (loop) A)))))",
    Parse_tactic.parse "REWRITE_TAC [ THM 205278854658789786 ]") false;;

(* "|--(ff (((A /. B) ** B) ** (A \. C)) --> gg C)" *)

register_proof 108470594909763685 (
  fun () ->
    decode_goal [] "(a (c (fun (scs_v39) (bool)) RC) (a (a (c (fun (stable_sy) (fun (tri_sy) (scs_v39))) <=.) (a (c (fun (loop) (stable_sy)) mark_term) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (v (loop) A)) (v (loop) B))) (v (loop) B))) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (v (loop) A)) (v (loop) C))))) (a (c (fun (loop) (tri_sy)) TAGB) (v (loop) C))))",
    Parse_tactic.parse "REWRITE_TAC [ THM 205278854658789786 ]") false;;

(* "|--(ff (A ** (((A \. B) /. (A \. B)) ** (A \. B)))--> gg B)" *)

register_proof 439217942077018335 (
  fun () ->
    decode_goal [] "(a (c (fun (scs_v39) (bool)) RC) (a (a (c (fun (stable_sy) (fun (tri_sy) (scs_v39))) <=.) (a (c (fun (loop) (stable_sy)) mark_term) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (v (loop) A)) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (v (loop) A)) (v (loop) B))) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (v (loop) A)) (v (loop) B)))) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (v (loop) A)) (v (loop) B)))))) (a (c (fun (loop) (tri_sy)) TAGB) (v (loop) B))))",
    Parse_tactic.parse "REWRITE_TAC [ THM 205278854658789786 ]") false;;

(* "|--(ff (A ** (((A \. B) /. A) ** ((A /. C) ** C))) --> gg B)" *)

register_proof 3914637889164229952 (
  fun () ->
    decode_goal [] "(a (c (fun (scs_v39) (bool)) RC) (a (a (c (fun (stable_sy) (fun (tri_sy) (scs_v39))) <=.) (a (c (fun (loop) (stable_sy)) mark_term) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (v (loop) A)) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (v (loop) A)) (v (loop) B))) (v (loop) A))) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (v (loop) A)) (v (loop) C))) (v (loop) C)))))) (a (c (fun (loop) (tri_sy)) TAGB) (v (loop) B))))",
    Parse_tactic.parse "REWRITE_TAC [ THM 205278854658789786 ]") false;;

(* "|--(ff ((C /. ((C /. A) \. C)) \. C) --> gg ((C /. A) \. C))" *)

register_proof 3071793164996901096 (
  fun () ->
    decode_goal [] "(a (c (fun (scs_v39) (bool)) RC) (a (a (c (fun (stable_sy) (fun (tri_sy) (scs_v39))) <=.) (a (c (fun (loop) (stable_sy)) mark_term) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (v (loop) C)) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (v (loop) C)) (v (loop) A))) (v (loop) C)))) (v (loop) C)))) (a (c (fun (loop) (tri_sy)) TAGB) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (v (loop) C)) (v (loop) A))) (v (loop) C)))))",
    Parse_tactic.parse "REWRITE_TAC [ THM 205278854658789786 ]") false;;

(* "|--(ff ((C /. A) \. C) --> gg ((C /. ((C /. A) \. C)) \. C))" *)

register_proof 526611983243431222 (
  fun () ->
    decode_goal [] "(a (c (fun (scs_v39) (bool)) RC) (a (a (c (fun (stable_sy) (fun (tri_sy) (scs_v39))) <=.) (a (c (fun (loop) (stable_sy)) mark_term) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (v (loop) C)) (v (loop) A))) (v (loop) C)))) (a (c (fun (loop) (tri_sy)) TAGB) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (v (loop) C)) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (v (loop) C)) (v (loop) A))) (v (loop) C)))) (v (loop) C)))))",
    Parse_tactic.parse "REWRITE_TAC [ THM 205278854658789786 ]") false;;

(* "|--(ff ((A ** B) ** A) --> gg ((( (A ** B) ** C) /. C) ** A))" *)

register_proof 3202230918055497275 (
  fun () ->
    decode_goal [] "(a (c (fun (scs_v39) (bool)) RC) (a (a (c (fun (stable_sy) (fun (tri_sy) (scs_v39))) <=.) (a (c (fun (loop) (stable_sy)) mark_term) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (v (loop) A)) (v (loop) B))) (v (loop) A)))) (a (c (fun (loop) (tri_sy)) TAGB) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (v (loop) A)) (v (loop) B))) (v (loop) C))) (v (loop) C))) (v (loop) A)))))",
    Parse_tactic.parse "REWRITE_TAC [ THM 205278854658789786 ]") false;;

(* "|--(ff ((A ** B) \. ((A ** B) ** C))--> gg C)" *)

register_proof 592760447585349764 (
  fun () ->
    decode_goal [] "(a (c (fun (scs_v39) (bool)) RC) (a (a (c (fun (stable_sy) (fun (tri_sy) (scs_v39))) <=.) (a (c (fun (loop) (stable_sy)) mark_term) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (v (loop) A)) (v (loop) B))) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (v (loop) A)) (v (loop) B))) (v (loop) C))))) (a (c (fun (loop) (tri_sy)) TAGB) (v (loop) C))))",
    Parse_tactic.parse "REWRITE_TAC [ THM 205278854658789786 ]") false;;

(* "|--(ff ((((A ** B)) \. ((A ** B) ** C)) /. C) --> gg (C /. ((C /. B) ** B)))" *)

register_proof 4478005601122269348 (
  fun () ->
    decode_goal [] "(a (c (fun (scs_v39) (bool)) RC) (a (a (c (fun (stable_sy) (fun (tri_sy) (scs_v39))) <=.) (a (c (fun (loop) (stable_sy)) mark_term) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (v (loop) A)) (v (loop) B))) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (v (loop) A)) (v (loop) B))) (v (loop) C)))) (v (loop) C)))) (a (c (fun (loop) (tri_sy)) TAGB) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (v (loop) C)) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (v (loop) C)) (v (loop) B))) (v (loop) B))))))",
    Parse_tactic.parse "REWRITE_TAC [ THM 205278854658789786 ]") false;;

(* "|--(ff ((((A ** B) \. ((A ** B) ** C)) /. (A /. (C\. A)))) --> gg (C /. C))" *)

register_proof 438739952271531602 (
  fun () ->
    decode_goal [] "(a (c (fun (scs_v39) (bool)) RC) (a (a (c (fun (stable_sy) (fun (tri_sy) (scs_v39))) <=.) (a (c (fun (loop) (stable_sy)) mark_term) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (v (loop) A)) (v (loop) B))) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (v (loop) A)) (v (loop) B))) (v (loop) C)))) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (v (loop) A)) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (v (loop) C)) (v (loop) A)))))) (a (c (fun (loop) (tri_sy)) TAGB) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (v (loop) C)) (v (loop) C)))))",
    Parse_tactic.parse "REWRITE_TAC [ THM 205278854658789786 ]") false;;

(* "|--(ff (((A ** B) \. ((A ** B) ** C)) /. ((A /. ((C \. D)\. A)))) --> gg (C /. ((D /. (C \. D)) \. D)))" *)

register_proof 3287950616156282951 (
  fun () ->
    decode_goal [] "(a (c (fun (scs_v39) (bool)) RC) (a (a (c (fun (stable_sy) (fun (tri_sy) (scs_v39))) <=.) (a (c (fun (loop) (stable_sy)) mark_term) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (v (loop) A)) (v (loop) B))) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (v (loop) A)) (v (loop) B))) (v (loop) C)))) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (v (loop) A)) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (v (loop) C)) (v (loop) D))) (v (loop) A)))))) (a (c (fun (loop) (tri_sy)) TAGB) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (v (loop) C)) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (v (loop) D)) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (v (loop) C)) (v (loop) D)))) (v (loop) D))))))",
    Parse_tactic.parse "REWRITE_TAC [ THM 205278854658789786 ]") false;;

(* "|--(ff ((D /. (C \. D)) \. D) --> gg (A /. ((C \. D) \. A)))" *)

register_proof 881666934616435319 (
  fun () ->
    decode_goal [] "(a (c (fun (scs_v39) (bool)) RC) (a (a (c (fun (stable_sy) (fun (tri_sy) (scs_v39))) <=.) (a (c (fun (loop) (stable_sy)) mark_term) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (v (loop) D)) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (v (loop) C)) (v (loop) D)))) (v (loop) D)))) (a (c (fun (loop) (tri_sy)) TAGB) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (v (loop) A)) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (v (loop) C)) (v (loop) D))) (v (loop) A))))))",
    Parse_tactic.parse "REWRITE_TAC [ THM 205278854658789786 ]") false;;

(* "|--(ff (A ** (A /. B))--> gg (((C /. A) \. C) ** (A /. B)))" *)

register_proof 4528745504145700941 (
  fun () ->
    decode_goal [] "(a (c (fun (scs_v39) (bool)) RC) (a (a (c (fun (stable_sy) (fun (tri_sy) (scs_v39))) <=.) (a (c (fun (loop) (stable_sy)) mark_term) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (v (loop) A)) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (v (loop) A)) (v (loop) B))))) (a (c (fun (loop) (tri_sy)) TAGB) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (v (loop) C)) (v (loop) A))) (v (loop) C))) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (v (loop) A)) (v (loop) B))))))",
    Parse_tactic.parse "REWRITE_TAC [ THM 205278854658789786 ]") false;;

(* "|--(ff (D /. C) --> gg (D /. ((( (A /. B)) ** B) ** (((A /. B) ** B) \. C))))" *)

register_proof 4271294765115254552 (
  fun () ->
    decode_goal [] "(a (c (fun (scs_v39) (bool)) RC) (a (a (c (fun (stable_sy) (fun (tri_sy) (scs_v39))) <=.) (a (c (fun (loop) (stable_sy)) mark_term) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (v (loop) D)) (v (loop) C)))) (a (c (fun (loop) (tri_sy)) TAGB) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (v (loop) D)) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (v (loop) A)) (v (loop) B))) (v (loop) B))) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (v (loop) A)) (v (loop) B))) (v (loop) B))) (v (loop) C)))))))",
    Parse_tactic.parse "REWRITE_TAC [ THM 205278854658789786 ]") false;;

(* "|--(ff ((((A /. B) ** B) ** (((A /. B) ** B) \. C))) --> gg (C))" *)

register_proof 2372488582296574527 (
  fun () ->
    decode_goal [] "(a (c (fun (scs_v39) (bool)) RC) (a (a (c (fun (stable_sy) (fun (tri_sy) (scs_v39))) <=.) (a (c (fun (loop) (stable_sy)) mark_term) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (v (loop) A)) (v (loop) B))) (v (loop) B))) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (v (loop) A)) (v (loop) B))) (v (loop) B))) (v (loop) C))))) (a (c (fun (loop) (tri_sy)) TAGB) (v (loop) C))))",
    Parse_tactic.parse "REWRITE_TAC [ THM 205278854658789786 ]") false;;

(* "|--(ff (((A /. B) ** B) ** (A \. (<> C))) --> gg (<> C))" *)

register_proof 3968941748548863540 (
  fun () ->
    decode_goal [] "(a (c (fun (scs_v39) (bool)) RC) (a (a (c (fun (stable_sy) (fun (tri_sy) (scs_v39))) <=.) (a (c (fun (loop) (stable_sy)) mark_term) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (v (loop) A)) (v (loop) B))) (v (loop) B))) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (v (loop) A)) (a (c (fun (loop) (loop)) ssqrt) (v (loop) C)))))) (a (c (fun (loop) (tri_sy)) TAGB) (a (c (fun (loop) (loop)) ssqrt) (v (loop) C)))))",
    Parse_tactic.parse "REWRITE_TAC [ THM 205278854658789786 ]") false;;

(* "|--(ff ((C /. (<> A)) \. C) --> gg ((C /. (<> ((C /. A) \. C))) \. C))" *)

register_proof 3975916967927283498 (
  fun () ->
    decode_goal [] "(a (c (fun (scs_v39) (bool)) RC) (a (a (c (fun (stable_sy) (fun (tri_sy) (scs_v39))) <=.) (a (c (fun (loop) (stable_sy)) mark_term) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (v (loop) C)) (a (c (fun (loop) (loop)) ssqrt) (v (loop) A)))) (v (loop) C)))) (a (c (fun (loop) (tri_sy)) TAGB) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (v (loop) C)) (a (c (fun (loop) (loop)) ssqrt) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (v (loop) C)) (v (loop) A))) (v (loop) C))))) (v (loop) C)))))",
    Parse_tactic.parse "REWRITE_TAC [ THM 205278854658789786 ]") false;;

(* "|--(ff (X ** (Y /. (<> Z))) --> gg ((X ** Y) /. (<> Z)))" *)

register_proof 2884952316294046941 (
  fun () ->
    decode_goal [] "(a (c (fun (scs_v39) (bool)) RC) (a (a (c (fun (stable_sy) (fun (tri_sy) (scs_v39))) <=.) (a (c (fun (loop) (stable_sy)) mark_term) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (v (loop) X)) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (v (loop) Y)) (a (c (fun (loop) (loop)) ssqrt) (v (loop) Z)))))) (a (c (fun (loop) (tri_sy)) TAGB) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (v (loop) X)) (v (loop) Y))) (a (c (fun (loop) (loop)) ssqrt) (v (loop) Z))))))",
    Parse_tactic.parse "REWRITE_TAC [ THM 205278854658789786 ]") false;;

(* "|--(ff (<> (X ** (Y /. (<> Z)))) --> gg (<>((X ** Y) /. (<> Z))))" *)

register_proof 2075606834046402441 (
  fun () ->
    decode_goal [] "(a (c (fun (scs_v39) (bool)) RC) (a (a (c (fun (stable_sy) (fun (tri_sy) (scs_v39))) <=.) (a (c (fun (loop) (stable_sy)) mark_term) (a (c (fun (loop) (loop)) ssqrt) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (v (loop) X)) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (v (loop) Y)) (a (c (fun (loop) (loop)) ssqrt) (v (loop) Z))))))) (a (c (fun (loop) (tri_sy)) TAGB) (a (c (fun (loop) (loop)) ssqrt) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (v (loop) X)) (v (loop) Y))) (a (c (fun (loop) (loop)) ssqrt) (v (loop) Z)))))))",
    Parse_tactic.parse "REWRITE_TAC [ THM 205278854658789786 ]") false;;

(* "|--(ff (X ** ((<> Y) /. (<> Z))) --> gg ((X ** (<> Y)) /. (<> Z)))" *)

register_proof 3534495803481343682 (
  fun () ->
    decode_goal [] "(a (c (fun (scs_v39) (bool)) RC) (a (a (c (fun (stable_sy) (fun (tri_sy) (scs_v39))) <=.) (a (c (fun (loop) (stable_sy)) mark_term) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (v (loop) X)) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (a (c (fun (loop) (loop)) ssqrt) (v (loop) Y))) (a (c (fun (loop) (loop)) ssqrt) (v (loop) Z)))))) (a (c (fun (loop) (tri_sy)) TAGB) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (v (loop) X)) (a (c (fun (loop) (loop)) ssqrt) (v (loop) Y)))) (a (c (fun (loop) (loop)) ssqrt) (v (loop) Z))))))",
    Parse_tactic.parse "REWRITE_TAC [ THM 205278854658789786 ]") false;;

(* "|--(ff (X /. ( Z ** (<> Y))) --> gg ((X /. (<> Y)) /. Z))" *)

register_proof 4053575842836710757 (
  fun () ->
    decode_goal [] "(a (c (fun (scs_v39) (bool)) RC) (a (a (c (fun (stable_sy) (fun (tri_sy) (scs_v39))) <=.) (a (c (fun (loop) (stable_sy)) mark_term) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (v (loop) X)) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (v (loop) Z)) (a (c (fun (loop) (loop)) ssqrt) (v (loop) Y)))))) (a (c (fun (loop) (tri_sy)) TAGB) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (v (loop) X)) (a (c (fun (loop) (loop)) ssqrt) (v (loop) Y)))) (v (loop) Z)))))",
    Parse_tactic.parse "REWRITE_TAC [ THM 205278854658789786 ]") false;;

(* "|--(ff ((Z /. Y) ** (Y/. (<> X))) --> gg (Z /.(<> X)))" *)

register_proof 4115641585456265887 (
  fun () ->
    decode_goal [] "(a (c (fun (scs_v39) (bool)) RC) (a (a (c (fun (stable_sy) (fun (tri_sy) (scs_v39))) <=.) (a (c (fun (loop) (stable_sy)) mark_term) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (v (loop) Z)) (v (loop) Y))) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (v (loop) Y)) (a (c (fun (loop) (loop)) ssqrt) (v (loop) X)))))) (a (c (fun (loop) (tri_sy)) TAGB) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (v (loop) Z)) (a (c (fun (loop) (loop)) ssqrt) (v (loop) X))))))",
    Parse_tactic.parse "REWRITE_TAC [ THM 205278854658789786 ]") false;;

(* "|--(ff (<> (<> (<> (A ** B)))) --> gg (<> (C \. (C ** (<> (<> (A ** B)))))))" *)

register_proof 197391147747099306 (
  fun () ->
    decode_goal [] "(a (c (fun (scs_v39) (bool)) RC) (a (a (c (fun (stable_sy) (fun (tri_sy) (scs_v39))) <=.) (a (c (fun (loop) (stable_sy)) mark_term) (a (c (fun (loop) (loop)) ssqrt) (a (c (fun (loop) (loop)) ssqrt) (a (c (fun (loop) (loop)) ssqrt) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (v (loop) A)) (v (loop) B))))))) (a (c (fun (loop) (tri_sy)) TAGB) (a (c (fun (loop) (loop)) ssqrt) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (v (loop) C)) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (v (loop) C)) (a (c (fun (loop) (loop)) ssqrt) (a (c (fun (loop) (loop)) ssqrt) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (v (loop) A)) (v (loop) B))))))))))",
    Parse_tactic.parse "REWRITE_TAC [ THM 205278854658789786 ]") false;;