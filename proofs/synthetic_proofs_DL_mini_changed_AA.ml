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
    Parse_tactic.parse "SIMP_TAC [ THM 1693479518508578392 ; THM 324105359611762671 ; THM 2898052989604839426 ; THM 4245526831407530504 ]") false;;

(* "|--(ff Y --> gg ( X /. (Y \. X)))" *)

register_proof 525707826793370528 (
  fun () ->
    decode_goal [] "(a (c (fun (scs_v39) (bool)) RC) (a (a (c (fun (stable_sy) (fun (tri_sy) (scs_v39))) <=.) (a (c (fun (loop) (stable_sy)) mark_term) (v (loop) Y))) (a (c (fun (loop) (tri_sy)) TAGB) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (v (loop) X)) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (v (loop) Y)) (v (loop) X))))))",
    Parse_tactic.parse "MP_TAC THM 1693479518508578392" THEN
    Parse_tactic.parse "ASM_MESON_TAC [ THM 51220887018286029 ]") false;;

(* "|--(ff (B \. A) --> gg ( ( A /. (B \. A)) \. A))" *)

register_proof 442425133913280399 (
  fun () ->
    decode_goal [] "(a (c (fun (scs_v39) (bool)) RC) (a (a (c (fun (stable_sy) (fun (tri_sy) (scs_v39))) <=.) (a (c (fun (loop) (stable_sy)) mark_term) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (v (loop) B)) (v (loop) A)))) (a (c (fun (loop) (tri_sy)) TAGB) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (v (loop) A)) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (v (loop) B)) (v (loop) A)))) (v (loop) A)))))",
    Parse_tactic.parse "MP_TAC THM 1679452577520289523" THEN
    Parse_tactic.parse "REWRITE_TAC [ THM 3007168780424280166 ]") false;;

(* "|--(ff ((C /. ((C /. A) \. C)) \. C) --> gg ((C /. A) \. C))" *)

register_proof 3071793164996901096 (
  fun () ->
    decode_goal [] "(a (c (fun (scs_v39) (bool)) RC) (a (a (c (fun (stable_sy) (fun (tri_sy) (scs_v39))) <=.) (a (c (fun (loop) (stable_sy)) mark_term) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (v (loop) C)) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (v (loop) C)) (v (loop) A))) (v (loop) C)))) (v (loop) C)))) (a (c (fun (loop) (tri_sy)) TAGB) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (v (loop) C)) (v (loop) A))) (v (loop) C)))))",
    Parse_tactic.parse "REWRITE_TAC [ THM 1530828594983725155 ]") false;;

(* "|--(ff ((C /. A) \. C) --> gg ((C /. ((C /. A) \. C)) \. C))" *)

register_proof 526611983243431222 (
  fun () ->
    decode_goal [] "(a (c (fun (scs_v39) (bool)) RC) (a (a (c (fun (stable_sy) (fun (tri_sy) (scs_v39))) <=.) (a (c (fun (loop) (stable_sy)) mark_term) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (v (loop) C)) (v (loop) A))) (v (loop) C)))) (a (c (fun (loop) (tri_sy)) TAGB) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (v (loop) C)) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (v (loop) C)) (v (loop) A))) (v (loop) C)))) (v (loop) C)))))",
    Parse_tactic.parse "REWRITE_TAC [ THM 442425133913280399 ]") false;;

(* "|--(ff C -->  gg ((A ** B) \. ((A ** B) ** C)))" *)

register_proof 2240419476433202733 (
  fun () ->
    decode_goal [] "(a (c (fun (scs_v39) (bool)) RC) (a (a (c (fun (stable_sy) (fun (tri_sy) (scs_v39))) <=.) (a (c (fun (loop) (stable_sy)) mark_term) (v (loop) C))) (a (c (fun (loop) (tri_sy)) TAGB) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (v (loop) A)) (v (loop) B))) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (v (loop) A)) (v (loop) B))) (v (loop) C))))))",
    Parse_tactic.parse "MP_TAC THM 1679452577520289523" THEN
    Parse_tactic.parse "ASM_MESON_TAC [ THM 1236764197481027867 ; THM 1693479518508578392 ; THM 1036463095527292498 ; THM 2313659865416298332 ]") false;;

(* "|--(ff ((D /. (C \. D)) \. D) --> gg (A /. ((C \. D) \. A)))" *)

register_proof 881666934616435319 (
  fun () ->
    decode_goal [] "(a (c (fun (scs_v39) (bool)) RC) (a (a (c (fun (stable_sy) (fun (tri_sy) (scs_v39))) <=.) (a (c (fun (loop) (stable_sy)) mark_term) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (v (loop) D)) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (v (loop) C)) (v (loop) D)))) (v (loop) D)))) (a (c (fun (loop) (tri_sy)) TAGB) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (v (loop) A)) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (v (loop) C)) (v (loop) D))) (v (loop) A))))))",
    Parse_tactic.parse "REWRITE_TAC [ THM 3312368905117307733 ]" THEN
    Parse_tactic.parse "MP_TAC THM 1693479518508578392" THEN
    Parse_tactic.parse "ASM_MESON_TAC [ THM 1530828594983725155 ; THM 3312368905117307733 ; THM 51220887018286029 ]") false;;

(* "|--(ff ((((A /. B) ** B) ** (((A /. B) ** B) \. C))) --> gg (C))" *)

register_proof 2372488582296574527 (
  fun () ->
    decode_goal [] "(a (c (fun (scs_v39) (bool)) RC) (a (a (c (fun (stable_sy) (fun (tri_sy) (scs_v39))) <=.) (a (c (fun (loop) (stable_sy)) mark_term) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (v (loop) A)) (v (loop) B))) (v (loop) B))) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (v (loop) A)) (v (loop) B))) (v (loop) B))) (v (loop) C))))) (a (c (fun (loop) (tri_sy)) TAGB) (v (loop) C))))",
    Parse_tactic.parse "REWRITE_TAC [ THM 2272701142723828279 ]") false;;

(* "|--(ff (((A /. B) ** B) ** (A \. (<> C))) --> gg (<> C))" *)

register_proof 3968941748548863540 (
  fun () ->
    decode_goal [] "(a (c (fun (scs_v39) (bool)) RC) (a (a (c (fun (stable_sy) (fun (tri_sy) (scs_v39))) <=.) (a (c (fun (loop) (stable_sy)) mark_term) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (v (loop) A)) (v (loop) B))) (v (loop) B))) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (v (loop) A)) (a (c (fun (loop) (loop)) ssqrt) (v (loop) C)))))) (a (c (fun (loop) (tri_sy)) TAGB) (a (c (fun (loop) (loop)) ssqrt) (v (loop) C)))))",
    Parse_tactic.parse "REWRITE_TAC [ THM 108470594909763685 ]") false;;

(* "|--(ff (X ** ((<> Y) /. (<> Z))) --> gg ((X ** (<> Y)) /. (<> Z)))" *)

register_proof 3534495803481343682 (
  fun () ->
    decode_goal [] "(a (c (fun (scs_v39) (bool)) RC) (a (a (c (fun (stable_sy) (fun (tri_sy) (scs_v39))) <=.) (a (c (fun (loop) (stable_sy)) mark_term) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (v (loop) X)) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (a (c (fun (loop) (loop)) ssqrt) (v (loop) Y))) (a (c (fun (loop) (loop)) ssqrt) (v (loop) Z)))))) (a (c (fun (loop) (tri_sy)) TAGB) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (v (loop) X)) (a (c (fun (loop) (loop)) ssqrt) (v (loop) Y)))) (a (c (fun (loop) (loop)) ssqrt) (v (loop) Z))))))",
    Parse_tactic.parse "REWRITE_TAC [ THM 2884952316294046941 ]") false;;