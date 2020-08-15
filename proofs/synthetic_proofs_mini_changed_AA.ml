set_jrh_lexer;;
open Lib;;
open Printer;;
open Theorem_fingerprint;;
open Import_proofs;;
open Tactics;;

Printer.current_encoding := Printer.Sexp;;


(* "|--(ff (pp ((nn (X /. Y)) ** Y)) --> gg X)" *)

register_proof 3594022267544265710 (
  fun () ->
    decode_goal [] "(a (c (fun (scs_v39) (bool)) RC) (a (a (c (fun (stable_sy) (fun (tri_sy) (scs_v39))) <=.) (a (c (fun (loop) (stable_sy)) mark_term) (a (c (fun (cc_v11) (loop)) plane) (a (a (c (fun (loop) (fun (loop) (cc_v11))) *.) (a (c (fun (finite_product) (loop)) plane_norm) (a (a (c (fun (loop) (fun (loop) (finite_product))) **.) (v (loop) X)) (v (loop) Y)))) (v (loop) Y))))) (a (c (fun (loop) (tri_sy)) TAGB) (v (loop) X))))",
    Parse_tactic.parse "MP_TAC THM 1890961052333620652" THEN
    Parse_tactic.parse "ONCE_REWRITE_TAC [ THM 4072279578834798085 ]" THEN
    Parse_tactic.parse "ASM_MESON_TAC [ THM 631359747053030193 ; THM 4072279578834798085 ; THM 2751491796312766639 ; THM 282046444875537080 ; THM 324105359611762671 ; THM 1293350574629016751 ]") false;;

(* "|--(ff (pp (Y ** (nn (Y \. X)))) --> gg X)" *)

register_proof 212959095654130630 (
  fun () ->
    decode_goal [] "(a (c (fun (scs_v39) (bool)) RC) (a (a (c (fun (stable_sy) (fun (tri_sy) (scs_v39))) <=.) (a (c (fun (loop) (stable_sy)) mark_term) (a (c (fun (cc_v11) (loop)) plane) (a (a (c (fun (loop) (fun (loop) (cc_v11))) *.) (v (loop) Y)) (a (c (fun (finite_product) (loop)) plane_norm) (a (a (c (fun (loop) (fun (loop) (finite_product))) *|) (v (loop) Y)) (v (loop) X))))))) (a (c (fun (loop) (tri_sy)) TAGB) (v (loop) X))))",
    Parse_tactic.parse "MP_TAC THM 1890961052333620652" THEN
    Parse_tactic.parse "ASM_MESON_TAC [ THM 631359747053030193 ; THM 4072279578834798085 ; THM 4275337362867714136 ]") false;;

(* "|--(ff (nn (B \. A)) --> gg (nn ( (nn ( A /. ( nn (B \. A)))) \. A)))" *)

register_proof 1416075696371770642 (
  fun () ->
    decode_goal [] "(a (c (fun (scs_v39) (bool)) RC) (a (a (c (fun (stable_sy) (fun (tri_sy) (scs_v39))) <=.) (a (c (fun (loop) (stable_sy)) mark_term) (a (c (fun (finite_product) (loop)) plane_norm) (a (a (c (fun (loop) (fun (loop) (finite_product))) *|) (v (loop) B)) (v (loop) A))))) (a (c (fun (loop) (tri_sy)) TAGB) (a (c (fun (finite_product) (loop)) plane_norm) (a (a (c (fun (loop) (fun (loop) (finite_product))) *|) (a (c (fun (finite_product) (loop)) plane_norm) (a (a (c (fun (loop) (fun (loop) (finite_product))) **.) (v (loop) A)) (a (c (fun (finite_product) (loop)) plane_norm) (a (a (c (fun (loop) (fun (loop) (finite_product))) *|) (v (loop) B)) (v (loop) A)))))) (v (loop) A))))))",
    Parse_tactic.parse "MP_TAC THM 896665165887867911" THEN
    Parse_tactic.parse "REWRITE_TAC [ THM 589786651697590584 ]") false;;

(* "|--(ff (pp ((pp ((nn (A /. B)) ** B)) ** (nn (A \. C)))) --> gg C)" *)

register_proof 222256382506930439 (
  fun () ->
    decode_goal [] "(a (c (fun (scs_v39) (bool)) RC) (a (a (c (fun (stable_sy) (fun (tri_sy) (scs_v39))) <=.) (a (c (fun (loop) (stable_sy)) mark_term) (a (c (fun (cc_v11) (loop)) plane) (a (a (c (fun (loop) (fun (loop) (cc_v11))) *.) (a (c (fun (cc_v11) (loop)) plane) (a (a (c (fun (loop) (fun (loop) (cc_v11))) *.) (a (c (fun (finite_product) (loop)) plane_norm) (a (a (c (fun (loop) (fun (loop) (finite_product))) **.) (v (loop) A)) (v (loop) B)))) (v (loop) B)))) (a (c (fun (finite_product) (loop)) plane_norm) (a (a (c (fun (loop) (fun (loop) (finite_product))) *|) (v (loop) A)) (v (loop) C))))))) (a (c (fun (loop) (tri_sy)) TAGB) (v (loop) C))))",
    Parse_tactic.parse "MP_TAC THM 1890961052333620652" THEN
    Parse_tactic.parse "ASM_MESON_TAC [ THM 631359747053030193 ; THM 4072279578834798085 ; THM 4275337362867714136 ; THM 3594022267544265710 ]") false;;

(* "|--(ff (nn ((nn (C /. (nn ( (nn (C /. A)) \. C)))) \. C)) --> gg (nn ((nn (C /. A)) \. C)))" *)

register_proof 1588737427990742600 (
  fun () ->
    decode_goal [] "(a (c (fun (scs_v39) (bool)) RC) (a (a (c (fun (stable_sy) (fun (tri_sy) (scs_v39))) <=.) (a (c (fun (loop) (stable_sy)) mark_term) (a (c (fun (finite_product) (loop)) plane_norm) (a (a (c (fun (loop) (fun (loop) (finite_product))) *|) (a (c (fun (finite_product) (loop)) plane_norm) (a (a (c (fun (loop) (fun (loop) (finite_product))) **.) (v (loop) C)) (a (c (fun (finite_product) (loop)) plane_norm) (a (a (c (fun (loop) (fun (loop) (finite_product))) *|) (a (c (fun (finite_product) (loop)) plane_norm) (a (a (c (fun (loop) (fun (loop) (finite_product))) **.) (v (loop) C)) (v (loop) A)))) (v (loop) C)))))) (v (loop) C))))) (a (c (fun (loop) (tri_sy)) TAGB) (a (c (fun (finite_product) (loop)) plane_norm) (a (a (c (fun (loop) (fun (loop) (finite_product))) *|) (a (c (fun (finite_product) (loop)) plane_norm) (a (a (c (fun (loop) (fun (loop) (finite_product))) **.) (v (loop) C)) (v (loop) A)))) (v (loop) C))))))",
    Parse_tactic.parse "REWRITE_TAC [ THM 2448164814320514290 ]") false;;

(* "|--(ff (nn ((nn (C /. A)) \. C)) --> gg (nn ((nn (C /. (nn ( (nn (C /. A)) \. C)))) \. C)))" *)

register_proof 471200694277467165 (
  fun () ->
    decode_goal [] "(a (c (fun (scs_v39) (bool)) RC) (a (a (c (fun (stable_sy) (fun (tri_sy) (scs_v39))) <=.) (a (c (fun (loop) (stable_sy)) mark_term) (a (c (fun (finite_product) (loop)) plane_norm) (a (a (c (fun (loop) (fun (loop) (finite_product))) *|) (a (c (fun (finite_product) (loop)) plane_norm) (a (a (c (fun (loop) (fun (loop) (finite_product))) **.) (v (loop) C)) (v (loop) A)))) (v (loop) C))))) (a (c (fun (loop) (tri_sy)) TAGB) (a (c (fun (finite_product) (loop)) plane_norm) (a (a (c (fun (loop) (fun (loop) (finite_product))) *|) (a (c (fun (finite_product) (loop)) plane_norm) (a (a (c (fun (loop) (fun (loop) (finite_product))) **.) (v (loop) C)) (a (c (fun (finite_product) (loop)) plane_norm) (a (a (c (fun (loop) (fun (loop) (finite_product))) *|) (a (c (fun (finite_product) (loop)) plane_norm) (a (a (c (fun (loop) (fun (loop) (finite_product))) **.) (v (loop) C)) (v (loop) A)))) (v (loop) C)))))) (v (loop) C))))))",
    Parse_tactic.parse "REWRITE_TAC [ THM 1416075696371770642 ]") false;;

(* "|--(ff (pp ( (pp ((nn (A /. B)) ** B)) ** (nn ((pp ((nn (A /. B)) ** B)) \. C)))) --> gg (C))" *)

register_proof 738642687812881077 (
  fun () ->
    decode_goal [] "(a (c (fun (scs_v39) (bool)) RC) (a (a (c (fun (stable_sy) (fun (tri_sy) (scs_v39))) <=.) (a (c (fun (loop) (stable_sy)) mark_term) (a (c (fun (cc_v11) (loop)) plane) (a (a (c (fun (loop) (fun (loop) (cc_v11))) *.) (a (c (fun (cc_v11) (loop)) plane) (a (a (c (fun (loop) (fun (loop) (cc_v11))) *.) (a (c (fun (finite_product) (loop)) plane_norm) (a (a (c (fun (loop) (fun (loop) (finite_product))) **.) (v (loop) A)) (v (loop) B)))) (v (loop) B)))) (a (c (fun (finite_product) (loop)) plane_norm) (a (a (c (fun (loop) (fun (loop) (finite_product))) *|) (a (c (fun (cc_v11) (loop)) plane) (a (a (c (fun (loop) (fun (loop) (cc_v11))) *.) (a (c (fun (finite_product) (loop)) plane_norm) (a (a (c (fun (loop) (fun (loop) (finite_product))) **.) (v (loop) A)) (v (loop) B)))) (v (loop) B)))) (v (loop) C))))))) (a (c (fun (loop) (tri_sy)) TAGB) (v (loop) C))))",
    Parse_tactic.parse "MP_TAC THM 896665165887867911" THEN
    Parse_tactic.parse "REWRITE_TAC [ THM 212959095654130630 ]") false;;

(* "|--(ff (pp ((pp ((nn (A /. B)) ** B)) ** (nn (A \. (pp (<> C)))))) --> gg (pp (<> C)))" *)

register_proof 3780005030532697670 (
  fun () ->
    decode_goal [] "(a (c (fun (scs_v39) (bool)) RC) (a (a (c (fun (stable_sy) (fun (tri_sy) (scs_v39))) <=.) (a (c (fun (loop) (stable_sy)) mark_term) (a (c (fun (cc_v11) (loop)) plane) (a (a (c (fun (loop) (fun (loop) (cc_v11))) *.) (a (c (fun (cc_v11) (loop)) plane) (a (a (c (fun (loop) (fun (loop) (cc_v11))) *.) (a (c (fun (finite_product) (loop)) plane_norm) (a (a (c (fun (loop) (fun (loop) (finite_product))) **.) (v (loop) A)) (v (loop) B)))) (v (loop) B)))) (a (c (fun (finite_product) (loop)) plane_norm) (a (a (c (fun (loop) (fun (loop) (finite_product))) *|) (v (loop) A)) (a (c (fun (cc_v11) (loop)) plane) (a (c (fun (loop) (cc_v11)) ssqrt) (v (loop) C))))))))) (a (c (fun (loop) (tri_sy)) TAGB) (a (c (fun (cc_v11) (loop)) plane) (a (c (fun (loop) (cc_v11)) ssqrt) (v (loop) C))))))",
    Parse_tactic.parse "MP_TAC THM 896665165887867911" THEN
    Parse_tactic.parse "REWRITE_TAC [ THM 222256382506930439 ]") false;;

(* "|--(ff (pp( X ** (nn ( (pp (<>Y)) /. (pp (<> Z)))))) --> gg (nn ((pp (X ** (pp (<>Y)))) /. (pp (<>  Z)))))" *)

register_proof 618966604087475093 (
  fun () ->
    decode_goal [] "(a (c (fun (scs_v39) (bool)) RC) (a (a (c (fun (stable_sy) (fun (tri_sy) (scs_v39))) <=.) (a (c (fun (loop) (stable_sy)) mark_term) (a (c (fun (cc_v11) (loop)) plane) (a (a (c (fun (loop) (fun (loop) (cc_v11))) *.) (v (loop) X)) (a (c (fun (finite_product) (loop)) plane_norm) (a (a (c (fun (loop) (fun (loop) (finite_product))) **.) (a (c (fun (cc_v11) (loop)) plane) (a (c (fun (loop) (cc_v11)) ssqrt) (v (loop) Y)))) (a (c (fun (cc_v11) (loop)) plane) (a (c (fun (loop) (cc_v11)) ssqrt) (v (loop) Z))))))))) (a (c (fun (loop) (tri_sy)) TAGB) (a (c (fun (finite_product) (loop)) plane_norm) (a (a (c (fun (loop) (fun (loop) (finite_product))) **.) (a (c (fun (cc_v11) (loop)) plane) (a (a (c (fun (loop) (fun (loop) (cc_v11))) *.) (v (loop) X)) (a (c (fun (cc_v11) (loop)) plane) (a (c (fun (loop) (cc_v11)) ssqrt) (v (loop) Y)))))) (a (c (fun (cc_v11) (loop)) plane) (a (c (fun (loop) (cc_v11)) ssqrt) (v (loop) Z))))))))",
    Parse_tactic.parse "REWRITE_TAC [ THM 4284981787273005745 ]") false;;