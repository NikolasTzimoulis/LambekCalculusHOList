set_jrh_lexer;;
open Lib;;
open Printer;;
open Theorem_fingerprint;;
open Import_proofs;;
open Tactics;;

Printer.current_encoding := Printer.Sexp;;


(* "|--(ff ((nn (nAtom Y))) --> gg ( nn ( (pp (pAtom X)) /. ( nn ((nn (nAtom Y)) \. (pp (pAtom X)))))))" *)

register_proof 1433254538010511209 (
  fun () ->
    decode_goal [] "(a (c (fun (scs_v39) (bool)) RC) (a (a (c (fun (stable_sy) (fun (tri_sy) (scs_v39))) <=.) (a (c (fun (loop) (stable_sy)) mark_term) (a (c (fun (finite_product) (loop)) plane_norm) (a (c (fun (list (char)) (finite_product)) drop1) (v (list (char)) Y))))) (a (c (fun (loop) (tri_sy)) TAGB) (a (c (fun (finite_product) (loop)) plane_norm) (a (a (c (fun (loop) (fun (loop) (finite_product))) **.) (a (c (fun (cc_v11) (loop)) plane) (a (c (fun (list (char)) (cc_v11)) drop0) (v (list (char)) X)))) (a (c (fun (finite_product) (loop)) plane_norm) (a (a (c (fun (loop) (fun (loop) (finite_product))) *|) (a (c (fun (finite_product) (loop)) plane_norm) (a (c (fun (list (char)) (finite_product)) drop1) (v (list (char)) Y)))) (a (c (fun (cc_v11) (loop)) plane) (a (c (fun (list (char)) (cc_v11)) drop0) (v (list (char)) X))))))))))",
    Parse_tactic.parse "MP_TAC THM 1890961052333620652" THEN
    Parse_tactic.parse "ASM_MESON_TAC [ THM 885342210898247996 ]") false;;

(* "|--(ff (nn ( (nn ( (nn (nAtom X)) /. ( nn ((pp (pAtom Y)) \. (nn (nAtom X)))))) \. (nn (nAtom X)))) --> gg (nn ((pp (pAtom Y)) \. (nn (nAtom X)))))" *)

register_proof 2653435474100312580 (
  fun () ->
    decode_goal [] "(a (c (fun (scs_v39) (bool)) RC) (a (a (c (fun (stable_sy) (fun (tri_sy) (scs_v39))) <=.) (a (c (fun (loop) (stable_sy)) mark_term) (a (c (fun (finite_product) (loop)) plane_norm) (a (a (c (fun (loop) (fun (loop) (finite_product))) *|) (a (c (fun (finite_product) (loop)) plane_norm) (a (a (c (fun (loop) (fun (loop) (finite_product))) **.) (a (c (fun (finite_product) (loop)) plane_norm) (a (c (fun (list (char)) (finite_product)) drop1) (v (list (char)) X)))) (a (c (fun (finite_product) (loop)) plane_norm) (a (a (c (fun (loop) (fun (loop) (finite_product))) *|) (a (c (fun (cc_v11) (loop)) plane) (a (c (fun (list (char)) (cc_v11)) drop0) (v (list (char)) Y)))) (a (c (fun (finite_product) (loop)) plane_norm) (a (c (fun (list (char)) (finite_product)) drop1) (v (list (char)) X)))))))) (a (c (fun (finite_product) (loop)) plane_norm) (a (c (fun (list (char)) (finite_product)) drop1) (v (list (char)) X))))))) (a (c (fun (loop) (tri_sy)) TAGB) (a (c (fun (finite_product) (loop)) plane_norm) (a (a (c (fun (loop) (fun (loop) (finite_product))) *|) (a (c (fun (cc_v11) (loop)) plane) (a (c (fun (list (char)) (cc_v11)) drop0) (v (list (char)) Y)))) (a (c (fun (finite_product) (loop)) plane_norm) (a (c (fun (list (char)) (finite_product)) drop1) (v (list (char)) X))))))))",
    Parse_tactic.parse "ASM_MESON_TAC [ THM 885342210898247996 ]") false;;