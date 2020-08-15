set_jrh_lexer;;
open Lib;;
open Printer;;
open Theorem_fingerprint;;
open Import_proofs;;
open Tactics;;

Printer.current_encoding := Printer.Sexp;;


(* "|--(ff ( ((Atom A) /. (Atom N)) ** (Atom N)) --> gg (Atom A))" *)

register_proof 2638008682462918684 (
  fun () ->
    decode_goal [] "(a (c (fun (scs_v39) (bool)) RC) (a (a (c (fun (stable_sy) (fun (tri_sy) (scs_v39))) <=.) (a (c (fun (loop) (stable_sy)) mark_term) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) A))) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) N)))) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) N))))) (a (c (fun (loop) (tri_sy)) TAGB) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) A)))))",
    Parse_tactic.parse "MP_TAC THM 1679452577520289523" THEN
    Parse_tactic.parse "MP_TAC THM 1679452577520289523" THEN
    Parse_tactic.parse "ASM_MESON_TAC [ THM 324105359611762671 ; THM 4245526831407530504 ; THM 4208037883098591283 ; THM 2898052989604839426 ]") false;;

(* "|--(ff (Atom Y) --> gg ( (Atom X) /. ((Atom Y) \. (Atom X))))" *)

register_proof 558101816666507998 (
  fun () ->
    decode_goal [] "(a (c (fun (scs_v39) (bool)) RC) (a (a (c (fun (stable_sy) (fun (tri_sy) (scs_v39))) <=.) (a (c (fun (loop) (stable_sy)) mark_term) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) Y)))) (a (c (fun (loop) (tri_sy)) TAGB) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) X))) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) Y))) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) X)))))))",
    Parse_tactic.parse "MP_TAC THM 2783218001153660763" THEN
    Parse_tactic.parse "ASM_MESON_TAC [ THM 1679452577520289523 ; THM 2331699710152630998 ; THM 2346343096004033294 ; THM 4208037883098591283 ; THM 2716539267995925388 ]") false;;

(* "|--(ff (Atom Y) --> gg (((Atom X) /. (Atom Y)) \. (Atom X)))" *)

register_proof 1236623560858316722 (
  fun () ->
    decode_goal [] "(a (c (fun (scs_v39) (bool)) RC) (a (a (c (fun (stable_sy) (fun (tri_sy) (scs_v39))) <=.) (a (c (fun (loop) (stable_sy)) mark_term) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) Y)))) (a (c (fun (loop) (tri_sy)) TAGB) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) X))) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) Y)))) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) X))))))",
    Parse_tactic.parse "MP_TAC THM 2783218001153660763" THEN
    Parse_tactic.parse "ASM_MESON_TAC [ THM 51220887018286029 ]") false;;

(* "|--(ff ( ( (Atom A) /. ((Atom B) \. (Atom A))) \. (Atom A)) --> gg ((Atom B) \. (Atom A)))" *)

register_proof 460115165279019657 (
  fun () ->
    decode_goal [] "(a (c (fun (scs_v39) (bool)) RC) (a (a (c (fun (stable_sy) (fun (tri_sy) (scs_v39))) <=.) (a (c (fun (loop) (stable_sy)) mark_term) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) A))) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) B))) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) A))))) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) A))))) (a (c (fun (loop) (tri_sy)) TAGB) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) B))) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) A))))))",
    Parse_tactic.parse "MP_TAC THM 2783218001153660763" THEN
    Parse_tactic.parse "SIMP_TAC [ THM 558101816666507998 ; THM 51220887018286029 ]") false;;

(* "|--(ff ((Atom B) \. (Atom A))--> gg ( ( (Atom A) /. ((Atom B) \. (Atom A))) \. (Atom A)))" *)

register_proof 4397087163139479502 (
  fun () ->
    decode_goal [] "(a (c (fun (scs_v39) (bool)) RC) (a (a (c (fun (stable_sy) (fun (tri_sy) (scs_v39))) <=.) (a (c (fun (loop) (stable_sy)) mark_term) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) B))) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) A))))) (a (c (fun (loop) (tri_sy)) TAGB) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) A))) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) B))) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) A))))) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) A))))))",
    Parse_tactic.parse "MP_TAC THM 2783218001153660763" THEN
    Parse_tactic.parse "ASM_MESON_TAC [ THM 51220887018286029 ]") false;;

(* "|--(ff ((((Atom A) /. (Atom B)) ** (Atom B)) ** ((Atom A) \. (Atom C))) --> gg (Atom C))" *)

register_proof 4513506378801904449 (
  fun () ->
    decode_goal [] "(a (c (fun (scs_v39) (bool)) RC) (a (a (c (fun (stable_sy) (fun (tri_sy) (scs_v39))) <=.) (a (c (fun (loop) (stable_sy)) mark_term) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) A))) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) B)))) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) B)))) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) A))) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) C)))))) (a (c (fun (loop) (tri_sy)) TAGB) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) C)))))",
    Parse_tactic.parse "MP_TAC THM 2783218001153660763" THEN
    Parse_tactic.parse "SIMP_TAC [ THM 2638008682462918684 ; THM 51220887018286029 ]") false;;

(* "|--(ff ((Atom A) ** ((((Atom A) \. (Atom B)) /. ((Atom A) \. (Atom B))) ** ((Atom A) \. (Atom B))))--> gg (Atom B))" *)

register_proof 1876798653094833217 (
  fun () ->
    decode_goal [] "(a (c (fun (scs_v39) (bool)) RC) (a (a (c (fun (stable_sy) (fun (tri_sy) (scs_v39))) <=.) (a (c (fun (loop) (stable_sy)) mark_term) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) A))) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) A))) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) B)))) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) A))) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) B))))) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) A))) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) B))))))) (a (c (fun (loop) (tri_sy)) TAGB) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) B)))))",
    Parse_tactic.parse "MP_TAC THM 2783218001153660763" THEN
    Parse_tactic.parse "ASM_MESON_TAC [ THM 1236764197481027867 ; THM 2898052989604839426 ; THM 2331699710152630998 ; THM 2716539267995925388 ; THM 4208037883098591283 ; THM 324105359611762671 ; THM 4245526831407530504 ]") false;;

(* "|--(ff (((Atom A) ** (Atom B)) ** (Atom A)) --> gg ((( ((Atom A) ** (Atom B)) ** (Atom C)) /. (Atom C)) ** (Atom A)))" *)

register_proof 114238463023881230 (
  fun () ->
    decode_goal [] "(a (c (fun (scs_v39) (bool)) RC) (a (a (c (fun (stable_sy) (fun (tri_sy) (scs_v39))) <=.) (a (c (fun (loop) (stable_sy)) mark_term) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) A))) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) B)))) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) A))))) (a (c (fun (loop) (tri_sy)) TAGB) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) A))) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) B)))) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) C)))) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) C)))) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) A))))))",
    Parse_tactic.parse "MP_TAC THM 2783218001153660763" THEN
    Parse_tactic.parse "ASM_MESON_TAC [ THM 51220887018286029 ]") false;;

(* "|--(ff (((((Atom A) ** (Atom B))) \. (((Atom A) ** (Atom B)) ** (Atom C))) /. (Atom C)) --> gg ((Atom C) /. (((Atom C) /. (Atom B)) ** (Atom B))))" *)

register_proof 2876977343986042984 (
  fun () ->
    decode_goal [] "(a (c (fun (scs_v39) (bool)) RC) (a (a (c (fun (stable_sy) (fun (tri_sy) (scs_v39))) <=.) (a (c (fun (loop) (stable_sy)) mark_term) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) A))) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) B)))) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) A))) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) B)))) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) C))))) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) C))))) (a (c (fun (loop) (tri_sy)) TAGB) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) C))) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) C))) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) B)))) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) B)))))))",
    Parse_tactic.parse "MP_TAC THM 2783218001153660763" THEN
    Parse_tactic.parse "SIMP_TAC [ THM 2638008682462918684 ; THM 4318149125608959292 ; THM 51220887018286029 ]") false;;

(* "|--(ff (((((Atom A) ** (Atom B)) \. (((Atom A) ** (Atom B)) ** (Atom C))) /. ((Atom A) /. ((Atom C)\. (Atom A))))) --> gg ((Atom C) /. (Atom C)))" *)

register_proof 1587678095110685970 (
  fun () ->
    decode_goal [] "(a (c (fun (scs_v39) (bool)) RC) (a (a (c (fun (stable_sy) (fun (tri_sy) (scs_v39))) <=.) (a (c (fun (loop) (stable_sy)) mark_term) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) A))) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) B)))) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (a (a (c (fun (loop) (fun (loop) (loop))) *.) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) A))) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) B)))) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) C))))) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) A))) (a (a (c (fun (loop) (fun (loop) (loop))) *|) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) C))) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) A))))))) (a (c (fun (loop) (tri_sy)) TAGB) (a (a (c (fun (loop) (fun (loop) (loop))) **.) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) C))) (a (c (fun (list (char)) (loop)) drop0) (v (list (char)) C))))))",
    Parse_tactic.parse "REWRITE_TAC [ THM 3312368905117307733 ]" THEN
    Parse_tactic.parse "MP_TAC THM 2783218001153660763" THEN
    Parse_tactic.parse "ASM_MESON_TAC [ THM 1679452577520289523 ; THM 4318149125608959292 ; THM 3312368905117307733 ; THM 4245526831407530504 ; THM 558101816666507998 ]") false;;