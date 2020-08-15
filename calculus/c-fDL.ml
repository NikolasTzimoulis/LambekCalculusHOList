needs "holist.ml";;

parse_as_prefix "<>";;
parse_as_prefix "<>^";;
parse_as_prefix "||";;
parse_as_prefix "||^";;
parse_as_prefix "|--";;
parse_as_infix("**",(16,"right"));;
parse_as_infix("**^",(16,"right"));;
parse_as_infix("/.",(16,"right"));;
parse_as_infix("/.^",(16,"right"));;
parse_as_infix("\\.",(16,"right"));;
parse_as_infix("\\.^",(16,"right"));;
parse_as_infix("-->",(10,"right"));;



let string_INDUCT, string_RECURSION = define_type "string = String num";;

let form_INDUCT, form_RECURSION = define_type 
"form = pp pform
	  | nn nform;
pform = pAtom string 
	  | ** form form
	  | <> form;
nform = nAtom string 
	  | \. form form	  
	  | /. form form
	  | || form";;	
      
let struct_INDUCT, struct_RECURSION = define_type
"strF = ff form 
	  | <>^ strF
	  | **^ strF strF
	  | focf form;
 strG = gg form
	  | ||^ strG
	  | /.^ strG strF
	  | \.^ strF strG
	  | focg form";;
let seq_INDUCT, seq_RECURSION = define_type  
"seq  = --> strF strG";;	




let provable_RULES, provable_INDUCT, provable_CASES = new_inductive_definition `
(!p. |--(ff (pp(pAtom p)) --> focg (pp(pAtom p)))) /\
(!n. |--(focf (nn(nAtom n)) --> gg (nn(nAtom n)))) /\
(! X A. |-- (X --> focg (pp A)) ==> |-- (X --> gg (pp A))) /\
(! Y B. |-- (focf (nn B) --> Y) ==> |-- ((ff (nn B)) --> Y)) /\
(! Y A. |-- ((ff (pp A)) --> Y) ==> |-- (focf (pp A) --> Y)) /\
(! X B. |-- (X --> gg (nn B)) ==> |-- (X --> focg (nn B))) /\
(!X Y A. |--(X --> gg A) /\ |--(ff A --> Y) ==> |--(X --> Y)) /\
(!X Y Z. |--(Y --> X \.^ Z) ==> |--(X **^ Y --> Z)) /\
(!X Y Z. |--(X **^ Y --> Z) ==> |--(Y --> X \.^ Z)) /\
(!X Y Z. |--(X --> Z /.^ Y) ==> |--(X **^ Y --> Z)) /\
(!X Y Z. |--(X **^ Y --> Z) ==> |--(X --> Z /.^ Y)) /\
(!X Y Z W. |--(X **^ (Y **^ <>^Z) --> W) ==> |--((X **^ Y) **^ <>^Z --> W)) /\
(!X Y Z W. |--((X **^ <>^Y) **^ Z --> W) ==> |--((X **^ Z) **^ <>^Y --> W)) /\
(!A B Y. |--(ff A **^ ff B --> Y) ==> |--(ff (pp(A ** B)) --> Y)) /\
(!A B X Y. |--(X --> focg A) /\ |--(Y --> focg B) ==> |--(X **^ Y --> focg(pp(A ** B)))) /\
(!A B X Y. |--(X --> focg A) /\ |--(focf B --> Y) ==> |--(focf(nn(A \. B)) --> X \.^ Y)) /\
(!A B X. |--(X --> ff A \.^ gg B) ==> |--(X --> gg(nn(A \. B)))) /\
(!A B X Y. |--(X --> focg A) /\ |--(focf B --> Y) ==> |--(focf(nn(B /. A)) --> Y /.^ X)) /\
(!A B X. |--(X --> gg A /.^ ff B) ==> |--(X --> gg (nn(A /. B)))) /\
(!A Y. |--(<>^ff A --> Y) ==> |--(ff (pp(<>A)) --> Y)) /\
(!A X. |--(X --> focg A) ==> |--(<>^X --> focg (pp(<>A)))) /\
(!A Y. |--(focf A --> Y) ==> |--(focf (nn (||A)) --> ||^Y)) /\
(!A X. |--(X --> ||^gg A) ==> |--(X --> gg (nn (||A))))`;;




(*
let lala2 = `|-- (ff (Atom p ** Atom q) --> gg (Atom p ** Atom q))`;;
let lala3 = `!A. |-- (ff A --> gg A)`;;
let lala4 = `|-- (<>^ff (||Atom C) --> (ff (Atom A) **^ ff ((Atom A \. Atom B) /. Atom C)) \.^ gg (Atom B))`;;
let lala5 = `|-- (ff (Atom A) --> gg ((Atom C /. Atom A) \. Atom C))`;;
let lala6 = `|-- (ff A --> gg ((C /. A) \. C))`;;
*)

let rule1 = `|-- (ff (pp(pAtom p)) --> focg (pp(pAtom p)))`;;
let rule2 = `|--(focf (nn(nAtom n)) --> gg (nn(nAtom n)))`;;
let rule3 = `|-- (X --> focg (pp A)) ==> |-- (X --> gg (pp A))`;;
let rule4 = `|-- (focf (nn B) --> Y) ==> |-- ((ff (nn B)) --> Y)`;;
let rule5 = `|-- ((ff (pp A)) --> Y) ==> |-- (focf (pp A) --> Y)`;;
let rule6 = `|-- (X --> gg (nn B)) ==> |-- (X --> focg (nn B))`;;
let rule7 = `|--(X --> gg A) /\ |--(ff A --> Y) ==> |--(X --> Y)`;;
let rule8 = `|--(Y --> X \.^ Z) ==> |--(X **^ Y --> Z)`;;
let rule9 = `|--(X **^ Y --> Z) ==> |--(Y --> X \.^ Z)`;;
let rule10 = `|--(X --> Z /.^ Y) ==> |--(X **^ Y --> Z)`;;
let rule11 = `|--(X **^ Y --> Z) ==> |--(X --> Z /.^ Y)`;;
let rule16 = `|--(X **^ (Y **^ <>^Z) --> W) ==> |--((X **^ Y) **^ <>^Z --> W)`;;
let rule17 = `|--((X **^ <>^Y) **^ Z --> W) ==> |--((X **^ Z) **^ <>^Y --> W)`;;
let rule18 = `|--(ff A **^ ff B --> Y) ==> |--(ff (pp(A ** B)) --> Y)`;;
let rule19 = `|--(X --> focg A) /\ |--(Y --> focg B) ==> |--(X **^ Y --> focg(pp(A ** B)))`;;
let rule26 = `|--(X --> focg A) /\ |--(focf B --> Y) ==> |--(focf(nn(A \. B)) --> X \.^ Y)`;;
let rule27 = `|--(X --> ff A \.^ gg B) ==> |--(X --> gg(nn(A \. B)))`;;
let rule28 = `|--(X --> focg A) /\ |--(focf B --> Y) ==> |--(focf(nn(B /. A)) --> Y /.^ X)`;;
let rule29 = `|--(X --> gg A /.^ ff B) ==> |--(X --> gg (nn(A /. B)))`;;
let rule30 = `|--(<>^ff A --> Y) ==> |--(ff (pp(<>A)) --> Y)`;;
let rule31 = `|--(X --> focg A) ==> |--(<>^X --> focg (pp(<>A)))`;;
let rule32 = `|--(focf A --> Y) ==> |--(focf (nn (||A)) --> ||^Y)`;;
let rule33 = `|--(X --> ||^gg A) ==> |--(X --> gg (nn (||A)))`;;


(*
MESON[provable_RULES] rule
toSexpTerm (concl it);;
*)

let simp1 = `|-- (((ff ((pp (pAtom p)))) **^ (ff ((pp (pAtom q))))) -->  (focg (pp ((pp (pAtom p)) ** (pp (pAtom q))))))`;;
let simp2 = `(|-- (((ff ((pp (pAtom p)))) **^ (ff ((pp (pAtom q))))) --> X)) ==> (|--((ff (pp ((pp (pAtom p)) ** (pp (pAtom q))))) --> X))`;;
             
let lala2 = `|-- ((ff (pp ((pp (pAtom p)) ** (pp (pAtom q))))) --> (gg (pp ((pp (pAtom p)) ** (pp (pAtom q))))))`;;

let tm01 = `|--(ff (pp ((nn ((pp (pAtom A)) /. (nn (nAtom N))) ** (nn (nAtom N))))) --> gg (pp (pAtom A)))`;;
let tm02 = `|-- (ff (nn ( (pp (pAtom A)) /. (nn (nAtom N)))) **^ ff (nn (nAtom N)) --> gg (pp (pAtom A)))`;;



(*
MESON[provable_RULES] rule

string_INDUCT, string_RECURSION
form_INDUCT, form_RECURSION
struct_INDUCT, struct_RECURSION
seq_INDUCT, seq_RECURSION
provable_RULES provable_INDUCT provable_CASES
struct_INDUCT, struct_RECURSION


e(MATCH_MP_TAC form_INDUCT);;
e(MESON_TAC[provable_RULES; form_INDUCT]);;

MESON[provable_RULES;form_INDUCT] lala


e(ASM_MESON_TAC[provable_INDUCT]);;


g lala3;;
e(MATCH_MP_TAC provable_INDUCT);;
e(REWRITE_TAC[provable_INDUCT]);;
e(MATCH_MP_TAC provable_INDUCT THEN REPEAT STRIP_TAC);;
e(MATCH_MP_TAC provable_CASES THEN REPEAT STRIP_TAC);;
e(MATCH_MP_TAC provable_RULES);;
e(MESON_TAC[provable_CASES]);;
e(MESON_TAC[provable_RULES]);; this solves lala1 and lala2
e(MESON_TAC[form_RECURSION; struct_RECURSION; provable_RULES]);;
e(ASM_SIMP_TAC[provable_INDUCT]);;
e(METIS_TAC[form_RECURSION; struct_RECURSION; provable_RULES]);;
MESON[provable_RULES] lala

string_INDUCT;;
toSexp string_INDUCT;;
string_RECURSION;;
toSexp string_RECURSION;;
form_INDUCT;;
toSexp form_INDUCT;;
form_RECURSION;;
toSexp form_RECURSION;;
struct_INDUCT;;
toSexp struct_INDUCT;;
struct_RECURSION;;
toSexp struct_RECURSION;;
seq_INDUCT;;
toSexp seq_INDUCT;;
seq_RECURSION;;
toSexp seq_RECURSION;;
provable_RULES;;
toSexp provable_RULES;;
provable_INDUCT;;
toSexp provable_INDUCT;;
provable_CASES;;
toSexp provable_CASES;;
*)