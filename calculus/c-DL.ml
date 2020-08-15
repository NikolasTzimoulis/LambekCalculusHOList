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
"form = Atom string
      | /. form form
	  | ** form form
	  | \. form form
	  | <> form
	  | || form";;	  
let struct_INDUCT, struct_RECURSION = define_type 
"strF = ff form 
	  | <>^ strF
	  | **^ strF strF;
 strG = gg form
	  | /.^ strG strF
	  | \.^ strF strG
	  | ||^ strG";;
let seq_INDUCT, seq_RECURSION = define_type 	  
"seq  = --> strF strG";;	 

let provable_RULES, provable_INDUCT, provable_CASES = new_inductive_definition
`(!p. |--(ff (Atom p) --> gg (Atom p))) /\
(!X Y A. |--(X --> gg A) /\ |--(ff A --> Y) ==> |--(X --> Y)) /\
(!X Y Z. |--(Y --> X \.^ Z) ==> |--(X **^ Y --> Z)) /\
(!X Y Z. |--(X **^ Y --> Z) ==> |--(Y --> X \.^ Z)) /\
(!X Y Z. |--(X --> Z /.^ Y) ==> |--(X **^ Y --> Z)) /\
(!X Y Z. |--(X **^ Y --> Z) ==> |--(X --> Z /.^ Y)) /\
(!X Y. |--(<>^X --> Y) ==> |--(X --> ||^Y)) /\
(!X Y. |--(X --> ||^Y) ==> |--(<>^X --> Y)) /\
(!X Y Z W. |--(X **^ (Y **^ <>^Z) --> W) ==> |--((X **^ Y) **^ <>^Z --> W)) /\
(!X Y Z W. |--((X **^ <>^Y) **^ Z --> W) ==> |--((X **^ Z) **^ <>^Y --> W)) /\
(!A B Y. |--(ff A **^ ff B --> Y) ==> |--(ff (A ** B) --> Y)) /\
(!A B X Y. |--(X --> gg A) /\ |--(Y --> gg B) ==> |--(X **^ Y --> gg (A ** B))) /\
(!A B X Y. |--(X --> gg A) /\ |--(ff B --> Y) ==> |--(ff (A \. B) --> X \.^ Y)) /\
(!A B X. |--(X --> ff A \.^ gg B) ==> |--(X --> gg (A \. B))) /\
(!A B X Y. |--(ff B --> Y) /\ |--(X --> gg A) ==> |--(ff (B /. A) --> Y /.^ X)) /\
(!A B X. |--(X --> gg B /.^ ff A) ==> |--(X --> gg (B /. A))) /\
(!A Y. |--(<>^ff A --> Y) ==> |--(ff (<>A) --> Y)) /\
(!A X. |--(X --> gg A) ==> |--(<>^X --> gg (<>A))) /\
(!A Y. |--(ff A --> Y) ==> |--(ff (||A) --> ||^Y)) /\
(!A X. |--(X --> ||^gg A) ==> |--(X --> gg (||A)))`;;



let rule1 = `|--(ff (Atom p) --> gg (Atom p))`;;
let rule2 = `|--(X --> gg A) /\ |--(ff A --> Y) ==> |--(X --> Y)`;;
let rule3 = `|--(Y --> X \.^ Z) ==> |--(X **^ Y --> Z)`;;
let rule4 = `|--(X **^ Y --> Z) ==> |--(Y --> X \.^ Z)`;;
let rule5 = `|--(X --> Z /.^ Y) ==> |--(X **^ Y --> Z)`;;
let rule6 = `|--(X **^ Y --> Z) ==> |--(X --> Z /.^ Y)`;;
let rule7 = `|--(<>^X --> Y) ==> |--(X --> ||^Y)`;;
let rule8 = `|--(X --> ||^Y) ==> |--(<>^X --> Y)`;;
let rule9 = `|--(X **^ (Y **^ <>^Z) --> W) ==> |--((X **^ Y) **^ <>^Z --> W)`;;
let rule10 = `|--((X **^ <>^Y) **^ Z --> W) ==> |--((X **^ Z) **^ <>^Y --> W)`;;
let rule11 = `|--(ff A **^ ff B --> Y) ==> |--(ff (A ** B) --> Y)`;;
let rule12 = `|--(X --> gg A) /\ |--(Y --> gg B) ==> |--(X **^ Y --> gg (A ** B))`;;
let rule13 = `|--(X --> gg A) /\ |--(ff B --> Y) ==> |--(ff (A \. B) --> X \.^ Y)`;;
let rule14 = `|--(X --> ff A \.^ gg B) ==> |--(X --> gg (A \. B))`;;
let rule15 = `|--(ff B --> Y) /\ |--(X --> gg A) ==> |--(ff (B /. A) --> Y /.^ X)`;;
let rule16 = `|--(X --> gg B /.^ ff A) ==> |--(X --> gg (B /. A))`;;
let rule17 = `|--(<>^ff A --> Y) ==> |--(ff (<>A) --> Y)`;;
let rule18 = `|--(X --> gg A) ==> |--(<>^X --> gg (<>A))`;;
let rule19 = `|--(ff A --> Y) ==> |--(ff (||A) --> ||^Y)`;;
let rule20 = `|--(X --> ||^gg A) ==> |--(X --> gg (||A))`;;


(*
MESON[provable_RULES;lala3_thm] tm05;;
toSexpTerm (concl it);;
*)


let lala1 = `|-- (ff (Atom p) --> gg (Atom p))`;;
let lala2 = `|-- (ff (Atom p ** Atom q) --> gg (Atom p ** Atom q))`;;
let lala3 = `!A. |-- (ff A --> gg A)`;;
let lala4 = `|-- (<>^ff (||Atom C) --> (ff (Atom A) **^ ff ((Atom A \. Atom B) /. Atom C)) \.^ gg (Atom B))`;;
let lala5 = `|-- (ff (Atom A) --> gg ((Atom C /. Atom A) \. Atom C))`;;
let lala6 = `|-- (ff A --> gg ((C /. A) \. C))`;;

let tm01 = `|-- (((ff ((Atom p))) **^ (ff ((Atom q)))) -->  (gg (( Atom p) ** (Atom q))))`;;
let tm02 = `|--(ff ((Atom p) ** (Atom q)) --> gg ( (Atom p) ** (Atom q)))`;;
let tm03 = `|--(ff ( (Atom A) /. (Atom N)) **^ (ff (Atom N)) --> gg (Atom A))`;;
let tm04 = `|--(ff ( ((Atom A) /. (Atom N)) ** (Atom N)) --> gg (Atom A))`;;
let tm05 = `|--((ff ( (X /. Y) ** Y)) --> gg X)`;;

(*
string_INDUCT, string_RECURSION
form_INDUCT, form_RECURSION
struct_INDUCT, struct_RECURSION
seq_INDUCT, seq_RECURSION
provable_RULES provable_INDUCT provable_CASES
struct_INDUCT, struct_RECURSION


g lala3;;
e(MATCH_MP_TAC form_INDUCT);;
e(MESON_TAC[provable_RULES; form_INDUCT; struct_INDUCT]);;
let lala3_thm = top_thm();;

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
e(MESON_TAC[form_struct_RECURSION; provable_RULES]);;
e(ASM_SIMP_TAC[provable_INDUCT]);;
METIS [form_RECURSION; struct_RECURSION; provable_RULES] lala3;
MESON [form_RECURSION; struct_RECURSION; provable_RULES] lala3;
MESON[provable_RULES] lala
*)

