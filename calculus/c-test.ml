parse_as_prefix "<>";;
parse_as_prefix "<>^";;
parse_as_prefix "||";;
parse_as_prefix "||^";;
parse_as_prefix "|--";;
parse_as_infix("**",(16,"right"));;
parse_as_infix("**^",(16,"right"));;
parse_as_infix("##",(16,"right"));;
parse_as_infix("##^",(16,"right"));;
parse_as_infix("/-",(16,"right"));;
parse_as_infix("/-^",(16,"right"));;
parse_as_infix("\\-",(16,"right"));;
parse_as_infix("\\-^",(16,"right"));;
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
	  | /- form form
	  | \- form form
	  | <> form;
nform = nAtom string 
	  | ## form form
	  | \. form form	  
	  | /. form form
	  | || form";;	
      
let struct_INDUCT, struct_RECURSION = define_type
"strF = ff form 
	  | <>^ strF
	  | **^ strF strF
	  | /-^ strF strG
	  | \-^ strG strF
	  | focf form;
 strG = gg form
	  | ||^ strG
	  | ##^ strG strG
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
(!X Y Z. |--(Y \-^ Z --> X) ==> |--(Z --> Y ##^ X)) /\
(!X Y Z. |--(Z --> Y ##^ X) ==> |--(Y \-^ Z --> X)) /\
(!X Y Z. |--(Z /-^ X --> Y) ==> |--(Z --> Y ##^ X)) /\
(!X Y Z. |--(Z --> Y ##^ X) ==> |--(Z /-^ X --> Y)) /\
(!X Y Z W. |--(X **^ (Y **^ <>^Z) --> W) ==> |--((X **^ Y) **^ <>^Z --> W)) /\
(!X Y Z W. |--((X **^ <>^Y) **^ Z --> W) ==> |--((X **^ Z) **^ <>^Y --> W)) /\
(!A B Y. |--(ff A **^ ff B --> Y) ==> |--(ff (pp(A ** B)) --> Y)) /\
(!A B X Y. |--(X --> focg A) /\ |--(Y --> focg B) ==> |--(X **^ Y --> focg(pp(A ** B)))) /\
(!A B Y. |--(ff A /-^ gg B --> Y) ==> |--(ff (pp(A /- B)) --> Y)) /\
(!A B X Y. |--(X --> focg A) /\ |--(focf B --> Y) ==> |--(X /-^ Y --> focg(pp(A /- B)))) /\
(!A B Y. |--(gg A \-^ ff B --> Y) ==> |--(ff (pp(A \- B)) --> Y)) /\
(!A B X Y. |--(X --> focg A) /\ |--(focf B --> Y) ==> |--(Y \-^ X --> focg(pp(B \- A)))) /\
(!A B X Y. |--(focf A --> X) /\ |--(focf B --> Y) ==> |--(focf(nn(A ## B)) --> X ##^ Y)) /\
(!A B X. |--(X --> gg A ##^ gg B) ==> |--(X --> gg(nn(A ## B)))) /\
(!A B X Y. |--(X --> focg A) /\ |--(focf B --> Y) ==> |--(focf(nn(A \. B)) --> X \.^ Y)) /\
(!A B X. |--(X --> ff A \.^ gg B) ==> |--(X --> gg(nn(A \. B)))) /\
(!A B X Y. |--(X --> focg A) /\ |--(focf B --> Y) ==> |--(focf(nn(B /. A)) --> Y /.^ X)) /\
(!A B X. |--(X --> gg A /.^ ff B) ==> |--(X --> gg (nn(A /. B)))) /\
(!A. |-- (ff A --> focg A)) /\
(!A. |-- (focf A --> gg A))
`;;




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
let rule12 = `|--(Y \-^ Z --> X) ==> |--(Z --> Y ##^ X)`;;
let rule13 = `|--(Z --> Y ##^ X) ==> |--(Y \-^ Z --> X)`;;
let rule14 = `|--(Z /-^ X --> Y) ==> |--(Z --> Y ##^ X)`;;
let rule15 = `|--(Z --> Y ##^ X) ==> |--(Z /-^ X --> Y)`;;
let rule16 = `|--(X **^ (Y **^ <>^Z) --> W) ==> |--((X **^ Y) **^ <>^Z --> W)`;;
let rule17 = `|--((X **^ <>^Y) **^ Z --> W) ==> |--((X **^ Z) **^ <>^Y --> W)`;;
let rule18 = `|--(ff A **^ ff B --> Y) ==> |--(ff (pp(A ** B)) --> Y)`;;
let rule19 = `|--(X --> focg A) /\ |--(Y --> focg B) ==> |--(X **^ Y --> focg(pp(A ** B)))`;;
let rule20 = `|--(ff A /-^ gg B --> Y) ==> |--(ff (pp(A /- B)) --> Y)`;;
let rule21 = `|--(X --> focg A) /\ |--(focf B --> Y) ==> |--(X /-^ Y --> focg(pp(A /- B)))`;;
let rule22 = `|--(gg A \-^ ff B --> Y) ==> |--(ff (pp(A \- B)) --> Y)`;;
let rule23 = `|--(X --> focg A) /\ |--(focf B --> Y) ==> |--(Y \-^ X --> focg(pp(B \- A)))`;;
let rule24 = `|--(focf A --> X) /\ |--(focf B --> Y) ==> |--(focf(nn(A ## B)) --> X ##^ Y)`;;
let rule25 = `|--(X --> gg A ##^ gg B) ==> |--(X --> gg(nn(A ## B)))`;;
let rule26 = `|--(X --> focg A) /\ |--(focf B --> Y) ==> |--(focf(nn(A \. B)) --> X \.^ Y)`;;
let rule27 = `|--(X --> ff A \.^ gg B) ==> |--(X --> gg(nn(A \. B)))`;;
let rule28 = `|--(X --> focg A) /\ |--(focf B --> Y) ==> |--(focf(nn(B /. A)) --> Y /.^ X)`;;
let rule29 = `|--(X --> gg A /.^ ff B) ==> |--(X --> gg (nn(A /. B)))`;;
let rule30 = `(!A. |-- (ff A --> focg A))`;;
let rule31 = `(!A. |-- (focf A --> gg A))`;;

let giu = `|--(ff (pp (Y ** (nn (Y \. X)))) --> gg X)`;;
let rule2_thm = MESON[provable_RULES] rule2;;
let rule30_thm = MESON[provable_RULES] rule30;;
let rule31_thm = MESON[provable_RULES] rule31;;

g giu;;
e(MP_TAC rule2_thm);;
e(MP_TAC rule30_thm);;