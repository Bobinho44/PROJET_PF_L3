(***                                        
        PROJET PROGRAMMATION FONCTIONNELLE: 
             Les machines à registres       
                                            
                                            
DELAGE Alix - GERARD Kylian                 
                                        ***)
(* 
   Définition du type incrément:
   - i_r: indice du registre à modifier
   - i_instr_suiv: indice de l'instruction suivante
*)
type increment = {i_r : int; i_instr_suiv : int};;

(* 
   Définition du type décrément:
   - i_r: indice du registre à modifier
   - i_instr_suiv: indice de l'instruction suivante
   - i_instr_suiv_si_r_vide: Décrément: décrémenter le ième registre et aller à l'instruction i; ou si le ième registre est vide aller à l'instruction j
*)
type decrement = {i_r : int; i_instr_suiv : int; i_instr_suiv_si_r_vide : int};;

(* 
   Définition du type instruction:
   - Stop: arrêt du programme
   - Incrément: incrémenter le ième registre et aller à l'instruction i
   - Décrément: décrémenter le ième registre et aller à l'instruction i; ou si le ième registre est vide aller à l'instruction j
*)
type instruction = Stop | Increment of increment | Decrement of decrement;;

(* 
   Définition du type registres:
   - liste d'entier (valeur des registres)
*)
type registres = Registres of int list;;

(* 
   Définition du type programme:
   - liste d'instruction (chaque instruction est indicé par sa position dans la liste)
*)
type programme = Programme of instruction list;;

(* 
   Récupérer les valeur contenues dans les différents registres sous la forme d'une liste
   - r: les registres
   retourne: une liste contenant les valeur des différents registres
*)
let registres_to_list r = match r with
	| Registres(registre) -> registre
;;

(* 
   Récupérer les instructions d'un programme sous la forme d'une liste
   - p: le programme
   retourne: une liste contenant les instructions du programme
*)
let programme_to_list p = match p with
	| Programme(programme) -> programme
;;

(* 
   Modifier le ième registre (utilisé par les fonction increment et decrement)
   - r: les registres actuels
   - i: l'indice du registre à incrémenter
   - f: la fonction donnant la nouvelle valeur du registre
   retourne: les nouveaux registres dont le ième registre a été modifié
*)
let modifier_registre r i f =
	let rec increment_aux r_liste i nouveau_r_liste acc = match r_liste with
		| [] -> Registres(List.rev nouveau_r_liste)
		| head::tail -> increment_aux tail i ((if acc = i then (f head) else head)::nouveau_r_liste) (acc+1)
	in increment_aux (registres_to_list r) i [] 0
;;

(* 
   Incrémenter le ième registre
   - r: les registres actuels
   - i: l'indice du registre à incrémenter
   retourne: les nouveaux registres dont le ième registre a été incrémenté
*)
let increment r i = modifier_registre r i (func x -> x+1);;

(* 
   Décrémenter le ième registre
   - r: les registres actuels
   - i: l'indice du registre à décrémenter
   retourne: les nouveaux registres dont le ième registre a été décrémenté
*)
let decrement r i = modifier_registre r i (func x -> x-1);;

(* 
   Vérifie si le ième registre de la machine est nul (0)
   - r: les registres actuels
   - i: l'indice du registre à tester
   retourne: un booléen indiquant si le ième registre est nul
*)	
let est_vide r i = List.nth (registres_to_list r) i = 0;;

(* 
   Affichage du contenu des différents registres sous la forme de chaîne de leurs contenue ([4][1][0]...)
   - r: les registres actuels
*)
let print_registres r = 
	let rec print_registres_aux r chaine = match r with
		| [] -> print_string chaine
		| head::tail -> print_registres_aux tail (chaine^"["^(string_of_int head)^"]")
	in print_registres_aux (registres_to_list r) ""
;;

(* 
   Affichage du programme sous la forme d'une chaîne des indices des instructions ([0][1][2]...)
   - p: le programme
*)
let print_programme p = 
	let rec print_registres_aux acc chaine = match acc with
		| 0 -> print_string chaine
		| _ -> print_registres_aux (acc-1) ("["^(string_of_int (acc-1))^"]"^chaine)
	in print_registres_aux (List.length (programme_to_list p)) ""
;;

(* 
   Affichage de l'index ("^") indiquant qu'elle sera la prochaine instruction à être executé
   - i: l'indice de la future instruction à executer
*)
let print_index i = 
	let rec print_index_aux acc chaine = 
		if acc = 0 then print_string (chaine^"^")
		else print_index_aux (acc-1) (chaine^" ")
	in print_index_aux (23+(i)*3+(if i > 9 then 1 else 0)*(i-10)) ""
;;

(* 
   Affichage de l'état de la machine (registre et la future instruction a executer)
   - r: les registres actuels
   - p: le programme
   - i: l'indice de la future instruction à executer
*)
let affichage_machine r p i =
	print_string ("Etat des registres:   ");
	print_registres r;
	print_newline ();
	print_string ("Instruction en cours: ");
	print_programme p;
	print_newline ();
	print_index i;
	print_newline ();
	print_string ("___________________");
	print_newline ()
;;
	
(* 
   Exécution d'un programme avec la machine à registres
   - r: les registres initiaux
   - p: le programme
   retourne: le registre modifié par le programme
*)
let executer r p = 
	let rec executer_aux r p i = 
		affichage_machine r p i;
		match (List.nth (programme_to_list p) i) with
			| Stop -> r
			| Increment({i_r = i_r_affecte; i_instr_suiv = i_suivant}) -> executer_aux (increment r i_r_affecte) p i_suivant
			| Decrement({i_r = i_r_affecte; i_instr_suiv = i_suivant; i_instr_suiv_si_r_vide = i_si_vide}) ->
			if (est_vide r i_r_affecte) then executer_aux r p i_si_vide
			else executer_aux (decrement r i_r_affecte) p i_suivant
	in executer_aux r p 0
;;


(***                      
    Multiplication a*b    
                      ***)

(* 
   Définition des instructions du programme
*)
let instr_m0 = Decrement({i_r = 0; i_instr_suiv = 1; i_instr_suiv_si_r_vide = 2});;
let instr_m1 = Decrement({i_r = 0; i_instr_suiv = 3; i_instr_suiv_si_r_vide = 8});;
let instr_m2 = Decrement({i_r = 1; i_instr_suiv = 2; i_instr_suiv_si_r_vide = 8});;
let instr_m3 = Decrement({i_r = 2; i_instr_suiv = 4; i_instr_suiv_si_r_vide = 6});;
let instr_m4 = Increment({i_r = 3; i_instr_suiv = 5});;
let instr_m5 = Increment({i_r = 1; i_instr_suiv = 3});;
let instr_m6 = Decrement({i_r = 3; i_instr_suiv = 7; i_instr_suiv_si_r_vide = 1});;
let instr_m7 = Increment({i_r = 2; i_instr_suiv = 6});;
let instr_m8 = Stop;;

(* 
   Création du programme de calcul de a*b (suite d'instruction)
*)
let programme_multiplication = Programme [instr_m0;instr_m1;instr_m2;instr_m3;instr_m4;instr_m5;instr_m6;instr_m7;instr_m8];;

(* 
   Calcul de a*b avec la machine à registres
   Interprétation du résultat via les valeurs des registres (le résultat est stocké dans le 2ème regisrre (d'indice 1)
   Affichage des différentes étapes du processus et du résultat
   - a: premier terme
   - b: second terme
*)
let multiplication a b = 
	print_string ("========== Multiplication ==========");
	print_newline ();
	print_newline ();
	print_string ("\nRésultat : "^(string_of_int a)^"x"^(string_of_int b)^" = "^(string_of_int (List.nth (registres_to_list (executer (Registres [a;b;b;0]) programme_multiplication)) 1;)));
    	print_newline ();
    	print_newline ();
    	print_string ("=====================================");
    	print_newline ()
;;	


(***                 
    Puissance a^b    
                 ***)

(* 
   Définition des instructions du programme
*)
let instr_p0 = Decrement({i_r = 3; i_instr_suiv = 1; i_instr_suiv_si_r_vide = 9});;
let instr_p1 = Decrement({i_r = 3; i_instr_suiv = 2; i_instr_suiv_si_r_vide = 23});;
let instr_p2 = Decrement({i_r = 0; i_instr_suiv = 3; i_instr_suiv_si_r_vide = 10});;
let instr_p3 = Increment({i_r = 5; i_instr_suiv = 4});;
let instr_p4 = Decrement({i_r = 0; i_instr_suiv = 5; i_instr_suiv_si_r_vide = 14});;
let instr_p5 = Increment({i_r = 5; i_instr_suiv = 6});;
let instr_p6 = Decrement({i_r = 2; i_instr_suiv = 7; i_instr_suiv_si_r_vide = 11});;
let instr_p7 = Increment({i_r = 4; i_instr_suiv = 8});;
let instr_p8 = Increment({i_r = 1; i_instr_suiv = 6});;
let instr_p9 = Decrement({i_r = 1; i_instr_suiv = 9; i_instr_suiv_si_r_vide = 13});;
let instr_p10 = Decrement({i_r = 1; i_instr_suiv = 10; i_instr_suiv_si_r_vide = 23});;
let instr_p11 = Decrement({i_r = 4; i_instr_suiv = 12; i_instr_suiv_si_r_vide = 4});;
let instr_p12 = Increment({i_r = 2; i_instr_suiv = 11});;
let instr_p13 = Increment({i_r = 1; i_instr_suiv = 23});;
let instr_p14 = Decrement({i_r = 3; i_instr_suiv = 15; i_instr_suiv_si_r_vide = 23});;
let instr_p15 = Decrement({i_r = 5; i_instr_suiv = 16; i_instr_suiv_si_r_vide = 17});;
let instr_p16 = Increment({i_r = 0; i_instr_suiv = 15});;
let instr_p17 = Decrement({i_r = 1; i_instr_suiv = 18; i_instr_suiv_si_r_vide = 19});;
let instr_p18 = Increment({i_r = 5; i_instr_suiv = 17});;
let instr_p19 = Decrement({i_r = 2; i_instr_suiv = 19; i_instr_suiv_si_r_vide = 20});;
let instr_p20 = Decrement({i_r = 5; i_instr_suiv = 21; i_instr_suiv_si_r_vide = 2});;
let instr_p21 = Increment({i_r = 1; i_instr_suiv = 22});;
let instr_p22 = Increment({i_r = 2; i_instr_suiv = 20});;
let instr_p23 = Stop;;

(* 
   Création du programme de calcul de a^b (suite d'instruction)
*)
let programme_puissance = Programme [instr_p0;instr_p1;instr_p2;instr_p3;instr_p4;instr_p5;instr_p6;instr_p7;instr_p8;instr_p9;instr_p10;instr_p11;instr_p12;instr_p13;instr_p14;instr_p15;instr_p16;instr_p17;instr_p18;instr_p19;instr_p20;instr_p21;instr_p22;instr_p23];;

(* 
   Calcul de a^b avec la machine à registres
   Interprétation du résultat via les valeurs des registres (le résultat est stocké dans le 2ème regisrre (d'indice 1)
   Affichage des différentes étapes du processus et du résultat
   - a: base
   - b: exposant
*)
let puissance a b = 
	print_string ("============= Puissance =============");
	print_newline ();
	print_newline ();
	print_string ("\nRésultat : "^(string_of_int a)^"^"^(string_of_int b)^" = "^(string_of_int (List.nth (registres_to_list (executer (Registres [a;a;a;b;0;0]) programme_puissance)) 1;)));
    	print_newline ();
    	print_newline ();
    	print_string ("=====================================");
    	print_newline ()
;;
