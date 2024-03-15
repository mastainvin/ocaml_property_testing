
#use "Test.ml" ;;

(* Tests de la division euclidienne                                                                          *)
(* Les tests sont effectués sur des couples d'entiers compris entre -100 et 100 dont le second est *non nul* *)
(* (d'où l'utilisation du filtre pour éviter des divisions par zéro).                                        *)
let gen_intcouple =
  let gen_dividend =                            Generator.int (-100) 100
  and gen_divisor  = Generator.filter ((<>) 0) (Generator.int (-100) 100)
    in Generator.combine gen_dividend gen_divisor ;;
let red_intcouple =
  let red_dividend =                           Reduction.int
  and red_divisor  = Reduction.filter ((<>) 0) Reduction.int
    in Reduction.combine red_dividend red_divisor ;;
let test_intcouple = Test.make_test gen_intcouple red_intcouple ;;

(* Construction des tests *)
let test_quorem       = test_intcouple "/ et mod (correct)" (fun (a, b) -> (a = (a / b) * b + (a mod b))) ;;
let test_quorem_wrong = test_intcouple "/ et mod (faux)"    (fun (a, b) -> (a = (a / b) * b - (a mod b))) ;;


(* Exécution des tests *)
Test.check    100 test_quorem       ;;
Test.check    100 test_quorem_wrong ;;
Test.fails_at 100 test_quorem       ;;
Test.fails_at 100 test_quorem_wrong ;;
Test.execute  100 [test_quorem ; test_quorem_wrong] ;;

(* Tests sur la concaténation de listes                                           *)
(* Les tests sont effectués sur des listes d'au plus dix entiers entre 0 et 10. *)
let gen_intlistcouple =
  let gen_intlist = Generator.list 10 (Generator.int_nonneg 10) in
    Generator.combine gen_intlist gen_intlist ;;
let red_intlistcouple =
  let red_intlist = Reduction.list     Reduction.int_nonneg     in
    Reduction.combine red_intlist red_intlist ;;
let test_intlistcouple = Test.make_test gen_intlistcouple red_intlistcouple ;;

(* Constructon des tests *)
let test_append       = test_intlistcouple "List.@ (correct)" (fun (l1, l2) -> List.length (l1 @ l2) = (List.length l1) + (List.length l2)) ;;
let test_append_wrong = test_intlistcouple "List.@ (faux)"    (fun (l1, l2) -> List.length (l1 @ l2) = (List.length l1) - (List.length l2)) ;;

(* Exécution des tests *)
Test.check    100 test_append       ;;
Test.check    100 test_append_wrong ;;
Test.fails_at 100 test_append       ;;
Test.fails_at 100 test_append_wrong ;;
Test.execute  100 [test_append ; test_append_wrong] ;;


(* Sur la recherche du dernier élément du liste *)

(* vrai fonction *)
let find_last l =
  let length = List.length l
  in if length > 0 then Some(List.nth l (length-1)) else None;;

(* fonction valide *)
let find_last_true l = 
  match l with 
  | [] -> None 
  | h :: t -> let rec helper l r =
    match l with 
    | h :: t -> helper t h 
    | _ -> r 
  in Some(helper l 0);;


(* fonction fausse dans les cas d'une liste d'un seul élément*)
let find_last_wrong l =
  match l with 
  | [] -> None 
  | h :: t -> let length = List.length t
  in if length > 0 then Some(List.nth t (length-1)) else None;;


let gen_intlist = Generator.list 10 (Generator.int_nonneg 10);;
let red_intlist = Reduction.list     Reduction.int_nonneg ;;

let test_intlist = Test.make_test gen_intlist red_intlist ;;


(* Constructon des tests *)
let test_sum       = test_intlist "List.@ (correct)" (fun (l1) -> find_last l1 = find_last_true l1) ;;
let test_sum_wrong = test_intlist "List.@ (faux)"    (fun (l1) -> find_last l1 = find_last_wrong l1) ;;

(* Exécution des tests *)
Test.check    100 test_sum       ;;
Test.check    100 test_sum_wrong ;;
Test.fails_at 100 test_sum       ;;
Test.fails_at 100 test_sum_wrong ;;
Test.execute  100 [test_sum ; test_sum_wrong] ;;

(* Test sur la longueur de la concaténation de chaînes de caractères (similaire à celle pour les listes) *)

let gen_stringcouple = 
  let gen_string = Generator.string 10 (Generator.char) in
  Generator.combine gen_string gen_string;;

let red_stringcouple = 
  let red_string = Reduction.string Reduction.char in 
  Reduction.combine red_string red_string;;

let test_stringcouple = Test.make_test gen_stringcouple red_stringcouple ;;

let test_concat_length       = test_stringcouple "String.^ (correct)" (fun (s1, s2) -> String.length (s1^s2) = (String.length s1) + (String.length s2)) ;;
let test_concat_length_wrong = test_stringcouple "String.^ (wrong)"   (fun (s1, s2) -> String.length (s1^s2) = (String.length s1) - (String.length s2)) ;;

Test.check 100 test_concat_length ;;
Test.check 100 test_concat_length_wrong ;;
Test.fails_at 100 test_concat_length ;;
Test.fails_at 100 test_concat_length_wrong ;;
Test.execute 100 [test_concat_length ; test_concat_length_wrong] ;;

(* test sur la concatenation de chaînes de caractères *)
(* concaténation(a,b) != concaténation(b,a) *)

let test_concat       = test_stringcouple "String.^ (correct)" (fun (s1, s2) -> (s1^s2) = String.cat s1 s2) ;;
let test_concat_wrong = test_stringcouple "String.^ (wrong)"   (fun (s1, s2) -> (s1^s2) = String.cat s2 s1) ;;

Test.check 100 test_concat ;;
Test.check 100 test_concat_wrong ;;
Test.fails_at 100 test_concat ;;
Test.fails_at 100 test_concat_wrong ;;
Test.execute 100 [test_concat ; test_concat_wrong] ;;

(* Test sur l'écriture d'une fonction (cas réel) calcul de la puissance (en utilisant la méthode rapide) *)
(* Utilisation des floats *)

(*Fonction valide *)
let rec puiss_rapide n x = 
    if n = 0 then
        1.
    else 
        puiss_rapide (n/2) (x*.x) *. if n mod 2 = 0 then 1.0 else x;; 

(* Fonction fausse *)
(* Oublie les cas impaires *)
let rec puiss_rapide_wrong n x = 
    if n = 0 then
        1.
    else 
        puiss_rapide (n/2) (x*.x);; 

let gen_floatcouple =
  let gen_float =                            Generator.float_nonneg 2. in
  let gen_int =                              Generator.int_nonneg 5
    in Generator.combine gen_float gen_int ;;

let red_floatcouple =
  let red_float =                           Reduction.float_nonneg in
  let red_int =                             Reduction.int_nonneg 
    in Reduction.combine red_float red_int ;;

let test_floatcouple = Test.make_test gen_floatcouple red_floatcouple ;;

(* utilisation de Float.to_int pour ne pas avoir de faux négatifs *)
let test_pow       = test_floatcouple "Float.pow (correct)" (fun (x, n) -> (Float.to_int ((Float.pow x (Float.of_int n))) = Float.to_int (puiss_rapide n x))) ;;
let test_pow_wrong = test_floatcouple "Float.pow (wrong)"   (fun (x, n) -> (Float.to_int ((Float.pow x (Float.of_int n))) = Float.to_int (puiss_rapide_wrong n x))) ;;

Test.check 100 test_pow ;;
Test.check 100 test_pow_wrong ;;
Test.fails_at 100 test_pow ;;
Test.fails_at 100 test_pow_wrong ;;
Test.execute 100 [test_pow ; test_pow_wrong] ;;


(* Tests taille d'une liste                                          *)
(* Les tests sont effectués sur des listes d'au plus dix entiers entre 0 et 10. *)

let gen_intlist = Generator.list 10 (Generator.int_nonneg 10) ;;

let red_intlist = Reduction.list     Reduction.int_nonneg ;;

let test_intlistcouple = Test.make_test gen_intlist red_intlist ;;

(* Constructon des tests *)
let test_length       = test_intlistcouple "List.@ (correct)" (fun l1 -> (List.length l1) >= 0) ;;
let test_length_wrong = test_intlistcouple "List.@ (faux)"    (fun l1 -> (List.length l1) <= 5) ;;

(* Exécution des tests *)
Test.check    100 test_length       ;;
Test.check    100 test_length_wrong ;;
Test.fails_at 100 test_length       ;;
Test.fails_at 100 test_length_wrong ;;
Test.execute  100 [test_length ; test_length_wrong] ;;