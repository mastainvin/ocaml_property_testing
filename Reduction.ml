module Reduction :
  sig
    (** Type d'une stratégie de réduction des éléments de type 'a
      * Une stratégie associe à chaque valeur une liste de propositions plus "simples".
      * NB : Les propositions sont ordonnées dans l'ordre croissance de "simplicité"
      *      (i.e. les valeurs les plus "simples" sont en début de liste).
      * IMPORTANT : Les stratégies implémentées respectent les conditions des générateurs correspondants.
      *)
    type 'a t = 'a -> 'a list

    (** La stratégie vide : ne renvoie aucune proposition de réduction *)
    val empty : 'a t

    (* TYPES DE BASE *)

    (** Stratégie de réduction sur les entiers
      * @param n entier
      * @return  liste d'entiers plus "simples" entre `-|n|` et `|n|`
      *)
    val int : int t

    (** Stratégie de réduction sur les entiers positifs
      * @param n entier positif
      * @return  liste d'entiers naturels plus "simples" entre 0 et `n`
      *)
    val int_nonneg : int t

    (** Stratégie de réduction sur les flottants
      * @param x flottant
      * @return  liste de flottants plus "simples" entre `-|x|` et `|x|`
      *)
    val float : float t

    (** Stratégie de réduction sur les flottants positifs
      * @param x flottant positif
      * @return  liste de flottants positifs plus "simples" entre `0` et `x`
      *)
    val float_nonneg : float t

    (** Stratégie de réduction sur les caractères
      * @param c caractère
      * @return  liste de caractères plus "simples"
      *)
    val char : char t
    (** Stratégie de réduction sur les caractères alphanumériques
      * @param c caractère alphanumérique
      * @return  liste de caractères alphanumériques plus "simples"
      *)
    val alphanum : char t

    (* CHAINES DE CARACTERES *)

    (** Stratégie de réduction sur les chaînes de caractères
      * @param red stratégie de réduction à utiliser sur chaque caractère
      * @param s   chaîne de caractères
      * @return    liste de chaînes de caractères plus "simples" au pire aussi longues que `s`
      *)
    val string : char t -> string t

    (* LISTES *)

    (** Stratégie de réduction sur les listes
      * @param red stratégie de réduction à utiliser sur chaque élément
      * @param l   liste
      * @return    liste de listes plus "simples" au pire aussi longues que `l`
      *)
    val list : 'a t -> ('a list) t

    (* TRANSFORMATIONS *)

    (** Stratégie de réduction sur les couples
      * @param fst_red stratégie de réduction de la première coordonnée
      * @param snd_red stratégie de réduction de la deuxième coordonnée
      * @return        stratégie de réduction sur les couples correspondants
      *)
    val combine : 'a t -> 'b t -> ('a * 'b) t

    (** Applique un filtre à une stratégie de réduction
      * @param p   filtre à appliquer à chaque réduction
      * @param red stratégie de réduction
      * @return    stratégie de réduction ne contenant que des propositions vérifiant `p`
      *)
    val filter : ('a -> bool) -> 'a t -> 'a t

  end =
  struct

    type 'a t = 'a -> 'a list ;;
    
    let empty = fun t -> [t] ;;
    
    let int n =
      let a = abs(n) in
      if a = 0 then [0] else
        let rec l i r = 
          if i < a then l (i+1) (if i = 0 then [0] else r@[i;-i]) else r in (l (0) []);;
        
    let int_nonneg n =
      let a = abs(n) in
      if a = 0 then [0] else
        let rec l i r = 
          if i < a then l (i+1) r@[i] else r in (l (0) []);;
        
    let float n =
      let a = abs_float(n) in
      let rec l i r = 
        if i < a then l (i+.0.5) (if i = 0. then [0.] else r@[i;-.i]) else r in (l (0.) []) ;;
        
    let float_nonneg n =
      let a = abs_float(n) in
      let rec l i r = 
        if i < a then l (i+.0.5) (if i = 0. then [0.] else r@[i]) else r in (l (0.) []) ;;
        
    let char c = 
      match c with 
      | '\000' .. '\031'  -> ['^';(Char.chr ((Char.code c)+64))] 
      | ' ' .. '@'
      | '[' .. '~' -> [c]
      | 'A' .. 'Z' -> [Char.lowercase_ascii c]
      | _ -> [] ;;
            
    let alphanum c = 
      match c with 
      | '0' .. '9' 
      | 'a' .. 'z' -> [c]
      | 'A' .. 'Z' -> [Char.lowercase_ascii c]
      | _ -> [' '] ;; 
          
    (* supprime les n premiers éléments *)
    let rec remove_to_n l n = 
      if n = 0 then l else
        match l with
        | head :: tail -> remove_to_n tail (n-1) 
        | _ -> [];;
    
    (* concaténation de listes dont on a enlevé n éléments *)
    let remove_n l n = let right = remove_to_n l n in 
      let rec remove_l l i x left right =
        let x = x@[left@right] in 
        match right with
        | head :: tail -> remove_l l (i+1) x (left@[List.nth l i]) tail  (* décale la liste enlevée *)
        | _ -> x
      in 
    remove_l l 0 [] [] right;;
    
    (* remove duplicates *)
    let cons_uniq xs x = if List.mem x xs then xs else x :: xs
    let remove_from_left xs = List.fold_left cons_uniq [] xs
    
    (** Fonction utilitaire pour combination l
      @param l1 liste de liste
      @param l2 liste
      @return combinaison de toutes les concaténations possible entre toutes les listes de l1 et les élément de l2
    *)
    let jointure_list l1 l2 = 
      List.concat_map (fun x -> (List.map (fun y -> x@[y]) l2)) l1;;
    
    (**Jointure entre n listes
      @param l listes de listes
      @return Jointure de toutes les listes de l
    *)
    let combination l = List.fold_left (fun a b -> jointure_list a b) [[]] l;;
    


    let list red l = let l_red = List.map red l in (* appliquer red sur tous les éléments de a liste *)
      let length = List.length l_red in 
      let combined = l_red in 
      let combined_length = List.length combined in 
      let rec for_combined i r = 
        if i = combined_length then
          r 
        else
          let combined_list = List.nth combined i in 
          let rec remove l n x = (*  concaténation de listes dont on a enlevé de 0 à n éléments *)
            if n > length then
              x@[]
            else
              let nx = remove_n l n in 
              remove l (n+1) x@nx in
          let combined_remove_list = remove combined_list 0 [] in 
          for_combined (i+1) r@combined_remove_list (* on applique remove sur toutes les listes de red *)
      in List.sort (fun l1 l2 -> compare (List.length l1) (List.length l2)) (remove_from_left(for_combined 0 []));; (* on tri par ordre de taille *)
    
    
    let string red s = let explode s = (* on transforme la chaine de caractère en liste de caractère *)
                        let rec exp i l =
                          if i < 0 then l else exp (i - 1) (s.[i] :: l) in
                        exp (String.length s - 1) [] in 
      let l = explode s in 
      let s_lists = list red l in (* on applique la fonction correspondant au listes *)
      let string_of_chars chars = (* on retransforme les solutions en chaines de caractères *)
        let buf = Buffer.create 16 in
        List.iter (Buffer.add_char buf) chars;
        Buffer.contents buf
      in List.map string_of_chars s_lists;;
    
    
    let jointure_couple l1 l2 = 
      List.concat_map (fun x -> (List.map (fun y -> (x,y)) l2)) l1;;
    
    let combine fst_red snd_red = fun x -> 
      let a = fst(x) and b = snd(x) in 
      let first = fst_red a and second = snd_red b 
      in (jointure_couple first second);;
    
    
    let filter p red = fun x -> let y = red x in List.filter p y;; 
    
  end ;;