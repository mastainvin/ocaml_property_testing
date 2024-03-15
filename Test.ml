#use "Property.ml" ;;
#use "Generator.ml" ;;
#use "Reduction.ml" ;;

module Test :

  sig
    (** Type d'un test portant sur des éléments de type 'a *)
    type 'a t

    (** Construit un test
      * @param gen  générateur pseudo-aléatoire de valeurs de test
      * @param red  stratégie de réduction
      * @param name nom du test
      * @param prop propriété qui fait l'objet du test
      * @return     test créé
      *)
    val make_test : 'a Generator.t -> 'a Reduction.t -> string -> 'a Property.t -> 'a t

    (** Effectue un test
      * @param n    nombre de valeurs à tester
      * @param test test à effectuer
      * @return     `true` si n > 0 et que toutes les valeurs à tester satisfont les conditions
      *)
    val check : int -> 'a t -> bool

    
    (** Cherche une valeur simple ne vérifiant pas la propriété
      * @param n nombre de valeurs à tester
      * @return  `None` si toutes les valeurs de test générées par `gen` vérifient `prop`,
                 une valeur ne vérifiant pas `prop` (éventuellement en appliquant `red`) sinon
      *)
    val fails_at : int -> 'a t -> 'a option

    
    (** Exécute plusieurs tests
      * @param n     nombre de valeurs testées par test
      * @param tests liste des tests à vérifier
      * @return      tableau associatif des résultats
      *)
    val execute : int -> ('a t) list -> ('a t * 'a option) list 
  end =
  struct

    type 'a t = Test of 'a Generator.t * 'a Reduction.t * string * 'a Property.t ;;

    let make_test gen red name prop = 
      Test(gen, red, name, prop);;

    let check n test =
      match test with Test(gen, red, name, prop) -> 
        if n <= 0 then false
        else
          let rec try_one i =
            if i = n then
              true
            else let value = Generator.next gen in (* génére un nouveau *)
            if prop value = false then 
              false
            else try_one (i+1)
          in try_one 0;;

    let fails_at n test = 
      match test with Test(gen, red, name, prop) -> 
        if n <= 0 then None
        else
      let value_fails = (* on cherche un élément qui valide pas la prop *)
          let rec try_one i =
            if i = n then
              Generator.next gen
            else let value = Generator.next gen in 
            if prop value = false then
              value
            else try_one (i+1)
          in try_one 0 in
      if prop value_fails = true then None (* si valide alors aucune réduction car valide *)
      else let values = red value_fails in (* sinon on génère une réduction *)
        let length = List.length values in
        let rec for_values_check_prop i = (* on cherche la première valeur qui valide pas la prop *)
          if i = length then value_fails
          else 
          let value = List.nth values i in 
          if prop value = false then value 
          else for_values_check_prop (i+1)
        in Some(for_values_check_prop 0);;
      
    let execute n tests = (* execute fails_at pour tous les tests de la liste *)
      let length = List.length tests in
        let rec for_tests_fails_at i r = 
          if i = length then 
            r
        else
          for_tests_fails_at (i+1) r@[(List.nth tests i, fails_at n (List.nth tests i))]
        in List.rev(for_tests_fails_at 0 []);;

  end ;;
