type tension
val zero : tension
val un : tension
val relie : tension -> tension -> unit
val nouvelle_tension : unit -> tension
val delai : tension -> tension
val nand : tension -> tension -> tension
val execute : tension array -> int array array -> tension list -> int list list
val affiche_sorties : int list list -> unit
val nb_bits : int
val compile : tension array -> tension array ->
              (int array  -> int array) 
