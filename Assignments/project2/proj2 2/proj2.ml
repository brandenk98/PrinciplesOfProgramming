open Proj2_types;;



(* This function returns the start symbol of g *)
let getStartSymbol (g : grammar) : string =
  (* YOUR CODE GOES HERE *)
  let (start, _) = g in start;;



(* This function returns a list containing all non-terminals symbols in g. 
    It is guaranteed that every nonterminal symbol appears as the LHS of some rule. 
    The returned list does not need to be sorted in any way.
    You do not need to remove duplicate entries.
*)
let getNonterminals (g : grammar) : string list =
  (* YOUR CODE GOES HERE *)
  let _, production = g in 
    
    let rec ntHelper prods acc = match prods with
    | [] -> acc
    | (lhs, rhs)::rest -> (ntHelper rest (lhs::acc))
    in ntHelper production [];;


(* This function build an initial, empty map from non-terminals to FIRST sets. 
    The returned symbol map should satisfy:
    1. The keys in the map are exactly the non-terminals of g.
    2. The value associated with each key is an empty set {}.
*)    
let getInitFirstSets (g : grammar) : symbolMap =
  (* YOUR CODE GOES HERE *)
 let nonTerms = getNonterminals g in
    
    let rec firstInitHelper nonTerms acc =
    match nonTerms with 
    | [] -> acc
    | nt::rest -> (firstInitHelper rest (SMap.add nt SymbolSet.empty acc))
    in firstInitHelper nonTerms SMap.empty;;



let getInitFollowSets (g : grammar) : symbolMap =
  (* YOUR CODE GOES HERE *)
 let start = (getStartSymbol g) in 
    let nonTerms = getNonterminals g in
    let s = (SymbolSet.add "eof" SymbolSet.empty) in (* add string eof to empty set *)
    let m = (SMap.add start s SMap.empty) in (* add start, s to empty Map*)
    
    let rec followInitHelper nonTerms acc =
    match nonTerms with
    |[] -> acc
    |nt::rest -> if nt = start (* current symbol = start symbol*)
        then acc (*don't overwrite the start symbol*)
        else (followInitHelper rest (SMap.add nt SymbolSet.empty acc))
    in followInitHelper nonTerms m;;







let rec computeFirstSet (first : symbolMap) (symbolSeq : string list) : SymbolSet.t =
  	match symbolSeq with
  	[] -> SymbolSet.singleton "eps"
  	| nt::acc->
  		if SMap.mem nt first then
  			let setFirst = SMap.find nt first in
  				if SymbolSet.mem "eps" setFirst
  				then SymbolSet.union(SymbolSet.remove "eps" setFirst) (computeFirstSet first acc)
  				else setFirst
  		else
  			SymbolSet.singleton nt;;

  (* YOUR CODE GOES HERE *)
 (*  This function takes a (possibly incomplete) map of FIRST sets, first, 
and a sequence of symbols, symbolSeq,
and returns the FIRST set of symbolSeq, according to the given FIRST sets. 
The sequence of symbols symbolSeq has type string list. 

For instance symbolSeq = ["A";"B"] represents the sequence AB for first set calculation.
Example:
computeFirstSet ["A" -> {"a","b"};"B" -> {"c","eps"}] ["A";"B"] -> {"a","b"}
computeFirstSet ["A" -> {"a","eps"};"B" -> {"b","eps"}] ["A";"B"] -> {"a","b","eps"}
*)
(* 2. 
Iterate until no more terminals or "eps" can be added to any FIRST(X), where X is a non-terminal:
    For each rule of the form X ::= Y where Y is a sequence of symbols, 
        add FIRST(Y ) to FIRST(X).
        let Y = Y1Y2...Yk be a sequence of symbols, FIRST(Y) can be computed:
            1. If Y = eps, 
                then FIRST(Y ) = {eps}.
            2. If Y1 is a terminal, 
                then FIRST(Y) = {Y1}.
            3. If Y1 is a non-terminal, then:
                (a) If eps in FIRST(Y1), then 
                    FIRST(Y) = (FIRST(Y1) − {eps}) ∪ FIRST(Y2...Yk)
                (b) If eps not in FIRST(Y1), then 
                    FIRST(Y) = FIRST(Y1).

    while (more terminals or eps can be added to FIRST(X)):
        for each rule x ::= Y:
            FIRST(Y) = None
            if Y = eps:
                FIRST(Y) = {eps}
            elif Y[0] is a nonterminal:
                if eps in FIRST(Y1):
                    FIRST(Y) = (FIRST(Y1) − {eps}) union FIRST(Y2...Yk) # the rest
                else:
                    FIRST(Y) = FIRST(Y1)
            else:
                FIRST(Y) = {Y1}
            
            add FIRST(Y) to FIRST(X)    
            
*)   


let recurseFirstSets (g : grammar) (first : symbolMap) firstFunc : symbolMap =
  (* YOUR CODE GOES HERE *)

  let (_,rules) = g in 
  let rec recurseHelper prods acc = match prods with
    | [] -> acc
    | (lhs, rhs)::rest -> if (SMap.mem lhs acc) = true
        then (recurseHelper rest (SMap.add lhs (SymbolSet.union (SMap.find lhs acc)(firstFunc first rhs))acc))
        else (recurseHelper rest (SMap.add lhs(firstFunc first rhs) acc))
  in recurseHelper rules SMap.empty;;

let rec getFirstSets (g : grammar) (first : symbolMap) firstFunc : symbolMap =
  (* YOUR CODE GOES HERE *)
 let temp = (recurseFirstSets g first  firstFunc) in if (SMap.equal SymbolSet.equal first temp) then first else getFirstSets g temp firstFunc;;

let rec updateFollowSet (first : symbolMap) (follow : symbolMap) (nt : string) (symbolSeq : string list) : symbolMap =
  (* YOUR CODE GOES HERE *)

  match symbolSeq with 
  [] -> follow
  | hd::[]-> if SMap.mem hd first then
  	  (SMap.add hd (SymbolSet.union (SMap.find hd follow) (SMap.find nt follow)) follow)
      else
  	  (SMap.add nt (SymbolSet.union (SymbolSet.singleton hd) (SMap.find nt follow)) follow)
  |hd::tl -> if SMap.mem hd first then 
         if SymbolSet.mem "eps" (computeFirstSet first tl) then 
             updateFollowSet first (SMap.add hd (SymbolSet.union (SymbolSet.remove "eps" (computeFirstSet first tl)) (SymbolSet.union (SMap.find hd follow) (SMap.find nt follow))) follow) nt tl
         else
             updateFollowSet first (SMap.add nt (SymbolSet.union (computeFirstSet first tl) (SMap.find hd follow)) follow) nt tl
      else updateFollowSet first follow nt tl;;


let recurseFollowSets (g : grammar) (first : symbolMap) (follow : symbolMap) followFunc : symbolMap =
  (* YOUR CODE GOES HERE *)

  let (_,rules) = g in
  let rec recurseHelper prods acc = match prods with
    | [] -> acc
    | (lhs, rhs)::rest -> if (SMap.mem lhs acc) = true
    	then (recurseHelper rest (SMap.add lhs (SymbolSet.union (SMap.find lhs acc) (SMap.find lhs (followFunc first follow lhs rhs))) acc))
    	else (recurseHelper rest (SMap.add lhs (SMap.find lhs (followFunc first follow  lhs rhs)) acc))
   in recurseHelper rules SMap.empty;;


let rec getFollowSets (g : grammar) (first : symbolMap) (follow : symbolMap) followFunc : symbolMap =
  (* YOUR CODE GOES HERE *)
    let temp = (recurseFollowSets g first follow followFunc) in 
    if (SMap.equal SymbolSet.equal follow temp) then follow else
  getFollowSets g first temp followFunc;;

let rec getPredictSets (g : grammar) (first : symbolMap) (follow : symbolMap) firstFunc : ((string * string list) * SymbolSet.t) list =
  (* YOUR CODE GOES HERE *)
[];;

let tryDerive (g : grammar) (inputStr : string list) : bool =
  (* YOUR CODE GOES HERE *)
false;;

let tryDeriveTree (g : grammar) (inputStr : string list) : parseTree =
  (* YOUR CODE GOES HERE *)
Terminal "empty";;

let genParser g = tryDerive g;;
let genTreeParser g = tryDeriveTree g;;
