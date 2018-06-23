(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option (s: string, lst) =
    case lst of
	[] => NONE
      | (hd::tl) => if same_string(hd,s)
		    then
			SOME(tl)
		    else
			case all_except_option(s,tl) of
			    NONE => NONE
			  | SOME ls => SOME(hd::ls) 

fun get_substitutions1 (sll: string list list, s: string) =
    case sll of
	[] => []
      | (hd::tl) => case all_except_option (s, hd) of
			NONE =>  get_substitutions1(tl,s)
		     |  SOME lst => lst @ get_substitutions1(tl,s)

fun get_substitutions2 (sll: string list list, s: string) =
    let fun helper(res: string list, sll: string list list) =
	    case sll of
		[] => res
	      | (hd::tl) => case all_except_option(s, hd) of
				NONE =>  helper(res,tl)
			     |  SOME lst => helper(lst@res,tl)
    in
	helper([],sll)
    end

fun similar_names (sll: string list list, name: {first:string,middle:string,last:string}) =
    case name of
	{first,middle,last} =>
	let fun helper(sub: string list, res) =
		case sub of
		    [] => res
		  | (hd::tl) =>  helper(tl, {first=hd,middle=middle,last=last} :: res)
	in
	    helper(get_substitutions2(sll,first),name::[])
	end


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color (c:card) =
    case c of
	 (Clubs,rank) => Black
	| (Spades,rank) => Black
	| (Diamonds,rank) => Red
	| (Hearts,rank) => Red

fun card_value (c:card) =
    case c of
	(suit,Ace) => 11
      | (suit,Num i) => i
      | (suit,_) => 10

fun remove_card (cs: card list, c: card,e) =
    case cs of
	[] => raise e
      | (hd::tl) => if hd=c
		    then
			tl
		    else
			hd::remove_card(tl,c,e)

fun all_same_color (cs: card list) =
    case cs of
	[] => true
      | x::[] => true
      | c1::c2::tl => if card_color(c1)=card_color(c2)
				      then
					  all_same_color(c2::tl)
				      else
					  false

fun sum_cards (cs: card list) =
    let fun helper(acc: int, card_list: card list) =
	    case card_list of
		[] => acc
	      | hd::tl => helper(acc+card_value hd, tl)
    in
	helper(0,cs)
    end
	
fun score (cs: card list, goal: int) =
    let val sum = sum_cards(cs);
	fun helper () =
	    if sum > goal
	    then
		3*(sum-goal)
	    else
		goal - sum
    in
	if all_same_color(cs)
	then
	    helper() div 2
	else
	    helper()
    end
	
fun officiate (cs: card list, ml: move list, goal: int) =
    let fun helper (held: card list, cards: card list, moves: move list) =
	    if sum_cards(held) > goal
	    then
		score(held,goal)
	    else
		case moves of
		    [] => score(held, goal)
		  | hd::tl => case hd of
				  Discard c => helper(remove_card(held,c, IllegalMove),cards, tl)
				| Draw => case cards of
					      [] => helper(held,[],[])
					    | hd_c::tl_c => helper(hd_c::held, tl_c,tl)
    in
	helper([],cs,ml)
    end
	

fun score_challenge (cs: card list, goal: int) =
    (* We calculate the origin sum as bs, then find Ace 
       and try to decrease bs by 10 each time.

       check if the decrease makes a better score.
            if true, keep recursion;
            else , which means we have found the best sum.
     *)
    let val sum_ori = sum_cards(cs);
	fun get_score (s: int) =
	    if s > goal
	    then
		3*(s-goal)
	    else
		goal - s
	
	fun sum_challenge (cards: card list, bs: int) =
	    if bs <= goal
	    then
		bs
	    else
		case cards of
		    [] => bs
		  | (_,Ace)::tl => if get_score(bs - 10) < get_score(bs)
				   then
				       sum_challenge(tl, bs - 10)
				   else
				       bs
		  | _::tl => sum_challenge(tl,bs)
    in
	if all_same_color(cs)
	then
	    get_score(sum_challenge(cs,sum_ori)) div 2
	else
	    get_score(sum_challenge(cs,sum_ori))
    end
	
fun officiate_challenge (cs: card list, ml: move list, goal: int) =
    let fun get_score (s: int) =
	    if s > goal
	    then
		3*(s-goal)
	    else
		goal - s
	fun sum_challenge (cards: card list, bs: int) =
	    if bs <= goal
	    then
		bs
	    else
		case cards of
		    [] => bs
		  | (_,Ace)::tl => if get_score(bs - 10) < get_score(bs)
				   then
				       sum_challenge(tl, bs - 10)
				   else
				       bs
		  | _::tl => sum_challenge(tl,bs)
				   
	fun helper (held: card list, cards: card list, moves: move list) =
	    if sum_challenge(held,sum_cards(held)) > goal
	    then
		score_challenge(held,goal)
	    else
		case moves of
		    [] => score_challenge(held, goal)
		  | hd::tl => case hd of
				  Discard c => helper(remove_card(held,c, IllegalMove),cards, tl)
				| Draw => case cards of
					      [] => helper(held,[],[])
					    | hd_c::tl_c => helper(hd_c::held, tl_c,tl)
    in
	helper([],cs,ml)
    end
	
				   
fun careful_player (cs: card list, goal: int) =
    let fun discard_option (helds: card list, next_card: card) =
	    let fun helper (cl: card list) =
		    case cl of
			[] => NONE
		      | hd::tl => if score(next_card::remove_card(helds,hd,IllegalMove),goal) = 0
				  then
				      SOME hd
				  else
				      helper(tl)
	    in
		helper(helds)
	    end
		
					  
	fun decision(pre_moves: move list, helds: card list, cards: card list) =
	    if score(helds,goal) = 0
	    then
		pre_moves (* stop since reach the best *)
	    else
		case cards of
		    [] => pre_moves (* no card in cs means no operation left *)
		  | c::tl => if sum_cards(helds) < goal - 10
			     then
				 decision(pre_moves @ [Draw], c::helds, tl)
					 (* draw a card in safe*)
			     else
				 case discard_option(helds, c) of
				     NONE => pre_moves (* I choose to stop *)
				   | SOME card  => decision
						       (pre_moves @ [Discard card,Draw],
							c::remove_card(helds,card,IllegalMove),
							tl) (* discard then draw *)
    in
	decision([],[],cs)
    end
	
