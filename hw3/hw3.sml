(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit 
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end



(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

fun typecheck_patterns (cl,pl) =
    let
	fun pat2typ p =
	    case p of
		ConstructorP (str,pat) => (case List.find (fn (name,_,_) => name=str) cl of
					      NONE => raise NoAnswer
					    | SOME (name,dt,_) => Datatype dt)
	      | UnitP => UnitT
	      | Variable s => Anything
	      | ConstP i => IntT
	      | TupleP plist => TupleT (map pat2typ plist)
	      | _ => Anything
	fun all_same lst =
	    case lst of
		hd::[] => SOME hd
	      | hd::hd2::tl => if hd=hd2
			       then
				   all_same (hd2::tl)
			       else
				   NONE
	      | _ => NONE
			 
    in
	all_same (map pat2typ pl)
    end
	
		

(**** you can put all your code here ****)

fun only_capitals sl =
    List.filter (fn x => Char.isUpper(String.sub(x,0))) sl

fun longest_string1 sl =
    foldl (fn (str, lstr) => if String.size str > String.size lstr then str else lstr) "" sl

fun longest_string2 sl =
    foldl (fn (str, lstr) => if String.size str >= String.size lstr then str else lstr) "" sl

fun longest_string_helper f sl str =
    case sl of
	[] => str
      | hd::tl => if f(String.size hd, String.size str)
		  then
		      longest_string_helper f tl hd
		  else
		      longest_string_helper f tl str

val longest_string3 = fn x => longest_string_helper (fn (a,b)=> a > b) x "";
val longest_string4 = fn x => longest_string_helper (fn (a,b)=> a >= b) x "";

val longest_capitalized = longest_string1 o only_capitals;

fun rev_string str =
    (String.implode o List.rev o String.explode) str


fun first_answer f questions =
    case questions of
	[] => raise NoAnswer
      | hd::tl => case f hd of
		      NONE => first_answer f tl
		    | SOME v => v

fun all_answers f questions =
    let fun helper (acc,lst) =
	    case lst of
		[] => SOME acc
	      | hd::tl => case f hd of
			      NONE => helper(acc,tl)
			    | SOME hdl => helper(hdl@acc,tl)
    in
	case questions of
	    [] => SOME []
	  | _ => case helper ([],questions) of
		     SOME [] => NONE
		  | _ =>  helper ([],questions)
	    
    end
	



fun count_wildcards p =
    g (fn _ => 1) (fn x => 0) p

fun count_wild_and_variable_lengths p =
    g (fn _ => 1) String.size p

fun count_some_var (s,p) =
    g (fn _ => 0) (fn x => if x=s then 1 else 0) p

fun check_pat p =
    let
	fun helper1 p =
	    case p of
		TupleP pl => foldl (fn (pat,sl) => (helper1 pat) @ sl) [] pl
	      | ConstructorP (name,pat) => helper1 pat
	      | Variable s => [s]
	      | _ => []

	fun helper2 sl =
	    case sl of
		[] => true
	      | hd::tl => (not (List.exists (fn x => hd=x) tl)) andalso (helper2 tl)
    in
	(helper2 o helper1) p
    end

fun match (v,p) =
    case p of
	Wildcard => SOME []
      | Variable s => SOME [(s,v)]
      | UnitP => (case v of
		     Unit => SOME []
		   | _ => NONE)
      | ConstP i => (case v of
			 Const iv => if iv = i then SOME [] else NONE
		       | _ => NONE)
      | TupleP ps => (case v of
			  Tuple vs => if length ps = length vs
				      then
					  all_answers match (ListPair.zip(vs,ps))
				      else
					  NONE
			| _ => NONE
		     )
      | ConstructorP (s1,p) => (case v of
				    Constructor(s2,v) => if s1=s2
							 then
							     match(v,p)
							 else
							     NONE
				  | _ => NONE)
				   
fun first_match v pl =
    first_answer (fn p => case match (v,p) of
			      NONE => NONE
			   | SOME lst => SOME (SOME lst)) pl handle NoAnswer => NONE
