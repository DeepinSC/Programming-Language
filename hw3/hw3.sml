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

fun comp2typ (t1, t2) =
	    case (t1,t2) of
		(Anything,_) => t2
	      | (_,Anything) => t1
	      | (TupleT l1, TupleT l2) => if length l1 <> length l2
					  then
					      Datatype "error"
					  else
					      TupleT (map comp2typ (ListPair.zip(l1,l2)))
	      | _ => if t1 = t2 then t1 else Datatype "error"
			       
fun pat2typ cl p  =
	    case p of
		ConstructorP (str,pat) =>
		(case List.find (fn (name,dt,typ) => name=str andalso comp2typ (pat2typ cl pat,typ) <> (Datatype "error")) cl of
					      NONE => Datatype "error"
					    | SOME (name,dt,_) => Datatype dt)
	      | UnitP => UnitT
	      | Variable s => Anything
	      | ConstP i => IntT
	      | TupleP plist => TupleT (map (pat2typ cl) plist )
	      | _ => Anything


			 
fun typecheck_patterns (cl,pl) =
    let
	
						  
	fun all_same lst =
	    case lst of
		hd::[] => SOME hd
	      | hd::hd2::tl => all_same(comp2typ(hd,hd2)::tl)				  
	      | _ => NONE
			 
    in
	case all_same (map (pat2typ cl) pl) of
	    SOME p => (case p of
			   Datatype "error" => NONE
			 | _ => SOME p)
	 | _ => NONE
		     
    end
	
		

(**** you can put all your code here ****)

fun only_capitals sl =
    List.filter (fn x => Char.isUpper(String.sub(x,0))) sl

fun longest_string1 sl =
    foldl (fn (str, lstr) => if String.size str > String.size lstr then str else lstr) "" sl

fun longest_string2 sl =
    foldl (fn (str, lstr) => if String.size str >= String.size lstr then str else lstr) "" sl

fun longest_string_helper f = 
    List.foldl (fn (s,sofar) => if f(String.size s,String.size sofar)
				then s
				else sofar) 
	                              ""
val longest_string3 = longest_string_helper op> ;
val longest_string4 = longest_string_helper op>= ;

val longest_capitalized = longest_string1 o only_capitals;

fun rev_string str =
    (String.implode o List.rev o String.explode) str


fun first_answer f questions =
    case questions of
	[] => raise NoAnswer
      | hd::tl => case f hd of
		      NONE => first_answer f tl
		    | SOME v => v
	
fun all_answers f xs =
    let fun loop (acc,xs) =
            case xs of
		[] => SOME acc
	      | x::xs' => case f x of
			      NONE => NONE
			    | SOME y => loop((y @ acc), xs')
    in
	loop ([],xs)
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
    case (v,p) of
	(_,Wildcard)    => SOME []
      | (_,Variable(s)) => SOME [(s,v)]
      | (Unit,UnitP)    => SOME []
      | (Const i, ConstP j)    => if i=j then SOME [] else NONE
      | (Tuple(vs),TupleP(ps)) => if length vs = length ps
				  then all_answers match (ListPair.zip(vs,ps))
				  else NONE
      | (Constructor(s1,v), ConstructorP(s2,p)) => if s1=s2
						   then match(v,p)
                                                   else NONE
      | _ => NONE
				   
fun first_match v pl =
    SOME (first_answer (fn pat => match (v,pat)) pl)
    handle NoAnswer => NONE
