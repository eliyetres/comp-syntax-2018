resource TinyRes = open Prelude in {

-- PredVP : NP -> VP -> S
-- ComplV2 : V2 -> NP -> VP
-- i_NP, youSg_NP, she_NP : NP
-- like_V2, look_for_V2 : V2


param
	Case = Nom | Acc ; 
    VForm = Pres ;
	
oper
    Verb : Type = {s : VForm => Str} ;
    
    smallVerb : (pres : Str) -> Verb
        = \pres -> { 
        s = table {
        Pres => pres 
        }
    };

    Verb2 : Type = Verb ** {c : Str} ;
    copulaVerb : Verb = smallVerb "gillar"  ;

};