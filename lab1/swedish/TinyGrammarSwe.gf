--# -path=.:../../abstract
concrete TinyGrammar of TinyGrammar = open TinyRes, Prelude in {
lincat
    Utt = {s : Str} ;    
	S = {s : Str} ; -- sentence

	NP = {s : Case => Str ; } ;
    Pron = { s : Case => Str ; } ; 

    V2 = Verb2 ;

	VP = {
	verb : Verb ;
	compl : Case => Str
    } ;
   
lin
    UttS s = s ;
    UttNP np = {s = np.s ! Nom} ; 

    --ComplV2   : V2  -> NP -> VP ;       -- love it  ---s
	ComplV2 v2 np = {
	verb = v2 ;
	compl = \\_ => v2.c ++ np.s ! Acc -- NP object in the accusative, preposition first
	} ;
	
	UseNP np = {
	verb = copulaVerb ;
	compl = \\_ => np.s ! Nom -- NP complement is in the nominative
	} ;

    PredVP np vp = {
    subj  = np.s ! Nom ;
    compl = vp.compl ! np.a ;   -- adjective in compl agrees to subject 
    verb  = vp.verb             -- verb needs no agreement to subject in Swe
    } ;

    -- UsePron   : Pron -> NP ;
    UsePron p = p ;  -- Pron is worst-case NP  
    
	i_NP = {
	s = table {Nom => "jag" ; Acc => "mig" } ;
	} ;
	youSg_NP = {
	s = table {Nom => "du" ; Acc => "dig" } ;
	} ;
	she_NP = {
	s = table {Nom => "hon" ; Acc => "henne" } ;
	} ;

    like_V2 = smallVerb "gillar" ** {c = []} ;
};