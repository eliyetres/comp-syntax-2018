--------------Abstract------------
-- (The abstract syntax defines what meanings can be expressed in the grammar)
abstract Hello = {

--category declarations introducing two categories, i.e. types of meanings
cat 
    Utt; -- utterance
    S; --sentence
    V; --verb
	
-- function declarations introducing three meaning-building functions
fun 
  UttS : S -> Utt; 
  S : S -> s ;
} ;

------------Resource------------
resource HelloRes = open Prelude in { --  this module uses the contents of the Prelude module without inheriting them
param
  Number = Sg | Pl ;
  Gender = Masc | Fem ;

oper
  Noun : Type = {s : Number => Str} ;
  Adjective : Type = {s : Str} ;
  Verb : Type = {s : VForm => Str} ;

} ;
--------------Concrete------------
concrete HelloLang of Hello = {
-- The mapping is defined by lincat definitions assigning a linearization type to each category,
lincat 
    V = Verb;
    AP = Adjective;
    N = Noun ;

    Utt = {s : Str} ;
    S  = {s : Str} ;
    VP = {
      verb : Verb ;
      compl : AForm => Str
      } ;

-- lin definitions assigning a linearization to each function.
lin
    UttS s = s ;
    UseV v = {
      verb = v ;
      compl = \\_ => []  -- empty complement
    } ;

};
