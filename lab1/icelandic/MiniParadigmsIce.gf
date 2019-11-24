--# -path=.:../../abstract
 resource MiniParadigmsIce = open

   MiniGrammarIce,
   MiniResIce
  
 in {

 oper
   mkN = overload {
     mkN : Str -> Noun   
       = \n -> lin N (smartNoun n) ;
     mkN : (n,g : Str) -> Noun  
       = \n,g -> lin N (smart2Noun n g) ;
     } ;

  mkPN : Str -> Gender -> PN
    = \s,g -> let noun = (smartNoun s) in lin PN {s = noun.s!Sing!Indef ; g=g} ;

mkA = overload {
  mkA : Str -> A
    = \s -> lin A (smartAdjective s) ;
  mkA : Str -> A
    = \s -> lin A (snjall_A s) ;
    } ;
    
  mkV = overload {
    mkV : Str -> V  -- predictable verb; better with present tense, e.g. hittar, leker
      = \s -> lin V (weakVerb s) ;
--     mkV : (inf,past,sup : Str) -> V  -- irregular verb, e.g. dricka-drack-druckit
--       = \inf,past,sup -> lin V (irregVerb inf past sup) ;
--     mkV : (inf,pres,past,sup,imp : Str) -> V
--     = \inf,pres,past,sup,imp -> lin V (fullVerb inf pres past sup imp) ;
     } ;

  mkV2 = overload {
--     mkV2 : Str -> V -> sup -> V2 
 --     = \s,v -> let verb = (mkVerb s) in lin V2 {s = verb.s!sup ** {c = []}}; 

    mkV2 : Str -> V2          -- predictable verb with direct object, e.g. "dräper"
      = \s   -> lin V2 (weakVerb s ** {c = []}) ;
--     mkV2 : Str -> Str -> V2  -- predictable verb with preposition, e.g. "titta - på"
--       = \s,p -> lin V2 (smartVerb s ** {c = p}) ;
     mkV2 : V -> V2            -- any verb with direct object, e.g. dricka_V
       = \v   -> lin V2 (v ** {c = []}) ;
--     mkV2 : V -> Str -> V2     -- any verb with preposition
--       = \v,p -> lin V2 (v ** {c = p}) ;
    } ;

   mkAdv : Str -> Adv
     = \s -> lin Adv {s = s} ;
  
   mkPrep : Str -> Prep
     = \s -> lin Prep {s = s} ;

 }