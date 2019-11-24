--# -path=.:../../abstract
concrete MiniGrammarIce of MiniGrammar = open MiniResIce, Prelude in {

  lincat
    Utt = {s : Str} ;
    Pol  = {s : Str ; isTrue : Bool} ; 
    Temp = {s : Str ; isPres : Bool} ;
    S  = {s : Str} ;
    QS = {s : Str} ;

    Cl, QCl = {   
      subj  : Str ;       
      verb  : Str ; sup : Str ;      
      compl : Str ; 
      n : Number ; p : Person
         } ;

    VP = {
      verb : Verb ; 
      compl : Number => Gender => Str  
    } ;     

   AP = {s : Number => Gender => Case => Declension => Str } ;
    --AP = Adjective ;
    
    CN = Noun ; 
    NP = {s : Case => Str ; p : Person ; n : Number ; g : Gender} ;
    Pron = {s : Case => Str ; p : Person ; n : Number ; g : Gender } ;
    
    -- Det = {
    --   s : Number => Case => Gender => Str ;  
    --   n : Number ;
    --   d : Species   
    -- } ;

      
    Det = {
      s : Number => Case => Gender => Str ;  
      n : Number ;
      d : Species   
    } ;

    Conj = {s : Str} ;
    Prep = {s : Str} ;

    V = Verb ; 
    V2 = Verb2 ;
    A = Adjective ;
    N = Noun ;
    PN = ProperNoun ;
    Adv = {s : Str} ;

  lin
-- Phrase
    UttS s = s ; -- Jón labbar
    UttQS s = s ; -- labbar Jón
    UttNP np = {s = np.s ! Nom} ; -- Jón (subject)
    UttAdv adv = adv ; -- í húsinu
--     UttImpSg  : Pol -> Imp -> Utt ; -- (do not) walk ----s

--Sentence
    UseCl t p cl = {
		s = cl.subj ++ case t.isPres of {
      True => cl.verb ;
      False => have_V2.s ! cl.n ! cl.p 
    } ++ case p.isTrue of {
      True => [] ; 
      False => "ekki"
    } 
    ++ case t.isPres of {
      True => [] ;
      False => cl.sup
    }
    ++ cl.compl ++ t.s ++ p.s
	 };

   UseQCl t p cl = {
		s = case t.isPres of {
      True => cl.verb ;
      False => have_V2.s ! cl.n ! cl.p 
    } ++ cl.subj ++ case p.isTrue of {
      True => [] ; 
      False => "ekki"
    } 
    ++ case t.isPres of {
      True => [] ;
      False => cl.sup
    }
    ++ cl.compl ++ t.s ++ p.s
	 };

  -- QuestCl don't see how this is relevant

    PredVP np vp = {                -- Jón labbar / Jón labbar ekki
      subj  = np.s ! Nom ;  
      verb  = vp.verb.s ! np.n ! np.p ; sup = vp.verb.sup ; 
      compl = vp.compl ! np.n ! np.g ; n = np.n ; p = np.p            
      } ;

  -- ImpVP sorry but I don't have this in the morphology and I don't have the time to add it


 -- Verb
    UseV v = {
      verb = v ;
      compl = \\_,_ => []  -- empty complement
      } ;

    ComplV2 v2 np = {
      verb = v2 ;
      compl = \\_,_ => v2.c ++ np.s ! Acc  -- NP object in the accusative, preposition first
      } ;
      
    UseAP ap = {
       verb = copulaVerb ;
       compl = \\g,n,c,d => ap.s ! g ! n ! c ! d
       } ; -- wants a table, gets a string

    UseNP np = {
      verb = copulaVerb ;
      compl = \\_,_ => np.s ! Nom    -- NP complement is in the nominative
      } ;

    UseAdv adv = {
      verb = copulaVerb ;
      compl = \\_,_ => adv.s
      } ;

    AdvVP vp adv =
      vp ** {compl = \\g,n => vp.compl ! g ! n ++ adv.s} ;

 -- Noun
  DetCN det cn = {
    s = \\_ => table {g => cn.g ++ cn.s}
    }; -- wants a table, gets a string

  UsePN pn = {
    s = \\_,n,p => pn ! n ! p }; -- wants a table, gets a string

  UsePron p = p ; 
    
  MassNP cn = {
    s = \\_,c => cn.s ! Sing ! Indef ! c ;
    a = NounSing cn.g
   } ; -- wants a table, gets a string

  a_Det = {
    s = \\_,_,d => [] ;
    n = Sing ;
    d = Indef ; 
    } ; -- wants a table, gets a string


    -- Det = {
    --   s : Number => Case => Gender => Str ;  
    --   n : Number ;
    --   d : Species   
    -- } ;


  aPl_Det = {
    s = \\_,_,g => [] ;
    n = Plur ;
    d = Indef ;
   } ; -- complains about having no gender

  the_Det= {
    s = \\_,_,g => [] ;
    n = Plur ;
    d = Def ;
    } ; -- complains about having no gender


  thePl_Det= {
    s = \\_,_,g => [] ;
    n = Plur ;
    d = Def ;
   } ; -- complains about having no gender

  UseN n = n ** {isSimple = True} ;

    AdjCN ap cn = {
      s = \\n,s,c =>
        ap.s ! n ! cn.g ! c ! (case s of {Def => Weak ; Indef => Strong}) ++
        cn.s ! n ! s ! c ;
      g = cn.g ;
      isSimple = False ;
      } ;

-- -- Adjective
    PositA a = a ;

-- -- Adverb
    PrepNP prep np = {s = prep.s ++ np.s ! Dat} ; -- í húsinu 

-- -- Conjunction
    CoordS conj a b = {s = a.s ++ conj.s ++ b.s} ;

-- -- Tense
    PPos  = {s = [] ; isTrue = True} ;
    PNeg  = {s = [] ; isTrue = False} ;

    TSim  = {s = [] ; isPres = True} ;
    TAnt  = {s = [] ; isPres = False} ;

-- -- Structural
     and_Conj = {s = "og"} ;
     or_Conj = {s = "eða"}; 
     every_Det = {
       s = \\_,_,g => "hver" ; 
       n = Sing ; 
       d = Indef
       }; -- complains about having no gender 
     in_Prep = {s = "í"} ;
     on_Prep = {s = "á"} ;
     with_Prep = {s = "með"}; 
     i_Pron = {
       s = table {Nom => "ég" ; Acc => "mig" ; Dat => "mér" ; Gen => "mín"} ;
       p = P1 ; n = Sing ; g = Masc
       } ;
     youSg_Pron = {
       s = table {Nom => "þú" ; Acc => "þig" ; Dat => "þér" ; Gen => "þín"} ;
       p = P2 ; n = Sing ; g = Masc
       } ;
     he_Pron = {
       s = table {Nom => "hann" ; Acc => "hann" ; Dat => "honum" ; Gen => "hans"} ;
       p = P3 ; n = Sing ; g = Masc
       } ;
     she_Pron = {
       s = table {Nom => "hún" ; Acc => "hana" ; Dat => "henni" ; Gen => "hennar"} ;
       p = P3 ; n = Sing ; g = Fem
       } ;
     we_Pron = {
       s = table {Nom => "við" ; Acc => "okkur" ; Dat => "okkur" ; Gen => "okkar"} ;
       p = P1 ; n = Plur ; g = Masc
       } ;
     youPl_Pron = {
       s = table {Nom => "þið" ; Acc => "ykkur" ; Dat => "ykkur" ; Gen => "ykkar"} ;
       p = P2 ; n = Plur ; g = Masc
       } ;
     they_Pron = {
       s = table {Nom => "þau" ; Acc => "þau" ; Dat => "þeim" ; Gen => "þeirra"} ;
       p = P3 ; n = Plur ; g = Neutr
       } ;
     have_V2 = mkVerb "hef" "hefur" "hefur" "höfum" "hafið" "hafa" "að hafa" ** {c = []} ;
}
