  --# -path=.:../../abstract

concrete MiniExtSwe of MiniExt = MiniLangSwe ** open MiniResSwe in  {

-- implementing this on top of MiniLang(Eng|Swe) is an alternative assignment
lincat

  IP, IAdv, RP, RS, Subj = {s : Str} ;

  VS, VA, VV, VQ = Verb;

  RCl = {
      subj  : Str ;      
      verb  : Verb ; 
      compl : Str    
      } ;
  
lin
  --   QuestVP   : IP -> VP -> QCl ;   -- vem ser mig
  QuestVP ip vp = {
    subj = ip.s ; -- this is always a string
    verb = vp.verb; 
    compl = vp.verb.s ! Pres ; 
  } ;

  -- QuestV2   : IP -> V2 -> QCl ;   -- vem ser jag
  QuestV2 ip v2 = {
    subj = ip.s ; -- this is always a string
    verb = v2 ;
    compl = v2.s ! Pres ;
  } ;
  
--   QuestIAdv : IAdv -> Cl -> QCl ; -- var ser hon mig
  QuestIAdv iadv cl = {
    subj = iadv.s ;
    verb = cl.verb;
    compl = cl.verb.s ! Pres ; 
  } ;

--Temp -> Pol -> RCl -> RS ;
UseRCl temp pol rcl = 
  let clt : {fin,inf : Str} = vpForm temp.isPres rcl.verb
      in {
      s = pol.s ++ temp.s ++    
	    rcl.subj ++               -- hon
	    clt.fin ++               -- (hade)
	    negation pol.isTrue ++   -- inte
	    clt.inf ++               -- druckit
	    rcl.compl                 -- öl
    
 };

--   RelVP     : RP -> VP -> RCl ;  -- that sees me
  RelVP rp vp = {
    subj = rp.s;
    verb = vp.verb ;
    compl = vp.verb.s ! Pres ; 

  } ;
  --   RelV2     : RP -> V2 -> RCl ;  -- that I see
  RelV2 rp v2 = {
    subj = rp.s;
    verb = v2 ;
    compl =  [] ;
  } ;

--   ComplVS   : VS -> S  -> VP ;  -- vet att hon ser mig
ComplVS v s = {
  verb=v;
  compl = \\_ => "att" ++ s.s ;
} ;
--   ComplVQ   : VS -> S  -> VP ;  -- wonder whom I see
ComplVQ v s = {
  verb=v;
  compl = \\_ => s.s ;
} ;

--   ComplVV   : VV -> VP -> VP ;  -- want to sleep
ComplVV v vp = {
  verb=v;
  compl = \\_ =>  vp.verb.s ! Inf ;
} ;

--   ComplVA   : VA -> AP -> VP ;  -- become red
ComplVA va ap = {
  verb = va ;
  compl = ap.s ;
 } ;

--   ReflV2    : V2 -> VP ;        -- see himself
ReflV2 v2 = {  
  verb = v2;
  compl = \\_ => v2.c ;
} ;

--   RelCN     : CN -> RS -> CN ; -- car that I see
  RelCN cn rs = {
    s = cn.s ;
    g = cn.g ;
    isSimple = cn.isSimple;
  } ;

--   PossSgDet : Pron -> Det ; -- my (car)    
PossSgDet pn = {
  s = table { _ => pn.p ! Sg } ;   -- Bool : if applied to a simple CN (relevant for den/det/de)
  n = Sg ;  
  d = Indef ; -- possessive pronouns will always be indef
  ad = Def ; -- agreement with adjective "min gula katt"
} ; 

--PossPlDet : Pron -> Det ; -- mina (bilar)
PossPlDet pn = {
  s = table { _ => pn.p ! Pl } ;   -- Bool : if applied to a simple CN (relevant for den/det/de)
  n = Pl ;  
  d = Indef ; -- possessive pronouns will always be indef
  ad = Def ; -- agreement with adjective "min gula katt"
} ; 

--   SubjS     : Subj -> S -> Adv ;  -- because she sees me
SubjS sub s = {
  s = sub.s ++ s.s;
  } ;

-- Structural words
  in_Prep = {s = "i"} ;
  on_Prep = {s = "på"} ;
  with_Prep = {s = "med"} ;

  who_IP        = {s = "vem"} ;
  what_IP       = {s = "vad"} ;

  where_IAdv    = {s = "var"} ;
  why_IAdv      = {s = "varför"} ;

  that_RP       = {s = "som"} ;

  if_Subj       = {s = "om"} ;
  because_Subj  = {s = "eftersom"} ;
  although_Subj = {s = "fastän" } ;


-- -- more tenses
--   TPastSim : Temp ; -- (slept)
  TPastSim = TAnt ; -- jag sov

--   TPastAnt : Temp ; -- had (slept)
  TPastAnt = TAnt ; -- jag har sovit
--   TCondSim : Temp ; -- would (sleep)
  TCondSim = TSim;  -- jag skulle sova
--   TCondAnt : Temp ; -- would have (slept)
  TCondAnt = TAnt ;

-- content words to test with
-- know_VS   : VS ;  -- know (that she sleeps)
  know_VS =  fullVerb "vet" "vet" "visste" "vetat" "veta" ** {c = []} ;
  -- wonder_VQ : VQ ;  -- wonder (who sleeps)
  wonder_VQ = conjIII "undra" ;
  -- want_VV   : VV ;  -- want (to sleep)
  want_VV = fullVerb "vill" "vill" "ville" "velat" "vela" ** {c = []} ;
  -- must_VV   : VV ;  -- must (sleep)
  must_VV = auxVerb "måste"  ;
  -- try_VV    : VV ;  -- try (to sleep)
  try_VV = conjII "försöka" ;
  -- become_VA : VA ;  -- become (red)
  become_VA = fullVerb "blir" "blir" "blev" "blivit" "bli" ** {c = []} ;



  
}