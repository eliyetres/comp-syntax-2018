--# -path=.:../../abstract
resource MiniResIce = open Prelude, Predef in {

param
  Number = Sing | Plur ;
  Case = Nom | Acc | Dat | Gen ;
  Person = P1 | P2 | P3 ;
  Gender = Masc | Fem | Neutr ;
  Species = Def | Indef ;
	Declension = Weak | Strong ;

  Agr = Ag Gender Number Person ; 

  NounAgreement = NounSing Number Case Gender Species | NounPlur Number Case Gender Species ;
  --VerbForm = Pres Number Person | Sup Number Person | Inf Number Person ;

oper
  ProperNoun : Type = {s : Case => Str ; g: Gender} ; 
  Noun : Type = {s : Number => Species => Case => Str ; g : Gender} ;

  mkNoun : (SgIndefNom,SgIndefAcc,SgIndefDat,SgIndefGen,PlIndefNom,PlIndefAcc,PlIndefDat,PlIndefGen,SgDefNom,SgDefAcc,SgDefDat,SgDefGen,PlDefNom,PlDefAcc,PlDefDat,PlDefGen : Str) -> Gender -> Noun =
    \ SgIndefNom,SgIndefAcc,SgIndefDat,SgIndefGen,PlIndefNom,PlIndefAcc,PlIndefDat,PlIndefGen,SgDefNom,SgDefAcc,SgDefDat,SgDefGen,PlDefNom,PlDefAcc,PlDefDat,PlDefGen,g -> { 
      s = table {
        Sing => table {Indef => table { Nom => SgIndefNom ;
                                        Acc => SgIndefAcc ; 
                                        Dat => SgIndefDat ; 
                                        Gen => SgIndefGen  
                                      } ;   
                      Def => table { Nom => SgDefNom ;
                                      Acc => SgDefAcc ;
                                      Dat => SgDefDat ;
                                      Gen => SgDefGen 
                                    } 
                      } ;
        Plur => table {Indef => table { Nom => PlIndefNom ;
                                        Acc => PlIndefAcc ; 
                                        Dat => PlIndefDat ;
                                        Gen => PlIndefGen 
                                      } ; 
                      Def => table    { Nom => PlDefNom ; 
                                        Acc => PlDefGen ; 
                                        Dat => PlDefDat ; 
                                        Gen => PlDefGen 
                                      }                          
                      } 
      } ;
      g = g
    };


    strong_masc1 : Str -> Noun = \hestur ->
      mkNoun hestur (tk 2 hestur) (tk 2 hestur + "i") (tk 2 hestur + "s")
            (hestur + "inn") (tk 2 hestur + "inn") (tk 2 hestur + "inum") (tk 2 hestur + "sins")
            (tk 2 hestur + "ar") (tk 2 hestur + "a") (tk 1 hestur + "m") (tk 2 hestur + "a")
            (tk 2 hestur + "arnir") (tk 2 hestur + "ana") (tk 1 hestur + "unum") (tk 2 hestur + "anna") Masc ;

    weak_fem : Str -> Noun = \tolva ->
      mkNoun tolva (init tolva + "u") (init tolva + "u") (init tolva + "u")
            (tolva + "n") (init tolva + "una") (init tolva + "unni") (init tolva + "unnar")
            (init tolva + "ur") (init tolva + "ur") (init tolva + "um") (init tolva + "a")
            (init tolva + "urnar") (init tolva + "urnar") (init tolva + "unum") (init tolva + "anna") Fem ;

    strong_fem : Str -> Noun = \borg ->
      mkNoun borg borg borg (borg + "ar")
            (borg + "in") (borg + "ina") (borg + "inni") (borg + "arinnar")
            (borg + "ir") (borg + "ir") (borg + "um") (borg + "a")
            (borg + "irnar") (borg + "irnar") (borg + "unum") (borg + "anna") Fem ;
  
    strong_neut : Str -> Noun = \dyr ->
      mkNoun dyr dyr (dyr + "i") (dyr + "s")
            (dyr + "id") (dyr + "id") (dyr + "inu") (dyr + "sins")
            dyr dyr (dyr + "um") (dyr + "a")
            (dyr + "in") (dyr + "in") (dyr + "unum") (dyr + "anna") Neutr ;

    
    mjolk_N = mkNoun "mjolk" "mjolk" "mjolk" "mjolkur" nonExist nonExist nonExist nonExist
                      "mjolkin" "mjolkina" "mjolkinni" "mjolkurinnar" nonExist nonExist nonExist nonExist;
    stelpa_N = mkNoun "stelpa" "stelpu" "stelpu" "stelpu" "stelpur" "stelpur" "stelpum" "stelpna"
                      "stelpan" "stelpuna" "stelpunni" "stelpunnar" "stelpurnar" "stelpunum" "stelpunum" "stelpnanna" ;
    vinur_N = mkNoun "vinur" "vin" "vini" "vinar" "vinir" "vini" "vinum" "vina"
                     "vinurinn" "vininum" "vininum" "vinarins" "vinirnir" "vinunum" "vinunum" "vinanna" ;
    bill_N = mkNoun "bill" "bil" "bil" "bils" "bilar" "bila" "bilum" "bila"
                    "billinn" "bilinn" "bilnum" "bilsins" "bilarnir" "bilana" "bilunum" "bilanna" ;
    bjor_N = mkNoun "bjor" "bjor" "bjor" "bjors" "bjorar" "bjora" "bjorum" "bjora"
                    "bjorinn" "bjorinn" "bjornum" "bjorsins" "bjorarnir" "bjorana" "bjorunum" "bjoranna" ; 
    barn_N = mkNoun "barn" "barn" "barni" "barns" "born" "born" "bornum" "barna"
                    "barnid" "barnid" "barninu" "barnsins" "bornin" "bornin" "bornunum" "barnanna" ;
    smabarn_N = mkNoun "smabarn" "smabarn" "smabarni" "smabarns" "smaborn" "smaborn" "smabornum" "smabarna"
                      "smabarnid" "smabarnid" "smabarninu" "smabarnsins" "smabornin" "smabornin" "smabornunum" "smabarnanna" ;
    sjor_N = mkNoun "sjor" "sjo" "sjo" "sjavar" nonExist nonExist nonExist nonExist
                    "sjorinn" "sjoinn" "sjonum" "sjavarins" nonExist nonExist nonExist nonExist ;
    tre_N = mkNoun "tre" "tre" "tre" "tres" "tre" "tre" "trjam" "trjaa"
                    "tred" "tred" "trenu" "tresins" "trein" "trein" "trjanum" "trjanna" ;
    vatn_N = mkNoun "vatn" "vatn" "vatni" "vatns" "votn" "votn" "votnum" "vatna"
                    "vatnid" "vatnid" "vatninu" "vatnsins" "votnin" "votnin" "votnunum" "vatnanna" ;
    bok_N = mkNoun "bok" "bok" "bok" "bokar" "baekur" "baekur" "bokum" "boka"
                   "bokin" "bokina" "bokinni" "bokarinnar" "baekurnar" "baekurnar" "bokunum" "bokanna" ;
    stjarna_N = mkNoun "stjarna" "stjornu" "stjornu" "stjornu" "stjornur" "stjornur" "stjornum" "stjarna"
                       "stjarnan" "stjornuna" "stjornunni" "stjornunni" "stjornurnar" "stjornurnar" "stjornunum" "stjarnanna" ;
    kona_N = mkNoun "kona" "konu" "konu" "konu" "konur" "konur" "konum" "kvenna"
                    "konan" "konuna" "konunni" "konunnar" "konurnar" "konurnar" "konunum" "kvennanna" ;
    kottur_N = mkNoun "kottur" "kott" "ketti" "kattar" "kettir" "ketti" "kottum" "katta"
                      "kotturinn" "kottinn" "kettinum" "kattarins" "kettirnir" "kettina" "kottunum" "kattanna" ;
    kyr_N = mkNoun "kyr" "ku" "ku" "kyr" "kyr" "kyr" "kum" "kua"
                   "kyrin" "kuna" "kunni" "kyrinnar" "kyrnar" "kyrnar" "kunum" "kunna" ;
    madur_N = mkNoun "madur" "mann" "manni" "manns" "menn" "menn" "monnum" "manna"
                    "madurinn" "manninn" "manninum" "mannsins" "mennirnir" "mennina" "monnunum" "mannanna" ; 


  smartNoun : Str -> Noun = \sg -> case sg of {
     _ + "ur" => strong_masc1 sg ;
     _ + "a" => weak_fem sg 
     } ;

  smart2Noun : (_,_ : Str) -> Noun = \nom,gen -> case <nom,gen> of {
    <_, _+"ar"> => strong_fem nom ;
    <_, _+"s"> => strong_neut nom 
    } ;

 

  Adjective : Type = {s : Number => Gender => Case => Declension => Str} ;

   mkAdjective : (StrongSgMascNom3p,WeakSgMascNom3p,StrongSgMascAcc3p,WeakSgMascAcc3p,StrongSgMascDat3p,WeakSgMascDat3p,StrongSgMascGen3p,WeakSgMascGen3p,StrongSgFemNom3p,WeakSgFemNom3p,StrongSgFemAcc3p,WeakSgFemAcc3p,StrongSgFemDat3p,WeakSgFemDat3p,StrongSgFemGen3p,WeakSgFemGen3p,StrongSgNeutrNom3p,WeakSgNeutrNom3p,StrongSgNeutrAcc3p,WeakSgNeutrAcc3p,StrongSgNeutrDat3p,WeakSgNeutrDat3p,StrongSgNeutrGen3p,WeakSgNeutrGen3p,StrongPlMascNom3p,WeakPlMascNom3p,StrongPlMascAcc3p,WeakPlMascAcc3p,StrongPlMascDat3p,WeakPlMascDat3p,StrongPlMascGen3p,WeakPlMascGen3p,StrongPlFemNom3p,WeakPlFemNom3p,StrongPlFemAcc3p,WeakPlFemAcc3p,StrongPlFemDat3p,WeakPlFemDat3p,StrongPlFemGen3p,WeakPlFemGen3p,StrongPlNeutrNom3p,WeakPlNeutrNom3p,StrongPlNeutrAcc3p,WeakPlNeutrAcc3p,StrongPlNeutrDat3p,WeakPlNeutrDat3p,StrongPlNeutrGen3p,WeakPlNeutrGen3p : Str) -> Adjective =
    \ StrongSgMascNom3p,WeakSgMascNom3p,StrongSgMascAcc3p,WeakSgMascAcc3p,StrongSgMascDat3p,WeakSgMascDat3p,StrongSgMascGen3p,WeakSgMascGen3p,StrongSgFemNom3p,WeakSgFemNom3p,StrongSgFemAcc3p,WeakSgFemAcc3p,StrongSgFemDat3p,WeakSgFemDat3p,StrongSgFemGen3p,WeakSgFemGen3p,StrongSgNeutrNom3p,WeakSgNeutrNom3p,StrongSgNeutrAcc3p,WeakSgNeutrAcc3p,StrongSgNeutrDat3p,WeakSgNeutrDat3p,StrongSgNeutrGen3p,WeakSgNeutrGen3p,StrongPlMascNom3p,WeakPlMascNom3p,StrongPlMascAcc3p,WeakPlMascAcc3p,StrongPlMascDat3p,WeakPlMascDat3p,StrongPlMascGen3p,WeakPlMascGen3p,StrongPlFemNom3p,WeakPlFemNom3p,StrongPlFemAcc3p,WeakPlFemAcc3p,StrongPlFemDat3p,WeakPlFemDat3p,StrongPlFemGen3p,WeakPlFemGen3p,StrongPlNeutrNom3p,WeakPlNeutrNom3p,StrongPlNeutrAcc3p,WeakPlNeutrAcc3p,StrongPlNeutrDat3p,WeakPlNeutrDat3p,StrongPlNeutrGen3p,WeakPlNeutrGen3p -> {
      s = table {
        Sing => table {
          Masc => table {
            Nom => table {Strong => StrongSgMascNom3p ; Weak => WeakSgMascNom3p} ;
            Acc => table {Strong => StrongSgMascAcc3p ; Weak => WeakSgMascAcc3p} ;
            Dat => table {Strong => StrongSgMascDat3p ; Weak => WeakSgMascDat3p} ;
            Gen => table {Strong => StrongSgMascGen3p ; Weak => WeakSgMascGen3p}
          };
          Fem => table {
            Nom => table {Strong => StrongSgFemNom3p ; Weak => WeakSgFemNom3p} ;
            Acc => table {Strong => StrongSgFemAcc3p ; Weak => WeakSgFemAcc3p} ;
            Dat => table {Strong => StrongSgFemDat3p ; Weak => WeakSgFemDat3p} ;
            Gen => table {Strong => StrongSgFemGen3p ; Weak => WeakSgFemGen3p}
          };
          Neutr => table {
            Nom => table {Strong => StrongSgNeutrNom3p ; Weak => WeakSgNeutrNom3p} ;
            Acc => table {Strong => StrongSgNeutrAcc3p ; Weak => WeakSgNeutrAcc3p} ;
            Dat => table {Strong => StrongSgNeutrDat3p ; Weak => WeakSgNeutrDat3p} ;
            Gen => table {Strong => StrongSgNeutrGen3p ; Weak => WeakSgNeutrGen3p}
          }
        };
        Plur => table {
          Masc => table {
            Nom => table {Strong => StrongPlMascNom3p ; Weak => WeakPlMascNom3p} ;
            Acc => table {Strong => StrongPlMascAcc3p ; Weak => WeakPlMascAcc3p} ;
            Dat => table {Strong => StrongPlMascDat3p ; Weak => WeakPlMascDat3p} ;
            Gen => table {Strong => StrongPlMascGen3p ; Weak => WeakPlMascGen3p}
          };
          Fem => table {
            Nom => table {Strong => StrongPlFemNom3p ; Weak => WeakPlFemNom3p} ;
            Acc => table {Strong => StrongPlFemAcc3p ; Weak => WeakPlFemAcc3p} ;
            Dat => table {Strong => StrongPlFemDat3p ; Weak => WeakPlFemDat3p} ;
            Gen => table {Strong => StrongPlFemGen3p ; Weak => WeakPlFemGen3p}
          };
          Neutr => table {
            Nom => table {Strong => StrongPlNeutrNom3p ; Weak => WeakPlNeutrNom3p} ;
            Acc => table {Strong => StrongPlNeutrAcc3p ; Weak => WeakPlNeutrAcc3p} ;
            Dat => table {Strong => StrongPlNeutrDat3p ; Weak => WeakPlNeutrDat3p} ;
            Gen => table {Strong => StrongPlNeutrGen3p ; Weak => WeakPlNeutrGen3p}
          } 
        }
      }
    };

  masc_ur : Str -> Adjective = \vondur ->
    let vond = init (init vondur)
      in mkAdjective vondur (vond+"i")
                    (vond+"an") (vond+"a")
                    (vond+"um") (vond+"a")
                    (vond+"s") (vond+"a")
                    (vond) (vond+"a")
                    (vond+"a") (vond+"u")
                    (vond+"ri") (vond+"u")
                    (vond+"rar") (vond+"u")
                    (init vond + "t") (vond+"a")
                    (init vond+"t") (vond+"a")
                    (init vond+"du") (vond+"a")
                    (vond+"s") (vond+"a") 
                    (vond+"ir") (vond+"u")
                    (vond+"a") (vond+"u")
                    (vond+"um") (vond+"u")
                    (vond+"ra") (vond+"u")
                    (vond+"ar") (vond+"u")
                    (vond+"ar") (vond+"u")
                    (vond+"um") (vond+"u")
                    (vond+"ra") (vond+"u")
                    vond (vond+"u")
                    vond (vond+"u")
                    (vond+"um") (vond+"u")
                    (vond+"ra") (vond+"u") ;            

  masc_r : Str -> Adjective = \blar ->
    let bla = init blar
      in mkAdjective blar (bla+"i")
                    (bla+"an") (bla+"a")
                    (bla+"um") (bla+"a")
                    (bla+"s") (bla+"a")
                    bla (bla+"a")
                    (bla+"a") (bla+"u")
                    (bla+"rri") (bla+"u")
                    (bla+"u") (bla+"u")
                    (bla+"tt") (bla+"a")
                    (bla+"tt") (bla+"a")
                    (bla+"u") (bla+"a")
                    (bla+"s") (bla+"a") 
                    (bla+"ir") (bla+"u")
                    (bla+"a") (bla+"u")
                    (bla+"um") (bla+"u")
                    (bla+"ra") (bla+"u")
                    (bla+"ar") (bla+"u")
                    (bla+"ar") (bla+"u")
                    (bla+"um") (bla+"u")
                    (bla+"ra") (bla+"u") 
                    bla (bla+"u")
                    bla (bla+"u")
                    (bla+"um") (bla+"u")
                    (bla+"rra") (bla+"u") ;

  masc_nn : Str -> Adjective = \hreinn ->
    let hrein = init hreinn
      in mkAdjective hreinn (hrein+"i")
                    (hrein+"an") (hrein+"a")
                    (hrein+"um") (hrein+"a")
                    (hrein+"s") (hrein+"a")
                    hrein (hrein+"a")
                    (hrein+"a") (hrein+"u")
                    (hrein+"ni") (hrein+"u")
                    (hrein+"nar") (hrein+"u")
                    (hrein+"t") (hrein+"a")
                    (hrein+"t") (hrein+"a")
                    (hrein+"u") (hrein+"a")
                    (hrein+"s") (hrein+"a") 
                    (hrein+"ir") (hrein+"u")
                    (hrein+"a") (hrein+"u")
                    (hrein+"um") (hrein+"u")
                    (hrein+"na") (hrein+"u")
                    (hrein+"ar") (hrein+"u")
                    (hrein+"ar") (hrein+"u")
                    (hrein+"um") (hrein+"u")
                    (hrein+"na") (hrein+"u")
                    hrein (hrein+"u")
                    hrein (hrein+"u")
                    (hrein+"um") (hrein+"u")
                    (hrein+"na") (hrein+"u") ;                

  svartur_A = mkAdjective "svartur" "svarti" 
                          "svartan" "svarta"
                          "svortum" "svarta"
                          "svarts" "svarta"
                          "svort" "svarta"
                          "svarta" "svortu"
                          "svartri" "svortu"
                          "svartrar" "svortu"
                          "svart" "svarta"
                          "svart" "svarta"
                          "svortu" "svarta"
                          "svarts" "svarta" 
                          "svartir" "svortu"
                          "svarta" "svortu"
                          "svortum" "svortu"
                          "svartra" "svortu"
                          "svartar" "svortu"
                          "svartar" "svortu"
                          "svortum" "svortu"
                          "svartra" "svortu"
                          "svort" "svortu"
                          "svort" "svortu"
                          "svortum" "svortu"
                          "svartra" "svortu" ;

  snjall_A = mkAdjective "snjall" "snjalli" 
                          "snjallan" "snjalla"
                          "snjollum" "snjalla"
                          "snjalls" "snjalla"
                          "snjoll" "snjalla"
                          "snjalla" "snjollu"
                          "snjallri" "snjollu"
                          "snjallrar" "snjollu"
                          "snjallt" "snjalla"
                          "snjallt" "snjalla"
                          "snjollu" "snjalla"
                          "snjalls" "snjalla" 
                          "snjallir" "snjollu"
                          "snjalla" "snjollu"
                          "snjollum" "snjollu"
                          "snjallra" "snjollu"
                          "snjallar" "snjollu"
                          "snjallar" "snjollu"
                          "snjollum" "snjollu"
                          "snjallra" "snjollu";

  kaldur_A = mkAdjective "kaldur" "kaldi" 
                          "kaldan" "kalda"
                          "koldum" "kalda"
                          "kalds" "kalda"
                          "kold" "kalda"
                          "kalda" "koldu"
                          "kaldri" "koldu"
                          "kaldrar" "koldu"
                          "kalt" "kalda"
                          "kalt" "kalda"
                          "kalda" "kalda"
                          "kalda" "kalda" 
                          "kaldir" "koldu"
                          "kalda" "koldu"
                          "koldum" "koldu"
                          "kaldra" "koldu"
                          "kaldar" "koldu"
                          "kaldar" "koldu"
                          "koldum" "koldu"
                          "kaldra" "koldu"
                          "kold" "koldu"
                          "kold" "koldu"
                          "koldum" "koldu"
                          "kaldra" "koldu"; 

  gamall_A = mkAdjective "gamall" "gamli" 
                          "gamlan" "gamla"
                          "gomlum" "gamla"
                          "gamals" "gamla"
                          "gomul" "gamla"
                          "gamla" "gomlu"
                          "gamalli" "gomlu"
                          "gamallar" "gomlu"
                          "gamalt" "gamla"
                          "gamalt" "gamla"
                          "gomlu" "gamla"
                          "gamals" "gamla" 
                          "gamlir" "gomlu"
                          "gamla" "gomlu"
                          "gomlum" "gomlu"
                          "gamalla" "gomlu"
                          "gamlar" "gomlu"
                          "gamlar" "gomlu"
                          "gomlum" "gomlu"
                          "gamalla" "gomlu"
                          "gomul" "gomlu"
                          "gomul" "gomlu"
                          "gomlum" "gomlu"
                          "gamalla" "gomlu";

  tilbuinn_A = mkAdjective "tilbuinn" "tilbuni" 
                          "tilbuinn" "tilbuna"
                          "tilbunum" "tilbuna"
                          "tilbuins" "tilbuna"
                          "tilbuin" "tilbuna"
                          "tilbuna" "tilbunu"
                          "tilbuinni" "tilbunu"
                          "tilbuinnar" "tilbunu"
                          "tilbuid" "tilbuna"
                          "tilbuid" "tilbuna"
                          "tilbunu" "tilbuna"
                          "tilbuins" "tilbuna" 
                          "tilbunir" "tilbunu"
                          "tilbuna" "tilbunu"
                          "tilbunum" "tilbunu"
                          "tilbuinna" "tilbunu"
                          "tilbunar" "tilbunu"
                          "tilbunar" "tilbunu"
                          "tilbunum" "tilbunu"
                          "tilbuinna" "tilbunu"
                          "tilbuin" "tilbunu"
                          "tilbuin" "tilbunu"
                          "tilbunum" "tilbunu"
                          "tilbuinna" "tilbunu";

  litill_A = mkAdjective "litll" "litli" 
                          "litinn" "litla"
                          "litlum" "litla"
                          "litils" "litla"
                          "litil" "litla"
                          "litla" "litlu"
                          "litilli" "litlu"
                          "litillar" "litlu"
                          "litid" "litla"
                          "litid" "litla"
                          "litlu" "litla"
                          "litils" "litla" 
                          "litlir" "litlu"
                          "litla" "litlu"
                          "litlum" "litlu"
                          "litilla" "litlu"
                          "litlar" "litlu"
                          "litlar" "litlu"
                          "litlum" "litlu"
                          "litilla" "litlu"
                          "litil" "litlu"
                          "litil" "litlu"
                          "litlum" "litlu"
                          "litilla" "litlu";

  stor_A = mkAdjective "stor" "stori" 
                        "storan" "stora"
                        "storum" "stora"
                        "stors" "stora"
                        "stor" "stora"
                        "stora" "storu"
                        "storri" "storu"
                        "storrar" "storu"
                        "stort" "stora"
                        "stort" "stora"
                        "storu" "stora"
                        "stors" "stora" 
                        "storir" "storu"
                        "stora" "storu"
                        "storum" "storu"
                        "storra" "storu"
                        "storar" "storu"
                        "storar" "storu"
                        "storum" "storu"
                        "storra" "storu"
                        "stor" "storu"
                        "stor" "storu"
                        "storum" "storu"
                        "storra" "storu";

  smartAdjective : Str -> Adjective = \StrongSgMascNom3p -> case StrongSgMascNom3p of {
     _ + "ur" => masc_ur StrongSgMascNom3p;
     _ + "r" => masc_r StrongSgMascNom3p;
     _ + "nn" => masc_nn StrongSgMascNom3p 
     } ;


  Verb : Type = {s : Number => Person => Str ; sup : Str } ;
    
    mkVerb : (IndicPres1pSg,IndicPres2pSg,IndicPres3pSg,IndicPres1pPl,IndicPres2pPl,IndicPres3pPl,Supine : Str) -> Verb
     = \IndicPres1pSg,IndicPres2pSg,IndicPres3pSg,IndicPres1pPl,IndicPres2pPl,IndicPres3pPl,Supine -> {
     s = table { 
                  Sing => table { P1 => IndicPres1pSg ;
                                  P2 => IndicPres2pSg ;
                                  P3 => IndicPres3pSg };
                  Plur => table { P1 => IndicPres1pPl;
                                  P2 => IndicPres2pPl;
                                  P3 => IndicPres3pPl }
     };
                  sup = Supine
        
       } ; 


  -- Verb : Type = {s : VerbForm => Number => Person => Str} ;

  -- mkVerb : (IndicPres1pSg,IndicPres2pSg,IndicPres3pSg,IndicPres1pPl,IndicPres2pPl,IndicPres3pPl,Inf,Sup : Str) -> Verb =
	-- 		\IndicPres1pSg,IndicPres2pSg,IndicPres3pSg,IndicPres1pPl,IndicPres2pPl,IndicPres3pPl,Inf,Sup -> {
	-- 			s = table {
	-- 				Pres => table { Sing => table { P1	=> IndicPres1pSg ;
	-- 				                                P2	=> IndicPres2pSg ;
	-- 				                                P3	=> IndicPres3pSg }
  --                                         };
  --                        Plur => table { P1	=> IndicPres1pPl ; 
	-- 				                                P2	=> IndicPres2pPl ;
  --                                         P3  => IndicPres3pPl }
  --         				}; 
  --       Inf = Inf ;
	-- 			Supine= Sup

	-- 	} ;	




    weakVerb : Str -> Verb = \hoppa ->
      mkVerb hoppa (hoppa+"r") (hoppa+"r")
             (init hoppa+"um") (init hoppa + "id") hoppa 
             (hoppa+"d") ;

    brjota_V = mkVerb "bryt" "brytur" "brytur"
                      "brjotum" "brjotid" "brjota"
                      "brotid" ;
    
    kaupa_V = mkVerb "kaupi" "kaupir" "kaupir"
                     "kaupum" "kaupid" "kaupa"
                     "keypt" ;

    koma_V = mkVerb "kem" "kemur" "kemur"
                    "komum" "komid" "koma"
                    "komid" ;

    drekka_V = mkVerb "drekk" "drekkur" "drekkur"
                      "drekkum" "drekkid" "drekka"
                      "drukkid" ;

    sja_V = mkVerb "se" "serd" "ser"
                    "sjaum" "sjaid" "sja"
                    "sed" ; 

    synda_V = mkVerb "syndi" "syndir" "syndir"
                      "syndum" "syndid" "synda"
                      "synt" ;

    fara_V = mkVerb "fer" "ferd" "fer"
                    "forum" "farid" "fara"
                    "farid" ; 
    drepa_V = mkVerb "drep" "drepur" "drepur"
                      "drepum" "drepid" "drepa"
                      "drepid" ;

    lesa_V = mkVerb "les" "lest" "les"
                    "lesum" "lesid" "lesa"
                    "lesid" ;

    hlaupa_V = mkVerb "hleyp" "hleypur" "hleypur"
                      "hlaupum" "hlaupid" "hlaupa"
                      "hlaupid" ;

    finna_V = mkVerb "finn" "finnur" "finnur"
                    "finnum" "finnid" "finna"
                    "fundid" ;
    sofa_V = mkVerb "sef" "sefur" "sefur"
                    "sofum" "sofid" "sofa"
                    "sofid" ;

    lifa_V = mkVerb "lifi" "lifir" "lifir"
                    "lifum" "lifid" "lifa"
                    "lifad" ; 
    skilja_V = mkVerb "skil" "skilur" "skilur"
                    "skiljum" "skiljid" "skilja"
                    "skilid" ; 

Verb2 : Type = Verb ** {c : Str} ;

negation : Bool -> Str = \b -> case b of {True => [] ; False => "ekki"} ;

copulaVerb : Verb = mkVerb "er" "ert" "er" "erum" "erud" "eru" "verid" ;

  --  vpForm : Bool -> Verb -> {sg,pl : Str} = \isPres,verb -> case isPres of {
  --     True  => {fin = verb.s ! Sing  ; inf = []} ;
  --     False => {fin = "hefur" ; inf = verb.s ! Sup}
  --     } ;



}
