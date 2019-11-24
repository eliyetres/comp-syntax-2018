-- model implementation using standard RGL
--# -path=.:danish



concrete DoctorDan of Doctor =
  open
    SyntaxDan,
    ParadigmsDan,
    Prelude
  in {

  -------------------
-- the first part could be a functor
-- exception in Swe: isProfessionProperty, no article: "I am a doctor - jag är läkare"

lincat
Phrase = Utt ;
Fact = Cl ;
Action = VP ;
Property = VP ;
Profession = CN ;
Person = NP ;
Place = {at,to : Adv} ;
Substance = NP ;
Illness = NP ;

lin
presPosPhrase fact = mkUtt (mkS fact) ;
presNegPhrase fact = mkUtt (mkS negativePol fact) ;
pastPosPhrase fact = mkUtt (mkS anteriorAnt fact) ;
pastNegPhrase fact = mkUtt (mkS anteriorAnt negativePol fact) ;
presQuestionPhrase fact = mkUtt (mkQS (mkQCl fact)) ;
pastQuestionPhrase fact = mkUtt (mkQS anteriorAnt (mkQCl fact)) ;

impPosPhrase action = mkUtt (mkImp action) ;
impNegPhrase action = mkUtt negativePol (mkImp action) ;

actionFact person action = mkCl person action ;
propertyFact person property = mkCl person property ;

isProfessionProperty profession = mkVP (mkNP profession) ; -- the only difference from Eng
needProfessionProperty profession = mkVP need_V2 (mkNP a_Det profession) ;
isAtPlaceProperty place = mkVP place.at ;
haveIllnessProperty illness = mkVP have_V2 illness ;

theProfessionPerson profession = mkNP the_Det profession ;

iMascPerson = i_NP ;
iFemPerson = i_NP ;
youMascPerson = you_NP ;
youFemPerson = you_NP ;
hePerson = he_NP ;
shePerson = she_NP ;

goToAction place = mkVP (mkVP go_V) place.to ;
stayAtAction place = mkVP (mkVP stay_V) place.at ;
vaccinateAction person = mkVP vaccinate_V2 person ;
examineAction person = mkVP examine_V2 person ;
takeSubstanceAction substance = mkVP take_V2 substance ;

-- end of functor

coughAction = mkVP (mkV "hoste") ;
breatheAction = mkVP (depV (mkV "åender")) ;
vomitAction = mkVP (depV (mkV "spytte"))  ;
sleepAction = mkVP (mkV "sove" "so" "sovet") ;
undressAction = mkVP (mkVP take_V2 (mkNP thePl_Det (mkN "klæd" "klæder"))) (pAdv "af") ;
dressAction = mkVP (mkVP put_V2 (mkNP thePl_Det (mkN "klæd" "klæder"))) (pAdv "på") ;
eatAction = mkVP (mkV "spise" "spiste" "spist") ;
drinkAction = mkVP (mkV "drikke" "drak" "drukket") ;
smokeAction = mkVP (mkV "ryge") ;
measureTemperatureAction = mkVP take_V2 (mkNP the_Det (mkN "temperatur")) ;
measureBloodPressureAction = mkVP (mkV2 (mkV "male")) (mkNP the_Det (mkN "blodtryk" neutrum)) ;

hospitalPlace = {at = pAdv "på sygehus" ; to = pAdv "til sygehus"} ;
homePlace = {at = pAdv "hjemme" ; to = pAdv "hjem"} ;
schoolPlace = {at = pAdv "i skolen" ; to = pAdv "ti skolen"} ;
workPlace = {at = pAdv "på arbejde" ; to = pAdv "til arbejde"} ;

doctorProfession = mkCN (mkN "læge") ;
nurseProfession = mkCN (mkN "sygeplejer" "sygeplejerske") ;
interpreterProfession = mkCN (mkN "tolk") ;

bePregnantProperty = mkVP (mkA "gravid") ;
beIllProperty = mkVP (mkA "syg") ;
beWellProperty = mkVP (mkA "sund") ;
beDeadProperty = mkVP (mkA "død") ;
haveAllergiesProperty = mkVP have_V2 (mkNP aPl_Det (mkN "allergi" "allergier")) ;
havePainsProperty = mkVP have_V2 (mkNP aPl_Det (mkN "smerte")) ;
haveChildrenProperty = mkVP have_V2 (mkNP aPl_Det (mkN "barn" "børn")) ;

feverIllness = mkNP (mkN "feber") ;
fluIllness = mkNP a_Det (mkN "forkølelse") ;
headacheIllness = mkNP (mkN "hovedpine" "hovedpiner") ;
diarrheaIllness = mkNP (mkN "diarré" "diarréer") ;
heartDiseaseIllness = mkNP aPl_Det (mkN "hjertelidelser") ;
lungDiseaseIllness = mkNP aPl_Det (mkN "lungesygdom") ;
hypertensionIllness = mkNP (mkN "hypertension") ;

alcoholSubstance = mkNP (mkN "alkohol") ;
medicineSubstance = mkNP aPl_Det (mkN "lægemiddel" "lægemiddel") ;
drugsSubstance = mkNP aPl_Det (mkN "stof" "stoffer") ;

oper
pAdv : Str -> Adv = ParadigmsDan.mkAdv ;

go_V = mkV "gå" "gik" "gået" ;
stay_V = mkV "blive" "blev" "blevet" ;
need_V2 = mkV2 (mkV "behøve") ;
take_V2 = mkV2 (mkV "tage" "tog" "taget") ;
put_V2 = mkV2 (mkV "sætter" "satte" "sat") ;
vaccinate_V2 = mkV2 (mkV "vaccinere") ;
examine_V2 = mkV2 (mkV "undersøge") ;

}
