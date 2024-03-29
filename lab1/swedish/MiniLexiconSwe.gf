--# -path=.:../../abstract

concrete MiniLexiconSwe of MiniLexicon = MiniGrammarSwe **
  open
    MiniParadigmsSwe,
    MiniResSwe
  in {
lin already_Adv = mkAdv "redan" ;
lin animal_N = mkN "djur" "djur" ;
lin apple_N = mkN "äpple" ;
lin baby_N = mkN "bebis" ;
lin bad_A = mkA "dålig" ;
lin beer_N = mkN "öl" "öl" ;
lin big_A = mkA "stor" ;
lin bike_N = mkN "cykel" "cyklar" ;
lin bird_N = mkN "fågel" "fåglar" ;
lin black_A = mkA "svart" ;
lin blood_N = mkN "blod" "blod" ;
lin blue_A = mkA "blå" ;
lin boat_N = mkN "båt" ;
lin book_N = mkN "bok" "böcker" ;
lin boy_N = mkN "pojke" ;
lin bread_N = mkN "bröd" "bröd" ;
lin break_V2 = mkV2 (mkV "krossa") ;
lin buy_V2 = mkV2 (mkV "köper") ;
lin car_N = mkN "bil" ;
lin cat_N = mkN "katt" "katter" ;
lin child_N = mkN "barn" "barn" ;
lin city_N = mkN "stad" "städer" ;
lin clean_A = mkA "ren" ;
lin clever_A = mkA "klok" ;
lin cloud_N = mkN "moln" "moln" ;
lin cold_A = mkA "kall" ;
lin come_V = mkV "komma" "kommer" "kom" "kommit" "kom" ;
lin computer_N = mkN "dator" "datorer" ;
lin cow_N = mkN "ko" "kon" ;
lin dirty_A = mkA "smutsig" ;
lin dog_N = mkN "hund" ;
lin drink_V2 = mkV2 (mkV "dricka" "drack" "druckit") ;
lin eat_V2 = mkV2 (mkV "äta" "åt" "ätit") ;
lin find_V2 = mkV2 (mkV "hitta") ;
lin fire_N = mkN "eld" ;
lin fish_N = mkN "fisk" ;
lin flower_N = mkN "blomma" ;
lin friend_N = mkN "vän" "vänner" ;
lin girl_N = mkN "flicka" ;
lin good_A = mkA "god" "gott" "goda" ;
lin go_V = mkV "gå" "går" "gick" "gått" "gå" ;
lin grammar_N = mkN "grammatik" "grammatiker" ;
lin green_A = mkA "grön" ;
lin heavy_A = mkA "tung" ;
lin horse_N = mkN "häst" ;
lin hot_A = mkA "het" ;
lin house_N = mkN "hus" "hus" ;
lin john_PN = mkPN "Johan" ;
lin jump_V = mkV "hoppa" ;
lin kill_V2 = mkV2 "döda" ;
lin language_N = mkN "språk" "språk" ;
lin live_V = mkV "lever" ;
lin love_V2 = mkV2 (mkV "älska") ;
lin man_N = mkN "man" "mannen" "män" "männen" Utr ;
lin milk_N = mkN "mjölk" ;
lin music_N = mkN "musik" "musiker" ;
lin new_A = mkA "ny" "nytt" "nya" ;
lin now_Adv = mkAdv "nu" ;
lin old_A = mkA "gammal" "gammalt" "gamla" ;
lin paris_PN = mkPN "Paris" Neutr ;
lin play_V = mkV "spela" ;
lin read_V2 = mkV2 (mkV "läser") ;
lin ready_A = mkA "färdig" ;
lin red_A = mkA "röd" "rött" "röda" ;
lin river_N = mkN "å" ;
lin run_V = mkV "springa" "sprang" "sprungit" ;
lin sea_N = mkN "hav" "hav" ;
lin see_V2 = mkV2 (mkV "se" "ser" "såg" "sett" "se") ;
lin ship_N = mkN "skepp" "skepp" ;
lin sleep_V = mkV "sova" "sov" "sovit" ;
lin small_A = mkA "liten" "litet" "små" ; ---- lilla
lin star_N = mkN "stjärna" ;
lin swim_V = mkV "simma" ;
lin teach_V2 = mkV2 (mkV "lära" "lär" "lärde" "lärt" "lär") ;
lin train_N = mkN "tåg" "tåg" ;
lin travel_V = mkV "reser" ;
lin tree_N = mkN "träd" "träd" ;
lin understand_V2 = mkV2 (mkV "förstå" "förstår" "förstod"  "förstått" "förstå") ;
lin wait_V2 = mkV2 "vänta" "på" ;
lin walk_V = mkV "promenera" ; --- gå
lin warm_A = mkA "varm" ;
lin water_N = mkN "vatten" "vattnet" "vatten" "vattnen" Neutr ;
lin white_A = mkA "vit" "vitt" "vita" ;
lin wine_N = mkN "vin" "vinet" "viner" "vinerna" Neutr ;
lin woman_N = mkN "kvinna" ;
lin yellow_A = mkA "gul" ;
lin young_A = mkA "ung" ;



}
