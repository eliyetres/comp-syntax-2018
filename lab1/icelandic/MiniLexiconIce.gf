--# -path=.:../../abstract
concrete MiniLexiconIce of MiniLexicon = MiniGrammarIce **
  open
    MiniParadigmsIce, MiniResIce
  in {
lin already_Adv = mkAdv "nu thegar" ;
lin animal_N = mkN "dyr" "dyrs" ;
lin apple_N = mkN "epli" "eplis" ;
lin baby_N = mkN "barn" "barns" ;
lin bad_A = mkA "vondur" ;
lin beer_N = mkN "bjor" "bjors" ;
lin big_A = mkA "stor" ;
lin bike_N = mkN "hjol" "hjols" ;
lin bird_N = mkN "fugl" "fugls" ;
lin black_A = svartur_A ;
lin blood_N = mkN "blod" "blods" ;
lin blue_A = mkA "blar" ;
lin boat_N = mkN "batur" ;
lin book_N = mkN "bok" "bokar" ;
lin boy_N = mkN "strakur" ;
lin bread_N = mkN "braud" "brauds" ;
lin break_V2 = mkV2 brjota_V ;
lin buy_V2 = mkV2 kaupa_V ;
lin car_N = mkN "bill" "bils" ;
lin cat_N = mkN "kottur" ;
lin child_N = mkN "barn" "barns" ;
lin city_N = mkN "borg" "borgar" ;
lin clean_A = mkA "hreinn" ;
lin clever_A = snjall_A ;
lin cloud_N = mkN "sky" ;
lin cold_A = mkA "kaldur" ;
lin come_V = koma_V ;
lin computer_N = mkN "tolva" ;
lin cow_N = mkN "kyr" ;
lin dirty_A = mkA "skitugur" ;
lin dog_N = mkN "hundur" ;
lin drink_V2 = mkV2 drekka_V ;
lin eat_V2 = mkV2 "borda" ;
lin find_V2 = mkV2 finna_V ;
lin fire_N = mkN "eldur" ;
lin fish_N = mkN "fiskur" ;
lin flower_N = mkN "blom" ;
lin friend_N = mkN "vinur" ;
lin girl_N = mkN "stelpa" ;
lin good_A = mkA "godur" ;
lin go_V = fara_V ;
lin grammar_N = mkN "malfraedi" ;
lin green_A = mkA "graenn" ;
lin heavy_A = mkA "thungur" ;
lin horse_N = mkN "hestur" ;
lin hot_A = mkA "heitur" ;
lin house_N = mkN "hus" ;
lin john_PN = mkPN "Jon" Masc ;
lin jump_V = mkV "hoppa" ;
lin kill_V2 = mkV2 drepa_V ;
lin language_N = mkN "tungumal" ;
lin live_V = lifa_V ;
lin love_V2 = mkV2 "elska" ;
lin man_N = mkN "madur" "menn" ;
lin milk_N = mkN "mjolk" ;
lin music_N = mkN "tonlist" ;
lin new_A = mkA "nyr" ;
lin now_Adv = mkAdv "nu+na" ;
lin old_A = mkA "gamall" ;
lin paris_PN = mkPN "Paris" Fem ;
lin play_V = mkV "spila" ;
lin read_V2 = mkV2 lesa_V ;
lin ready_A = mkA "tilbuinn" ;
lin red_A = mkA "raudur" ;
lin river_N = mkN "a" ;
lin run_V = hlaupa_V ;
lin sea_N = mkN "sjor" ;
lin see_V2 = mkV2 sja_V ;
lin ship_N = mkN "skip" ;
lin sleep_V = sofa_V ;
lin small_A = mkA "litill" ;
lin star_N = mkN "stjarna" ;
lin swim_V = synda_V ;
lin teach_V2 = mkV2 "kenna" ;
lin train_N = mkN "lest" ;
lin travel_V = mkV "ferdast" ;
lin tree_N = mkN "tre" ;
lin understand_V2 = mkV2 skilja_V ;
lin wait_V2 = mkV2 "bida" ;
lin walk_V = mkV "ganga" ;
lin warm_A = mkA "hlyr" ;
lin water_N = mkN "vatn" ;
lin white_A = mkA "hvitur" ;
lin wine_N = mkN "vin" ;
lin woman_N = mkN "kona" ;
lin yellow_A = mkA "gulur" ;
lin young_A = mkA "ungur" ;

}
