dt‿json ← •Import¨ "include/"⊸∾¨"datetime.bqn"‿"json.bqn" # https://github.com/mlochbaum/bqn-libs
Fmt ← (0⊸>/"-"˙)∾•Fmt∘|
ToUnix ← { "null":0; Fmt ⌊dt.FromTimestamp •ParseFloat¨ ∊⟜"-: "⊸((⊢-˜+`×¬)⊸⊔) 𝕩 }∘>⚇1
"there are 179 scores without timestamps at the start of the dataset, but they are in chronological order. the way these times are approximated is by taking some scores that i am sort of certain of the date of (by getting them off esolangs logs), and doing linear interpolation
prepend ""https://discord.com/channels/346530916832903169/348702212110680064/"" to the id to get a message link

score   0  1602598381  2020-10-13 14:13:01
    #off-topic 765577844582645801: 'highscores now ""exist"" // really the highscores should consider mode shouldn't they'
score  30  1602599679  2020-10-13 14:34:39
    #off-topic 765583287959289876: screenshot showing devtools network tab with '200 GET 🔒ubq323.website scores.cgi?act=submit&name=cor&score=-2'
score  47  1602605444  2020-10-13 16:10:44
    #off-topic 765607466141483048: 'highscores now track mode'
score 114  1602694657  2020-10-14 16:57:37
    #off-topic 765981652903919647: 'ffbm has more modes now' (c and m added)
score 179  1602763036  2020-10-15 11:57:16
    score is provided in data
"
s‿t←⟨0‿30‿47‿114‿179,1602598381‿1602599679‿1602605444‿1602694657‿1602763036⟩
prehist ← Fmt¨{b←0‿¯1⊸+˘s⍋𝕩 ⋄ +˝˘|⌊(-˝˘b⊏s)÷˜(⌽˘b⊏i)×𝕩-b⊏s}↕179
data ← json.Parse •FChars ⊑•args
! ∧´(<"name"‿"score"‿"modes"‿"ts")≡¨⊏¨data  # check key order hasnt changed
table ← (⊢⊔˜·⊐1⊏˘⊢) prehist⌾(179⊸↑)∘ToUnix⌾(2⊸⊏˘) >(1⌽1⊸⊏)¨data    # group by gamemode
high  ← 1‿1‿0‿1⊸/˘>(⊢⊏˜·⊑∘⍒⊏˘)¨table  # keep only highest scores per gamemode
hist  ← ∾(⊢/˜·(⊢>¯9»⌈`)⊏˘)¨table      # keep scores that were higher than the earlier high score
date  ← Fmt ⌊•UnixTime @ #•file.Modified ⊑•args    # for "last updated"
"high.txt"‿"hist.txt"{𝕨•FLines date<⊸∾(⊣∾' '∾⊢)´¨<˘Fmt⌾⊑˘𝕩}¨high‿hist