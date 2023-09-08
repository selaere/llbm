dt‿json ← •Import¨ "include/"⊸∾¨"datetime.bqn"‿"json.bqn" # https://github.com/mlochbaum/bqn-libs
Fmt ← (0⊸>/"-"˙)∾•Fmt∘|
t←0 ⋄ ToUnix ← { "null":Fmt t+↩1; Fmt ⌊dt.FromTimestamp •ParseFloat¨ ∊⟜"-: "⊸((⊢-˜+`×¬)⊸⊔) 𝕩 }∘>
data ← json.Parse •FChars ⊑•args
! ∧´(<"name"‿"score"‿"modes"‿"ts")≡¨⊏¨data  # check key order hasnt changed
table ← (⊢⊔˜·⊐1⊏˘⊢) >(ToUnix⌾(2⊸⊑)1⌽1⊸⊏)¨data    # read data, convert dates, rearrange, group by gamemode
high  ← >(⊢⊏˜·⊑∘⍒⊏˘)¨table  # keep only highest scores per gamemode
hist  ← ∾(⊢/˜·(⊢>¯9»⌈`)⊏˘)¨table  # keep scores that were higher than the earlier high score
date  ← Fmt ⌊•UnixTime @ #•file.Modified ⊑•args    # for "last updated"
"high.txt"‿"hist.txt"{𝕨•FLines date<⊸∾(⊣∾' '∾⊢)´¨<˘Fmt⌾⊑˘𝕩}¨high‿hist