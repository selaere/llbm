dt‿json ← •Import¨ "include/"⊸∾¨"datetime.bqn"‿"json.bqn" # https://github.com/mlochbaum/bqn-libs
Fmt ← (↑⟜"-"0⊸>)∾•Fmt∘|
ToUnix ← { "null":"0"; Fmt ⌊dt.FromTimestamp •ParseFloat¨ ∊⟜"-: "⊸((⊢-˜+`×¬)⊸⊔) 𝕩 }∘>
data ← json.Parse •FChars ⊑•args
! ∧´(<"name"‿"score"‿"modes"‿"ts")≡¨⊏¨data  # check key order hasnt changed
table ← >(ToUnix⌾(2⊸⊑)1⌽1⊸⊏)¨data    # read data, convert dates, rearrange
high  ← {𝕩⊏˜⊑⍒⊏˘𝕩}¨(⊢⊔˜·⊐1⊏˘⊢)table  # keep only highest scores per gamemode
date  ← Fmt ⌊•UnixTime @ #•file.Modified ⊑•args    # for "last updated"
"output.txt"•FLines (<date)∾(⊣∾' '∾⊢)´¨Fmt⌾⊑¨high