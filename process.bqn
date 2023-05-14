dt‿json ← •Import¨ "include/"⊸∾¨"datetime.bqn"‿"json.bqn" # https://github.com/mlochbaum/bqn-libs
Fmt ← (↑⟜"-"0⊸>)∾•Fmt∘|
ToUnix ← { "null" : "0" ; Fmt ⌊ dt.FromTimestamp •ParseFloat¨ ∊⟜"-: "⊸((⊢-˜+`×¬)⊸⊔) 𝕩 }∘>
data ← (•Import "include/json.bqn").Parse •FChars 0⊑•args
! ∧´(<"name"‿"score"‿"modes"‿"ts")≡¨⊏¨data
"output.txt"•FLines(⊣∾' '∾⊢)´¨Fmt⌾⊑¨{𝕩⊏˜⊑⍒⊏˘𝕩}¨(⊢⊔˜·⊐1⊏˘⊢)>(ToUnix⌾(2⊸⊑)1⌽1⊸⊏)¨data