data ← (•Import "include/json.bqn").Parse •FChars "ffbm-data-2023-05-08.json"
! ∧´(<"name"‿"score"‿"modes"‿"ts")≡¨⊏¨data
"output.txt"•FLines(⊣∾","∾⊢)´¨((↑⟜"-"0⊸>)∾•Fmt∘|)⌾⊑¨{𝕩⊏˜⊑⍒⊏˘𝕩}¨(⊢⊔˜·⊐1⊏˘⊢)>(>⌾(2⊸⊑)1⌽1⊸⊏)¨data