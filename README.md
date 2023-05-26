## get data
ubq has asked me not to send requests very often to this. please keep in mind. 
```sh
curl -L 'https://ubq323.website/ffbm/scores.cgi?act=all' > ffbm-data.json
```
## build
use the nix at `makepage.nix`:
```sh
nix-build makepage.nix --arg data ./ffbm-data.json
```
OR
install elm and bqn and have bqn-libs in ./include and then
```sh
elm make --output Main.js src/Main.elm
cp Main.js Main.min.js  # or use some actual minimization
bqn process.bqn ffbm-data.json
```