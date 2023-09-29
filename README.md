### clone
```sh
git clone --recurse-submodules
```
a flake.nix is provided that will give you an environment with [spago](https://github.com/purescript/spago), [purs-backend-es](https://github.com/aristanetworks/purescript-backend-optimizer) and [bqn](https://github.com/mlochbaum/BQN).
### get data
ubq has asked me not to send requests very often to this. please keep in mind. 
```sh
curl -L 'https://ubq323.website/ffbm/scores.cgi?act=all' > ffbm-data.json
```
### process data
```sh
bqn process.bqn ffbm-data.json
```
### build
```sh
spago bundle-app  # using purs
spago -x prod.dhall build && purs-backend-es bundle-app --no-build -y  # optimized and minimized
```