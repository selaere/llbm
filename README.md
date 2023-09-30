see it live at <https://selaere.github.io/llbm/>

## what is this?
```log
[18:50] taswelll: oh sorry this requires a bit of context
[18:51] taswelll: this visualizes high scores in the video game ",flappy flap bird man" (https://ubq323.website/ffbm/)
[18:51] taswelll: if you visit the website you will see there are a bunch of modes you can choose! Bad, Worse, Unsatisfactory, Inadequate, etcetcetc
[18:51] taswelll: BUT
[18:51] taswelll: they are CHECKBOXES
[18:52] taswelll: you can choose MULTIPLE of them
[18:52] taswelll: and they all have their own leaderboard
[18:53] taswelll: ",leader lead board man" is a website that visualizes the highest scores in those combinations of modes
[18:55] taswelll: it's basically a table but split in half (because order of modes doesn't matter)
[18:56] taswelll: the games with 1 mode are in the diagonal
[18:56] taswelll: the game without any is in the sticky outy bit at the top
[18:56] taswelll: the games with 2 modes are the rest of the table
```
## how 2 make?
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