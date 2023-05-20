## build
install elm and then
```sh
elm make --output pkg/main.js src/Main.elm
```
## get data
ubq has asked me not to send requests very often to this. please keep in mind. 

have [bqn](https://mlochbaum.github.io/BQN/) in path and [bqn-libs](https://github.com/mlochbaum/bqn-libs) in /include
```sh
curl -L 'https://ubq323.website/ffbm/scores.cgi?act=all' > ffbm-data-$(date -Id).json
bqn process.bqn ffbm-data-$(date -Id).json
```