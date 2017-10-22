mkdir chain
testrpc --db chain

To keep the addresses, so you don't have to import accounts into MetaMask again, do:

-- ACTUALLY, doesn't just --db chain do this for you?

testrpc --db chain -m "the HD wallet mnemonic"

npx watchify eth.js -t babelify -o bundle.js
npx solcjs Y.sol --bin
npx solcjs Y.sol --abi

Have to rename ABI file from .abi to .json.

python -m SimpleHTTPServer
