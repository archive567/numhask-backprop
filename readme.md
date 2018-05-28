numhask-backprop
================

[![Build
Status](https://travis-ci.org/tonyday567/numhask-backprop.svg)](https://travis-ci.org/tonyday567/numhask-backprop)
[![Hackage](https://img.shields.io/hackage/v/numhask-backprop.svg)](https://hackage.haskell.org/package/numhask-backprop)
[![lts](https://www.stackage.org/package/numhask-backprop/badge/lts)](http://stackage.org/lts/package/numhask-backprop)
[![nightly](https://www.stackage.org/package/numhask-backprop/badge/nightly)](http://stackage.org/nightly/package/numhask-backprop)

numhask-backprop

results
-------

alpha: -1.0000000000000024 beta: 2.0000000000000036

recipe
------

    stack build --test --exec "$(stack path --local-install-root)/bin/numhask-backprop" --exec "$(stack path --local-bin)/pandoc -f markdown -i other/_readme.md -t markdown -o readme.md --filter pandoc-include --mathjax" --file-watch

reference
---------

-   [gitter](https://gitter.im/haskell-backprop/Lobby)
-   [backprop](https://github.com/mstksg/backprop)
-   [hackage](https://hackage.haskell.org/package/backprop-0.2.3.0)
-   [blog
    code](https://github.com/mstksg/inCode/blob/master/code-samples/functional-models/model.hs)
-   [blog intro](https://backprop.jle.im/)

