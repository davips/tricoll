# cabal install profiteur
W="--work-dir .stack-work-profile"
stack $W build --profile && stack $W exec -- tricoll +RTS -p && profiteur tricoll.prof && firefox tricoll.prof.html
