
~~~
cabal2nix --shell . > ./shell.nix
nix-shell -p pkgs.graphviz
cabal configure
cabal build
cabal run -- -f test.conf
~~~


~~~
librarySystemDepends = [ graphviz ];
executableSystemDepends = [ graphviz ];
~~~
