Readme
======

This is a set of *gitit* plugins I use with a custom version of *gitit* named
*jrg-gitit* that I use. 


Building
--------

In order to test the build pass:

~~~
nix-shell
cabal configure
cabal build
~~~


Whenever a change to the `*.cabal` file occur (most likely only in
dependencies), you need to run the following command:

~~~
cabal2nix . > ./default.nix
~~~


Testing
-------

Actual testing of the plugins is done using the *jrg-gitit* repository.


Future
------

At some point it may become desirable to split this repositories on a per
plugin basis so that they can be used as part of other projects. If anyone
is interested in such use, this is something I am open to consider as well
as publishing the result on hackage.
