# Serenity Web Media Player

Serenity is a very simple web-based media player, currently similar to a *very* light [subsonic](http://www.subsonic.org/pages/index.jsp). It primary served as a playground for me to learn both Haskell and AngularJS, and is far from production quality, and in fact in it's current state contains a number of known missteps (especially in Angular).

## Dependencies
[Taglib](http://taglib.github.io/)

## Building and Installing
First install [GHC](http://www.haskell.org/ghc/), [Cabal](http://www.haskell.org/cabal/) and the dependencies above, then run `cabal install` from the source directory.

## Usage
By default Serentiy will use `~/.Serenity` as it's data directory.  This can be changed by passing any other directory as the final argument to any commands.

`Serenity --setup` or `-s` Creates the database and the static html/js/css files in it's data directory
`Serenity --import <dir>` or `-i <dir>` Imports all MP3s found in the directory into the database.

When run without any agruments, Serentiy will start it's webserver accessible at [http://localhost:3000](http://localhost:3000)