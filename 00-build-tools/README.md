# Notes on development tools

Here we can read about cool tools to use with Haskell.
Haskell does not have standard IDE as Java or C++. 
This is good and bad. It is good as it gives you freedom of choice and 
in Haskell teams people don't force their choices. The bad is that you
have to build and configure some stuff yourself. But you can do it in the best way that 
suits your needs.

## Editor

I personally like neovim as it's vim with great defaults.
Also i saw many other editors and teammates could do the magic with them.
You can also look at vim, emacs, Sublime Text, vscode, atom, Leksah and many others.

## Build tools

### Stack

For simplicity we are going to use stack. As it provides nice combos 
of the libraries that work well together and it's easy to use.
we will rely heavily on it's feature of continuous re-building of the code
to make it independent from the concrete editor.

We will use just a handful of commands:

```
-- init project
stack new my-project

-- install libraries and apps
stack install

-- build with watching on file changes
stack build --file-watch

-- try stuff in the ghci REPL
stack ghci
```

It's more often that NIX is used with Haskell on real projects
but for most of the projects `stack` will be more than enough.

### Nix

To make it easy to work with nix we will use the tool [`niv`](https://github.com/nmattia/niv).
First instal nix as it is described [here](https://nixos.org/).

We can setup the nix project with:

```
niv init
```

Then we can add dependencies with niv:

```
> niv add ghc -v 8.4.3 
> niv add github-repo/haskell-lib
```

And we can update and drop the libs

```
niv update haskell-lib -r commit-hash
niv drop haskell-lib
```

Nix is great to use when our project depends on system tools
or on some behemoth haskell libraries that require complicated resolution of dependencies.
You may not stumble on that in this course but it's often happens in real work environment.
The niv tool can make nix setup a breeze.

I think it's vital to know the basics of NIX for modern haskell development.
It's ok to start with stack/cabal but someday I'm sure you will need the nix.
Hopefully somebody will set it up for you for the work but nonetheless it's great
to start learning it bit by bit. Great sources to learn nix:

* [Nix pills](https://nixos.org/guides/nix-pills/)

* [Nix language](https://nixos.wiki/wiki/Nix_Expression_Language)

* Also it's great to read the source code of the real nix projects to learn **undocumented features**.

## Source code navigation

### Find that name

I like to use [ag silver searcher](https://github.com/ggreer/the_silver_searcher)
to find names in the source code. 
I've made my own alias for haskell files:

```
alias agh='ag --haskell'
```

It can use regex to search. It's easy to find function definition with:

```
> agh ^needleFun
```

## Hoogle 

It's great to setup [Hoogle](https://hoogle.haskell.org/) to look up definitions in the projects and in the dependnecies.

TODO: describe how to do it

## Minimal Haskell neovim setup

TODO: describe how to setup neovim to use with haskell

