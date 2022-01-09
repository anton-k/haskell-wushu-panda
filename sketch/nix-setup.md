## Nix instead of stack

Let's use the Nix instead of stack for a change. 
This line of code can cause problems in big projects:

```yaml
allow-newer: true
```

It ignores some version boundaries of the libraries. And this can bite us someday.
With NIX we can pinpoint concrete versions of the libraries. 
This is especially useful with crypto applications. Imagine that we are not careful
on version sepcifications and argon2 library maintainer spots the error in his lib.
Owner of the lib updates the algorithm and we silently our paswords will change and
fail to work. This is not the case with our definition. As we have fixed the commit hash
for stack yaml. With nix we are forced to do that with all libraries. 
Which produce solid and reproducible builds. You can be sure that if NIX project
works on your PC then it will work on nix-powered PC of your friend.

This is because NIX takes rigorous sandbox approach. The whole environment is sandboxed.
Not only haskell libraries as for stack but also OS tools. Stack also has NIX features.
But let's do it in pure NIX way to learn something new.

Install [nix](https://nixos.org/) package manager and [niv](https://github.com/nmattia/niv) 
dependency managment.

Let's init nix project (inside project `hpass` do):

```
> cd hpass
> niv init
```

Let's install GHC:

```
niv add ghc -v 8.10.7
```

This will generate `nix` directory that stores versions of our dependencies.
To be able to build the code we need to specify define special file `default.nix`:

TODO complete me:
See tutorial: https://scrive.github.io/nix-workshop/05-package-management/02-basic-haskell.html
for basic setup

```

```
