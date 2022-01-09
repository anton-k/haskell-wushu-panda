# Password obfuscator

![Safe pic](https://github.com/anton-k/haskell-wushu-panda/blob/main/img/password-obfuscator.jpg)

Many web sites do want to know our credentials. Hopefully they use
obfuscation techniques to prevent stealing of the paswords and reusal of them
on other platforms. Hopefully they do it but unfortunately some don't do that
and even don't do that on purpose. 

As martial pandas we know lots of top secrets and we'd like to keep them safe.
So let's create a tool to obfuscate the passeords so that we can easily
generate passwords for various platforms. 

Our requirements: it should be easy to 
* generate strong passwords 
* update passwords if it's reused or expired
* we don't know the passwrds and can not look them up (auto copy paste)

## Password obfuscation theory

The main idea is to generate strong passwords out of master password and the input data.
We never show the master password and keep it secret and use only derived passwords on sites.
It's convenient to use the web site name as an input. This way we can generate unique 
and strong passwords. There are several cryptographic algorithms to use for the task:

* PBKDF1
* PBKDF2
* Argon2
* bcrypt
* scrypt
* Catena, Lyra2, Makwa, or yescrypt

We can pick up any of them. We can study the hackage to choose the algorithm we need.
Hopefully somebody already implemented that and indeed we have several options.
Let's use library [argon2](https://hackage.haskell.org/package/argon2).
Study the docs for the library.
We init the project and add `argon2` to dependencies.

```
-- file hpass.cabal
  build-depends:
      base >=4.7 && <5
    , argon2
```

## Design of the application

We will create a CLI that asks for master password and auxilliary input
to produce derived password. The password is going to by pasted to our buffer.
So that we can Copy paste it in the browser window that asks for it.

## Include github dependency to stack

With argon2 we face the problems. As we can not use it on stack directly.
Stack keeps it's own versions of some subset of the hackage. The rest of the libraries
we need to specify in the `extra-deps` field of the `stack.yaml` file.
For our task we need to add to `stack.yaml`:

```yaml
extra-deps:
   - git: https://github.com/haskell-hvr/argon2
     commit: cafbc6e5d998ba182d92879c3a6e7b3f5ca93fc1

 allow-newer: true
```

We also allow newer as there is dependency mismatch on regions of the base library in the argon2
and our project.

## Application

Let's build the application bit by bit.

### Obfuscation

### Copy paste the result

### Parse the arguments

### Hide the master password input

## Nix instead of stack

Let's use the Nix instead of stack for a change. 

## Tips and tricks

As we don't know the password it's easy to misspel stuff on the init. 
If the site wants to retype the password don't copy-paste it twice. Instead
it's safer to go through our routine twice and generate the password with the same inputs.

## Exercise

* Some platforms require passwords to be no more than so many symbols.
    Add option to generate passwords of certain size.

* Often it's useful to store options in human readable form. 
    Add YAML representation of the options and use this format instead of Haskell show/read.
    Read from the yaml file. Use library [yaml](https://hackage.haskell.org/package/yaml).

* Make default file for the hpass options. For now we need to supply the string to the file.
  Instead of that if file is missing try to read YAML config from default file in 
  current directory (say ".hpassrc"). If file is missing either only then use default argon2 options. 
  Use library [`directory`](https://hackage.haskell.org/package/directory) to solve that.

* Make manager of the options. For example instead of editting the `/home/user/.hpassrc` file
  we can set option with:

  ```
  hpass --set NAME --value VALUE
  ```
  
  or 

  ```
  hpass set NAME VALUE
  ```
  
  Look at [commands](https://hackage.haskell.org/package/optparse-applicative-0.16.1.0#commands)
  in the library `optparse-applicative` to implement latter case.

