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



