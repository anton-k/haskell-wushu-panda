# Haskell wushu panda

![Panda pic](https://github.com/anton-k/haskell-wushu-panda/blob/main/img/main-panda-logo.jpg)

### Introduction

Welcome to **Haskell in Practice** — a series designed for developers who have already dipped their toes into the
waters of Haskell (perhaps from books like [elephant-book](https://www.amazon.com/Learn-You-Haskell-Great-Good/dp/1593272839) 
or some other [intermediate course](https://www.goodreads.com/book/show/25587599-haskell-programming-from-first-principles) on Haskell 
or even the advanced [In depth book](https://www.manning.com/books/haskell-in-depth)).
Many find that even after reading these resources, they're unsure how to apply their knowledge effectively. This
course bridges that gap by providing bite-sized projects and practical examples.

Each project in this series is a sequence of tiny applications — each one runnable and focused on doing something
useful or fun. The learning material includes the source code for each project, plus detailed explanations in the
Readme files. I encourage you to retype these tutorials yourself to see how they work (or even better, find your
own way). My goal is simple: help you realize that building powerful things doesn't require writing massive
amounts of code.

The point here isn’t just typing thousands of lines — my real-world projects often stayed under 4k lines, and yet
were fully functional. That’s one of the beauties of functional programming in Haskell. I hope this series will
inspire you to build your own practical applications, useful or fun or both — something that feels rewarding.

### Table of Projects

* [Introduction to Haskell development tools](https://github.com/anton-k/haskell-wushu-panda/blob/main/00-build-tools/README.md)

* [Pomodoro timer](https://github.com/anton-k/haskell-wushu-panda/blob/main/01-pomodoro/README.md)

* [Password obfuscation](https://github.com/anton-k/haskell-wushu-panda/blob/main/02-password-obfuscator/README.md)

* Exam engine 
    * [Simple CLI implementation](https://github.com/anton-k/haskell-wushu-panda/blob/main/03-exams/01-exams-simple/README.md)

    * [Making it interactive](https://github.com/anton-k/haskell-wushu-panda/blob/main/03-exams/02-exams-interactive/README.md)

<!---
Wish list of projects:

* Graphics (generative art)

* Oberheim synthesizer emulator (working with audio)

* Memo app
    * In memory implementation

    * DB implementation

* Compiler

    * Lambda calculus language

    * Wrapping to EDSL

    * Parser

    * Type-checker 

    * REPL

    * Compiler

* Toy Blockchain
    
* Web site to manage finances

    * Server (servant)

    * Frontend (reflex/obelisk)

* Boring chapter. ByteString/Text/JSON/whatever conversions

* Hard chapter. Hunting for memory leaks

* Concurrency task. (maybe flow lib revival)

* ig/fb terminal chat over API

* Linux audio configurator

* Web crawler

* Dive into NIX

* Android app??? obelisk

--->

<!---
## Interviews
--->

### How to learn Haskell in practice

Haskell is a great language. There is no doubt about it. But also 
It can be hard to use as it's not only practical but also full of experimental features.
This is cool for a researcher (so many things to explore) but for the novice
it can be easy to get lost among the features.

I think the best way to learn a tool is to try it out and do something cool with it.
We can start a simple project and see how it works and makes our life a bit better.

In this series I hope to help you to learn practical Haskell by studying a 
number of practical and useful tiny apps. In each app we will study some 
practical aspect of this great language.

I have lots of experience with Haskell and I'd like to share it with you.
Of course it's bound to my tastes. So it's great to watch out for many styles
and find the one that suits you. For me I've always wanted to have something 
done with Haskell and to do it as simple as possible.

Maybe you will disagree on design choices or on implementation details this 
is also great. Why not to experiment with it and make it different as you go.

My advice to learn the Haskell is 

* find some cool project that will make you work hard
   for pure fun of it. This way you can learn it faster and be motivated to do it in Haskell.
   Keep it small so not to drawn in it. Get something cool done that you really need.
   It can be small command line tool or web crawler, or even some web site template
   or crypto application. Haskell has wide range of applications.
   Also it's cool to find a buddy to work with. Together it's harder to be lazy.

* study lots of code in the public domain 
   that was written by pros (it's great to start with the `base/prelude` and move
   to the cool libraries like `servant`, `prettyprinter`, `mtl`) 

* collaborate on public projects or on real job. You will get lots of advices from pros and 
  you will be forced to apply them.

* try to explain stuff to somebody else on forums/reddit/chats

* meet haskeller in person (can be a rare bird in your city but it's better to try), 
    go to conferences and hackathons this can expand the horizons.

* don't hang out on forums/reddit/telegram/email/lists too much. 
  It's better to do something and go back to ask
  when you really need the help. Although you can find a job with those tools
  they won't make you a better programmer without application to the real code
  and you can loose lots of time on it and be discouraged by people who seems to know more than you.
  Don't let it discourage you. Bit by bit in small steps you will become a pro
  or just invent a couple of cool apps with this great language.

The course is for a novice who already knows what [Monad](https://github.com/anton-k/monads-for-drummers), Applicative or [Typeclassopedia](https://wiki.haskell.org/Typeclassopedia)
classes are but does not know how to put that knowledge in practice. 
So I'm not going to explain the basics but try to make it as accessible as I can
and demystify some advanced or mundane topics.


