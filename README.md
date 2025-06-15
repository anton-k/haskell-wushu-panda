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

Haskell is an amazing language. It blends powerful theoretical concepts with experimental features, which can make
it feel complex for beginners, but that’s part of its richness.

I recommend starting by building real projects—begin with small ones—to build practical skills without getting
bogged down in theory or overwhelming details. This active approach helps you apply what you know and retain it
better than passive study alone.

In this series, I'll guide you through learning **practical** Haskell using tiny apps that demonstrate specific
concepts like functors, applicative functors, and typeclasses by showing how they work *in practice*. We’ll focus
on hands-on application to make theory accessible and clear.

Haskell can be used for so many things — small command-line tools, web crawlers, site templates, crypto apps and
that’s one of its greatest strengths.

* **Collaborate on open projects or tackle work tasks**. Working alongside experienced developers (or even with
   colleagues at your job) gives you valuable feedback and helps you apply best practices faster than reading about
   them.

* Go beyond just studying the basics by diving into public code—especially the base/prelude—and gradually
   exploring cool libraries like `servant`, `prettyprinter`, or `mtl`. It’s wonderful to see how concepts from **the
   base** evolve in real-world applications.

   Haskell has a fantastic ecosystem. Trying out these tools will help you build confidence and understand what
   really matters for practical programming—without getting stuck on overly abstract details.

*   Collaborate, contribute, and get advice from pros! This forces you to actually apply your knowledge—and learn
   how to balance theory with real-world needs.

   But **don’t forget**: if learning is all about reading or chatting online (especially in lists), it can be
   ineffective. It’s better to start a project yourself—get something cool done that solves a problem, even a small
   one—and go back to the internet only when you need help. While knowing how to ask questions and discuss ideas with
   experienced developers on forums like Reddit or chats is valuable, spending too much time there might slow down
   your progress.

* Try explaining what you’ve learned (or are trying) to someone else—whether it's in a forum, chat room, or just
   talking to a friend. Teaching solidifies understanding and gives you confidence.

*  Meet fellow Haskell enthusiasts! While they may be rare locally, joining local meetups, conferences, or
   hackathons can open up incredible opportunities—and learning from others broadens your horizons exponentially.

The audience for this course is the **novice** developer who already understands what Monads, Applicatives, and
Typeclasses are—but hasn't figured out how to use them in real projects. Therefore, I will not explain basics or
dive into theory-only topics like `Typeclassopedia`. Instead, my goal here is simple: help you put that knowledge
into practice while keeping things as accessible and clear as possible.
