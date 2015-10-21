---
layout: post
title: Write Yourself a Lisp (With Purescript!) Part 1
---

[Write Yourself a Scheme in 48 Hours](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours) is an excellent resource for Haskell newbies. Originally I wanted to read it to understand how parsing in general (and parsing libraries like parsec, in particular) works with functional languages. But I ended up hooked on writing my own lisp.

The tutorial itself is made for people writing Haskell, but really I was interested in learning [purescript-parsing](https://github.com/purescript-contrib/purescript-parsing). So going through the book, I decided to adapt it purescript and purescript-parsing instead of Haskell and parsec.

Is this a terrible idea? That depends on what the motivations are. If you're just looking for a lisp dialect that compiles to JavaScript, there's plenty of options that are production-ready (look at, for example, [ClojureScript](https://github.com/clojure/clojurescript). But if you want to understand a little more about how language design works, then writing your own is one of the most instructive exercises. 

So, if you're here because you're looking for a lisp dialect to write JavaScript in, I'd suggest looking elsewhere. Otherwise, this series hopes to be a way to introduce (or refresh) the reader on a number of things --- the original Write Yourself a Scheme is aimed at people who want to learn Haskell and know some Lisp, or people who have a strong quantitave background but no programming experience. I hope this series will be accessible to people who want to learn PureScript and know JavaScript, or people who know Haskell and want to learn PureScript. With that said, let's being with the ...

## First steps

You'll need to have [PureScript](https://github.com/purescript/purescript) (>= 0.7) installed and preferrably [Pulp](https://github.com/pulp/pulp) (>= 4.0.2) for building and running purescript files. If installing both of those doesn't also entail having node.js or iojs installed, those will be required as well. This series was written on Linux, but there isn't anything in principle that would prevent it from working on Mac OS or Windows.

Now it's time to start with the obligatory "Hello, World" example. Make a new folder and run `pulp init` inside; this will create a skeleton `browser.json` file that will be used to control dependencies and install a few core PureScript libraries. It will also create a few folders and the file `src/Main.purs`, so open up that file in your favorite text editor. You'll notice it already has what looks like a "Hello, World" example --- you can run it with `pulp run` from your project's root folder. Assuming everything was installed without problems, you should see "Hello sailor!" printed to the console.

The rest of the series won't be as step-by-step as the above. I'll assume you have Write Yourself a Scheme open in the background for reference --- after all PureScript is written in (and highly inspired by) Haskell, so much of that book applies here --- but I'll still make an effort to explain PureScript for those unfamiliar with either it or Haskell.
