# Fractan
> so you thought that brianf*ck was esoteric.

This repository contains a talk that will be presented at [ScalaUA 2020][scalaua].

## Introduction
[Fractan][fractan] is
> a Turing-complete esoteric programming language invented by the mathematician John Conway. A FRACTRAN program is an ordered list of positive fractions together with an initial positive integer input n. The program is run by updating the integer n as follows:
>
> 1. for the first fraction f in the list for which nf is an integer, replace n by nf
> 2. repeat this rule until no fraction in the list produces an integer when multiplied by n, then halt.

[scalaua]: https://www.scalaua.com/
[fractan]: https://en.wikipedia.org/wiki/FRACTRAN