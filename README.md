# Fractan
> so you thought that brianf*ck was esoteric.

This repository contains a talk that will be presented at [ScalaUA 2020][scalaua].

## Introduction
[Fractan][fractan] is
> a Turing-complete esoteric programming language invented by the mathematician John Conway. A FRACTRAN program is an ordered list of positive fractions together with an initial positive integer input n. The program is run by updating the integer n as follows:
>
> 1. for the first fraction f in the list for which nf is an integer, replace n by nf
> 2. repeat this rule until no fraction in the list produces an integer when multiplied by n, then halt.

## Development
We use [remark][] as a presentation tool. If you want to make changes to the presentation one should look at [`presentation.md`][content], which contains the source.

### Serving locally
One needs to serve the files locally to be able to preview the presentation. One way is to use [miniserve][]. After downloading and installing one could use the following command

```bash
miniserve --port 3722 docs
```

And go to [`http://localhost:3722/index.html`](http://localhost:3722/index.html).

[scalaua]: https://www.scalaua.com/
[fractan]: https://en.wikipedia.org/wiki/FRACTRAN
[remark]: https://remarkjs.com/#1
[content]: https://github.com/fifth-postulate/fractan/blob/master/docs/presentation.md
[miniserve]: https://github.com/svenstaro/miniserve