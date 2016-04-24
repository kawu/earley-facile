earley-facile
=============

**earley-facile** is a simple command-line tool which allows to interactively
parse context-free grammars with the Earley algorithm.


Installation
------------

It is recommended to install *earley-facile* using the [Haskell Tool
Stack][stack], which you will need to install on your machine beforehand.
Then clone this repository into a local directory and run:

    stack install

The command will install the *earley-facile* tool to a directory which depends
on your OS.  On linux, the default target directory is `~/.local/bin`, thus the
installed application will be available at:

    ~/.local/bin/earley-facile

Usage
-----

The *earley-facile* package provides a command-line tool which can be used to
read CFG grammars from files and to use them to interactively parse input
sentences.

For instance, to parse "John drove the car" with the grammar stored in the
`grammars/car.txt` file, run:

    earley-facile -i "John drove the car" -c grammars/car.txt

To see all options of the command-line tool, run:

    earley-facile --help

The tool doesn't provide access to the input history by itself.
On linux, you can use a dedicated tool called `rlwrap` to solve
this issue, e.g.:

    rlwrap earley-facile -i "John drove the car" -c grammars/car.txt

### Format

Here's an example of a toy CFG grammar:
```
# GRAMMAR
S  -> NP VP
NP -> det noun
NP -> pnoun
VP -> verb
VP -> verb NP
VP -> verb PP
VP -> verb NP PP
PP -> prep NP

# LEXICON
drove 	<- verb
John 	<- pnoun
car 	<- noun
station <- noun
to 	<- prep
the 	<- det
```

CFG rules are defined using the `->` arrow, with the head specified on the
left of the arrow and the body of the rule on its right.
All non-terminals must start with upper-case letters, while terminals (POS tags)
with lower-case letters.

The lexicon part, which relates concrete words with the corresponding POS tags,
is defined for convenience, even though the actuall words do not play any
role in the underlying parsing algorithm.

You can find the grammar given above and other grammar examples in
the `grammar` directory of this repository.

### Example

Once you install *earley-facile* and run it using:

    rlwrap earley-facile -i "John drove the car" -c grammars/car.txt

you will enter into the parsing session during which you will be able to (a)
query the underlying parsing chart, containing all the items generated so far,
and (b) ask the tool to fire inference rules with respect to selected items.

##### Axiom

At the very beginning of the parsing process the chart is empty.
The only inference rule which can be used at this stage is the axiom rule.
It takes a single argument specifying the root non-terminal to be
matched against the input sentence.
For instance, to (eventually) find analyses containing non-terminal `S` in
their roots, run:
```
> axiom "S"
>
```

##### Print

You can immediatelly check if `axiom` has modified the underlying chart
by using the `print` command: 
```
> print 0
<0> ("S" ->  * "NP" "VP", 0, 0)
>
```
The `print` command takes one obligatory argument -- the column of
the chart to be inspected.

##### Process

The key operation of the parser is implemented by the `process` command,
which takes as argument the ID of the chart item and considers wheter
any of the three inference rules of the Earley algorithm can apply to
this item.
In the example below, only prediction can be applied w.r.t. item `0`.
```
> process 0
[P] ("NP" ->  * "det" "noun", 0, 0)
[P] ("NP" ->  * "pnoun", 0, 0)
> print 0
#0# ("S" ->  * "NP" "VP", 0, 0)
<1> ("NP" ->  * "det" "noun", 0, 0)
<2> ("NP" ->  * "pnoun", 0, 0)
>
```

In particular, the `process` command searches for all (already
processed, marked with #_#) chart items which the specified item can
complement, as in the example below:
```
> process 1
> process 2
[S] ("NP" -> "pnoun" * , 0, 1)
> print 0
#0# ("S" ->  * "NP" "VP", 0, 0)
#1# ("NP" ->  * "det" "noun", 0, 0)
#2# ("NP" ->  * "pnoun", 0, 0)
> print 1
<3> ("NP" -> "pnoun" * , 0, 1)
> process 3
[C] ("S" ->  * "NP" "VP", 0, 0) => ("S" -> "NP" * "VP", 0, 1)
> print 1
#3# ("NP" -> "pnoun" * , 0, 1)
<4> ("S" -> "NP" * "VP", 0, 1)
>
```

The trace of the applications of the inference rules is stored in the
underlying chart as well.  To see how items of the given column were
derived, use the `-v` flag of the `print` command:
```
> print -v 1
#3# ("NP" -> "pnoun" * , 0, 1)
  * [S] ("NP" ->  * "pnoun", 0, 0)
<4> ("S" -> "NP" * "VP", 0, 1)
  * [C] ("S" ->  * "NP" "VP", 0, 0) + ("NP" -> "pnoun" * , 0, 1)
>
```

##### Forest

The `forest` command allows to see the list of all trees represented by the
given chart item:
```
> forest 4
Left "NP"
|
`- Right "pnoun"

>
```
Once you get to the very end of the parsing process, you will be able to obtain
parse forests for items representing analyses matching the entire input
sentence:
```
...
> print 4
#18# ("NP" -> "det" "noun" * , 2, 4)
#19# ("VP" -> "verb" "NP" * , 1, 4)
#20# ("VP" -> "verb" "NP" * "PP", 1, 4)
#21# ("S" -> "NP" "VP" * , 0, 4)
#22# ("PP" ->  * "prep" "NP", 4, 4)
> forest 21
Left "NP"
|
`- Right "pnoun"

Left "VP"
|
+- Right "verb"
|
`- Left "NP"
   |
   +- Right "det"
   |
   `- Right "noun"

> 
```


[stack]: http://docs.haskellstack.org "Haskell Tool Stack"
