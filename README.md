Recurse functions
======================
Task #6

Per-file description:
* DataTypes.hs - declaration of type Nat, a bit strange implementation of
  Peano's natural numbers (it's really weird, but proper for the task we need
them)
* Primitives.hs - declaration of recurse function primitives
* Basics.hs - declaration of some helpers, written in terms of recursive
  functions, we will use to construct the bigger ones
* Functions.hs - implementation of task
* Playground.hs - examples of usage

This code is assumed to be tested in ghci by loading Playground.hs.
Template Haskell language extensions was actively used, writing the code. All
usages are in way of implementing some function f :: Int -> Int -> Q Exp, which
defines <<code generator>>, that can be later being used by calling $(f n m).
Code generators are used to provide type-correct functions with exact number of
arguments (like e.g. U primitive: $(u 3 2) generates code for U_3^2)


Propositional Calculus
======================

Tasks #1, #2, #3

Jar: https://dl.dropboxusercontent.com/u/3693476/PropositionalCalculus.jar


## Simple proof check

```
java -jar PropositionalCalculus.jar -m pc -i tests_pc/1.in -o tests_pc/1.out
```


## Deduction expand

Optional parametrs: 
* -i <file> (input file, input.txt by default)
* -o <file> (output file, output.txt by default)
* -n <number> (expand <number> last assumptions, 1 by default)
* -na (expand all assumptions)
* -math/-alt (alternate mode of printing, with characters ∧∨→¬)
* -npc (Do not print comments)
* -nr (Do not reduce unused expressions in proof, not recommended to use)

```
java -jar PropositionalCalculus.jar -m de -i tests_de/3.in -o tests_de/3.out
```



## Proof find

Optional parametrs: 
* -i <file> (input file, input.txt by default)
* -o <file> (output file, output.txt by default)
* -math/-alt (alternate mode of printing, with characters ∧∨→¬)
* -npc (Do not print comments)
* -nr (Do not reduce unused expressions in proof, not recommended to use)
* -pi (Print lengths of base proofs)

```
java -jar PropositionalCalculus.jar -m f -i tests_pf/6.in -o tests_pf/6.out
```

