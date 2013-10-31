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

```
java -jar PropositionalCalculus.jar -m f -i tests_pf/6.in -o tests_pf/6.out
```

