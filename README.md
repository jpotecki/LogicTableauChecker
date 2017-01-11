# Propositional Logic prover

## Usage
1. Clone and build the stack project ```stack build```
2. Run the programme with commandline args ```stack exec -- tableauChecker --valid "-(p^p)"```

## Flags
1. -h
2. --satisfiable fmla
3. --valid fmla

## Definition of a Fmla
1. A formula is defined as ``` fmla = prop | -prop | (fmla^fmla) | (fmlavfmla) | (fmla>fmla) ```
2. Valid props are all letters 'a' .. 'z' except 'v' ``` prop = delete 'v' $ ['a'..'z'] ```


# Misc
The programme contains an axiom builder, which can be found in ```src/Axioms.hs```
In order to test the correctness of the programme, run ```$stack test``` which will run a couple of 
QuickCheck tests. By importing ```test FormulaTest``` you get an instance of ```Arbitrary``` for 
```Fmla``` which can be used in order to create random formulae. Combine that with the axiom library, and
you can get randomised valid or unsatisfiable formulae.