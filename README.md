# Propositional Logic prover

## Usage
1. Clone and build the stack project ```stack build```
2. Run the programme with commandline args ```stack exec -- tableauChecker --valid "-(p^p)"```

## Flags
1. -h
2. --satisfiable fmla
3. --valid fmla

## Definition of a Fmla
1. A formula is defined as ``` fmla = prop | -prop | (fmla ^ fmla) | (fmla v fmla) | (fmla > fmla) ```
2. Valid props are all letters 'a' .. 'z' except 'v' ``` prop = delete 'v' $ ['a'..'z'] ```