# Profiling Coconut Code

## Prerequisites

- Have the code to be executed included in `app/Main.hs`

## Instructions

Run the following:

```bash
stack build --profile
stack exec --profile -- Coconut-exe +RTS -hc -p -K1k
```

- -hc enables heap profiling
- -K1k sets the stack size to 1k

## Results

- Coconut-exe.prof shows profiling information (even if the program was terminated halfway through execution)
- To see heap profiling information, run `hp2ps -e8in -c Coconut-exe.hp`.
  - This generates a PostScript file `Coconut-exe.ps`
  - To my knowledge, you can either run Pandoc to convert to a readable file format, or use `evince` to
	view the file
