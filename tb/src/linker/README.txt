Linkers are mostly OS-indepedent ngl, like yea i gotta parse the shitty object files into execs but layouting,
generating code stubs, and doing relocations are basically the same everywhere once the parsing is done.

Pipeline:

  * Importing (Extremely parallel): Call import functions, each of these will extract the symbols & relocations into memory.
    The section data isn't read but we do keep track of which files and what ranges they exist in
    for layouting reasons.

  * Layout step (Not so parallel): We use whatever user-defined ordinals to sort the data into a consistent ordering, from there
    we know the complete size of the final file (useful for file map writing).

  * Export step (Extremely parallel):

