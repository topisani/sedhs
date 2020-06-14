# Features

- [x] Multiple commands separated by newlines/semicolon
- [ ] Errors!
- [ ] Command line flags
  - [ ] Suppress default output
  - [ ] Read script from file
- [ ] Address negation with `!`
- [ ] REGEX addresses

**Note:**
> Implementors are encouraged to provide warning messages about labels that are never referenced by a b or t command, jumps to labels that do not exist, and label arguments that are subject to truncation. 

# Functions

- [x] { - Execute a list of sed editing commands only when the pattern space is selected
- [ ] a - Write text to standard output as described previously.
- [ ] b - Branch to the : command verb bearing the label argument
- [x] c - Delete the pattern space, place text on the output and start the next cycle.
- [x] d - Delete the pattern space and start the next cycle.
- [ ] D - delete the pattern space through the first <newline>, and start the next cycle without reading any new input.
- [x] g - Replace the contents of the pattern space with the hold space
- [x] G - Append to the pattern space a <newline> followed by the contents of the hold space.
- [x] h - Replace the contents of the hold space with the contents of the pattern space.
- [x] H - Append to the hold space a <newline> followed by the contents of the pattern space.
- [ ] i - Write text to standard output
- [ ] l - Write the pattern space to standard output in a visually unambiguous form
- [ ] n - Write pattern space to output and read the next line of input
- [ ] N - Append the next line of input to the pattern space
- [x] p - Write the pattern space to standard output.
- [x] P - Write the pattern space, up to the first <newline>, to standard output.
- [ ] q - Branch to the end of the script and quit without starting a new cycle.
- [ ] r - Copy the contents of rfile to standard output as described previously.
- [ ] s - Substitute the replacement string for instances of the BRE in the pattern space.
- [ ] t - Branch to the : command verb bearing the label if any substitutions have been made recently.
- [ ] w - Append (write) the pattern space to wfile.
- [x] x - Exchange the contents of the pattern and hold spaces.
- [ ] y - Replace all occurrences of characters in string1 with the corresponding characters in string2.
- [ ] : - Do nothing. This command bears a label to which the b and t commands branch.
- [x] = - Write the line number to standard output.
- [x] # - Comment
