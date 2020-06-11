# sedhs

An implementation of `sed`, written in haskell

# POSIX `sed` Specification

Copied from https://pubs.opengroup.org/onlinepubs/9699919799/utilities/sed.html

## SYNOPSIS

```sh
sed [-n] script [file...]
sed [-n] -e script [-e script]... [-f script_file]... [file...]
sed [-n] [-e script]... -f script_file [-f script_file]... [file...]
```
## DESCRIPTION

The _sed_ utility is a stream editor that shall read one or more text files, make editing changes according to a script of editing commands, and write the results to standard output. The script shall be obtained from either the _script_ operand string or a combination of the option-arguments from the **\-e** _script_ and **\-f** _script\_file_ options.

## OPTIONS

The _sed_ utility shall conform to XBD [_Utility Syntax Guidelines_](../basedefs/V1_chap12.html#tag_12_02) , except that the order of presentation of the **\-e** and **\-f** options is significant.

The following options shall be supported:

**\-e ** _script_

Add the editing commands specified by the _script_ option-argument to the end of the script of editing commands.

**\-f ** _script\_file_

Add the editing commands in the file _script\_file_ to the end of the script of editing commands.

**\-n**

Suppress the default output (in which each line, after it is examined for editing, is written to standard output). Only lines explicitly selected for output are written.

If any **\-e** or **\-f** options are specified, the script of editing commands shall initially be empty. The commands specified by each **\-e** or **\-f** option shall be added to the script in the order specified. When each addition is made, if the previous addition (if any) was from a **\-e** option, a <newline> shall be inserted before the new addition. The resulting script shall have the same properties as the _script_ operand, described in the OPERANDS section.

## OPERANDS

The following operands shall be supported:

_file_

A pathname of a file whose contents are read and edited. If multiple _file_ operands are specified, the named files shall be read in the order specified and the concatenation shall be edited. If no _file_ operands are specified, the standard input shall be used.

_script_

A string to be used as the script of editing commands. The application shall not present a _script_ that violates the restrictions of a text file except that the final character need not be a <newline>.

## STDIN

The standard input shall be used if no _file_ operands are specified, and shall be used if a _file_ operand is '-' and the implementation treats the '-' as meaning standard input. Otherwise, the standard input shall not be used. See the INPUT FILES section.

## INPUT FILES

The input files shall be text files. The _script\_file_s named by the **\-f** option shall consist of editing commands.

## ENVIRONMENT VARIABLES

The following environment variables shall affect the execution of _sed_:

_LANG_

Provide a default value for the internationalization variables that are unset or null. (See XBD [_Internationalization Variables_](../basedefs/V1_chap08.html#tag_08_02) for the precedence of internationalization variables used to determine the values of locale categories.)

_LC\_ALL_

If set to a non-empty string value, override the values of all the other internationalization variables.

_LC\_COLLATE_

Determine the locale for the behavior of ranges, equivalence classes, and multi-character collating elements within regular expressions.

_LC\_CTYPE_

Determine the locale for the interpretation of sequences of bytes of text data as characters (for example, single-byte as opposed to multi-byte characters in arguments and input files), and the behavior of character classes within regular expressions.

_LC\_MESSAGES_

Determine the locale that should be used to affect the format and contents of diagnostic messages written to standard error.

_NLSPATH_

\[[XSI](javascript:open_code('XSI'))\] ![[Option Start]](../images/opt-start.gif) Determine the location of message catalogs for the processing of _LC\_MESSAGES._ ![[Option End]](../images/opt-end.gif)

## ASYNCHRONOUS EVENTS

Default.

## STDOUT

The input files shall be written to standard output, with the editing commands specified in the script applied. If the **\-n** option is specified, only those input lines selected by the script shall be written to standard output.

## STDERR

The standard error shall be used only for diagnostic and warning messages.

## OUTPUT FILES

The output files shall be text files whose formats are dependent on the editing commands given.

## EXTENDED DESCRIPTION

The _script_ shall consist of editing commands of the following form:

**\[**_address_**\[**,_address_**\]\]**_function_

where _function_ represents a single-character command verb from the list in _Editing Commands in sed_, followed by any applicable arguments.

The command can be preceded by <blank> characters and/or <semicolon> characters. The function can be preceded by <blank> characters. These optional characters shall have no effect.

In default operation, _sed_ cyclically shall append a line of input, less its terminating <newline> character, into the pattern space. Reading from input shall be skipped if a <newline> was in the pattern space prior to a **D** command ending the previous cycle. The _sed_ utility shall then apply in sequence all commands whose addresses select that pattern space, until a command starts the next cycle or quits. If no commands explicitly started a new cycle, then at the end of the script the pattern space shall be copied to standard output (except when **\-n** is specified) and the pattern space shall be deleted. Whenever the pattern space is written to standard output or a named file, _sed_ shall immediately follow it with a <newline>.

Some of the editing commands use a hold space to save all or part of the pattern space for subsequent retrieval. The pattern and hold spaces shall each be able to hold at least 8192 bytes.

### Addresses in sed

An address is either a decimal number that counts input lines cumulatively across files, a '$' character that addresses the last line of input, or a context address (which consists of a BRE, as described in [Regular Expressions in sed](#tag_20_116_13_02), preceded and followed by a delimiter, usually a <slash>).

An editing command with no addresses shall select every pattern space.

An editing command with one address shall select each pattern space that matches the address.

An editing command with two addresses shall select the inclusive range from the first pattern space that matches the first address through the next pattern space that matches the second. (If the second address is a number less than or equal to the line number first selected, only one line shall be selected.) Starting at the first line following the selected range, _sed_ shall look again for the first address. Thereafter, the process shall be repeated. Omitting either or both of the address components in the following form produces undefined results:

**\[**_address_**\[**,_address_**\]\]**

### Regular Expressions in sed

The _sed_ utility shall support the BREs described in XBD [_Basic Regular Expressions_](../basedefs/V1_chap09.html#tag_09_03), with the following additions:

*   In a context address, the construction "\\cBREc", where _c_ is any character other than <backslash> or <newline>, shall be identical to "/BRE/". If the character designated by _c_ appears following a <backslash>, then it shall be considered to be that literal character, which shall not terminate the BRE. For example, in the context address "\\xabc\\xdefx", the second _x_ stands for itself, so that the BRE is "abcxdef".
    
*   The escape sequence '\\n' shall match a <newline> embedded in the pattern space. A literal <newline> shall not be used in the BRE of a context address or in the substitute function.
    
*   If an RE is empty (that is, no pattern is specified) _sed_ shall behave as if the last RE used in the last command applied (either as an address or as part of a substitute command) was specified.
    

### Editing Commands in sed

In the following list of editing commands, the maximum number of permissible addresses for each function is indicated by \[ _0addr_\], \[ _1addr_\], or \[ _2addr_\], representing zero, one, or two addresses.

The argument _text_ shall consist of one or more lines. Each embedded <newline> in the text shall be preceded by a <backslash>. Other <backslash> characters in text shall be removed, and the following character shall be treated literally.

The **r** and **w** command verbs, and the _w_ flag to the **s** command, take an _rfile_ (or _wfile_) parameter, separated from the command verb letter or flag by one or more <blank> characters; implementations may allow zero separation as an extension.

The argument _rfile_ or the argument _wfile_ shall terminate the editing command. Each _wfile_ shall be created before processing begins. Implementations shall support at least ten _wfile_ arguments in the script; the actual number (greater than or equal to 10) that is supported by the implementation is unspecified. The use of the _wfile_ parameter shall cause that file to be initially created, if it does not exist, or shall replace the contents of an existing file.

The **b**, **r**, **s**, **t**, **w**, **y**, and **:** command verbs shall accept additional arguments. The following synopses indicate which arguments shall be separated from the command verbs by a single <space>.

The **a** and **r** commands schedule text for later output. The text specified for the **a** command, and the contents of the file specified for the **r** command, shall be written to standard output just before the next attempt to fetch a line of input when executing the **N** or **n** commands, or when reaching the end of the script. If written when reaching the end of the script, and the **\-n** option was not specified, the text shall be written after copying the pattern space to standard output. The contents of the file specified for the **r** command shall be as of the time the output is written, not the time the **r** command is applied. The text shall be output in the order in which the **a** and **r** commands were applied to the input.

Editing commands other than **{...}**, **a**, **b**, **c**, **i**, **r**, **t**, **w**, **:**, and **#** can be followed by a <semicolon>, optional <blank> characters, and another editing command. However, when an **s** editing command is used with the _w_ flag, following it with another command in this manner produces undefined results.

A function can be preceded by a '!' character, in which case the function shall be applied if the addresses do not select the pattern space. Zero or more <blank> characters shall be accepted before the '!' character. It is unspecified whether <blank> characters can follow the '!' character, and conforming applications shall not follow the '!' character with <blank> characters.

If a _label_ argument (to a **b**, **t**, or **:** command) contains characters outside of the portable filename character set, or if a _label_ is longer than 8 bytes, the behavior is unspecified. The implementation shall support _label_ arguments recognized as unique up to at least 8 bytes; the actual length (greater than or equal to 8) supported by the implementation is unspecified. It is unspecified whether exceeding the maximum supported label length causes an error or a silent truncation.

**\[**_2addr_**\] {**_editing command_

_editing command_

...

**}**

Execute a list of _sed_ editing commands only when the pattern space is selected. The list of _sed_ editing commands shall be surrounded by braces. The braces can be preceded or followed by <blank> characters. The <right-brace> shall be preceded by a <newline> or <semicolon> (before any optional <blank> characters preceding the <right-brace>).

Each command in the list of commands shall be terminated by a <newline> character, or by a <semicolon> character if permitted when the command is used outside the braces. The editing commands can be preceded by <blank> characters, but shall not be followed by <blank> characters.

**\[**_1addr_**\]a\\** 
_text_

Write text to standard output as described previously.

**\[**_2addr_**\]b \[**_label_**\]**

Branch to the **:** command verb bearing the _label_ argument. If _label_ is not specified, branch to the end of the script.

**\[**_2addr_**\]c\\**

text_

Delete the pattern space. With a 0 or 1 address or at the end of a 2-address range, place _text_ on the output and start the next cycle.

**\[**_2addr_**\]d**

Delete the pattern space and start the next cycle.

**\[**_2addr_**\]D**

If the pattern space contains no <newline>, delete the pattern space and start a normal new cycle as if the **d** command was issued. Otherwise, delete the initial segment of the pattern space through the first <newline>, and start the next cycle with the resultant pattern space and without reading any new input.

**\[**_2addr_**\]g**

Replace the contents of the pattern space by the contents of the hold space.

**\[**_2addr_**\]G**

Append to the pattern space a <newline> followed by the contents of the hold space.

**\[**_2addr_**\]h**

Replace the contents of the hold space with the contents of the pattern space.

**\[**_2addr_**\]H**

Append to the hold space a <newline> followed by the contents of the pattern space.

**\[**_1addr_**\]i\\**

_text_

Write _text_ to standard output.

**\[**_2addr_**\]l**

(The letter ell.) Write the pattern space to standard output in a visually unambiguous form. The characters listed in XBD [_Escape Sequences and Associated Actions_](../basedefs/V1_chap05.html#tagtcjh_2) ( '\\\\', '\\a', '\\b', '\\f', '\\r', '\\t', '\\v' ) shall be written as the corresponding escape sequence; the '\\n' in that table is not applicable. Non-printable characters not in that table shall be written as one three-digit octal number (with a preceding <backslash>) for each byte in the character (most significant byte first).

Long lines shall be folded, with the point of folding indicated by writing a <backslash> followed by a <newline>; the length at which folding occurs is unspecified, but should be appropriate for the output device. The end of each line shall be marked with a '$'.

**\[**_2addr_**\]n**

Write the pattern space to standard output if the default output has not been suppressed, and replace the pattern space with the next line of input, less its terminating <newline>.

If no next line of input is available, the **n** command verb shall branch to the end of the script and quit without starting a new cycle.

**\[**_2addr_**\]N**

Append the next line of input, less its terminating <newline>, to the pattern space, using an embedded <newline> to separate the appended material from the original material. Note that the current line number changes.

If no next line of input is available, the **N** command verb shall branch to the end of the script and quit without starting a new cycle or copying the pattern space to standard output.

**\[**_2addr_**\]p**

Write the pattern space to standard output.

**\[**_2addr_**\]P**

Write the pattern space, up to the first <newline>, to standard output.

**\[**_1addr_**\]q**

Branch to the end of the script and quit without starting a new cycle.

**\[**_1addr_**\]r ** _rfile_

Copy the contents of _rfile_ to standard output as described previously. If _rfile_ does not exist or cannot be read, it shall be treated as if it were an empty file, causing no error condition.

**\[**_2addr_**\]s/**_BRE_**/**_replacement_**/**_flags_

Substitute the replacement string for instances of the BRE in the pattern space. Any character other than <backslash> or <newline> can be used instead of a <slash> to delimit the BRE and the replacement. Within the BRE and the replacement, the BRE delimiter itself can be used as a literal character if it is preceded by a <backslash>.

The replacement string shall be scanned from beginning to end. An <ampersand> ( '&' ) appearing in the replacement shall be replaced by the string matching the BRE. The special meaning of '&' in this context can be suppressed by preceding it by a <backslash>. The characters "\\_n"_, where _n_ is a digit, shall be replaced by the text matched by the corresponding back-reference expression. If the corresponding back-reference expression does not match, then the characters "\\_n"_ shall be replaced by the empty string. The special meaning of "\\_n"_ where _n_ is a digit in this context, can be suppressed by preceding it by a <backslash>. For each other <backslash> encountered, the following character shall lose its special meaning (if any).

A line can be split by substituting a <newline> into it. The application shall escape the <newline> in the replacement by preceding it by a <backslash>.

The meaning of an unescaped <backslash> immediately followed by any character other than '&', <backslash>, a digit, <newline>, or the delimiter character used for this command, is unspecified.

A substitution shall be considered to have been performed even if the replacement string is identical to the string that it replaces. Any <backslash> used to alter the default meaning of a subsequent character shall be discarded from the BRE or the replacement before evaluating the BRE or using the replacement.

The value of _flags_ shall be zero or more of:

_n_

Substitute for the _n_th occurrence only of the BRE found within the pattern space.

**g**

Globally substitute for all non-overlapping instances of the BRE rather than just the first one. If both **g** and _n_ are specified, the results are unspecified.

**p**

Write the pattern space to standard output if a replacement was made.

**w ** _wfile_

Write. Append the pattern space to _wfile_ if a replacement was made. A conforming application shall precede the _wfile_ argument with one or more <blank> characters. If the **w** flag is not the last flag value given in a concatenation of multiple flag values, the results are undefined.

**\[**_2addr_**\]t \[**_label_**\]**

Test. Branch to the **:** command verb bearing the _label_ if any substitutions have been made since the most recent reading of an input line or execution of a **t**. If _label_ is not specified, branch to the end of the script.

**\[**_2addr_**\]w ** _wfile_

Append (write) the pattern space to _wfile_.

**\[**_2addr_**\]x**

Exchange the contents of the pattern and hold spaces.

**\[**_2addr_**\]y/**_string1_**/**_string2_**/**

Replace all occurrences of characters in _string1_ with the corresponding characters in _string2_. If a <backslash> followed by an 'n' appear in _string1_ or _string2_, the two characters shall be handled as a single <newline>. If the number of characters in _string1_ and _string2_ are not equal, or if any of the characters in _string1_ appear more than once, the results are undefined. Any character other than <backslash> or <newline> can be used instead of <slash> to delimit the strings. If the delimiter is not 'n', within _string1_ and _string2_, the delimiter itself can be used as a literal character if it is preceded by a <backslash>. If a <backslash> character is immediately followed by a <backslash> character in _string1_ or _string2_, the two <backslash> characters shall be counted as a single literal <backslash> character. The meaning of a <backslash> followed by any character that is not 'n', a <backslash>, or the delimiter character is undefined.

**\[**_0addr_**\]:**_label_

Do nothing. This command bears a _label_ to which the **b** and **t** commands branch.

**\[**_1addr_**\]=**

Write the following to standard output:

"%d\\n", <_current line number_\> 

**\[**_0addr_**\]**

Ignore this empty command.

**\[**_0addr_**\]#**

Ignore the '#' and the remainder of the line (treat them as a comment), with the single exception that if the first two characters in the script are "#n", the default output shall be suppressed; this shall be the equivalent of specifying **\-n** on the command line.

## EXIT STATUS

The following exit values shall be returned:

 0

Successful completion.

\>0

An error occurred.

## CONSEQUENCES OF ERRORS

Default.
