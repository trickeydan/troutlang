# Trout Interpreter

The interpreter for Trout.

### Philosophy

Trout is built to be both minimal and complete. Specifically, it is meant to contain the minimal set of features to complete arbitrary tasks withing the problem domain, and to complete them well.

This approach makes Trout a relatively simple but quite powerful language.

### Operation

Trout is an interpreted DSL that processes streams of integers. The streams of integers are fed to the interpreter on `stdin` and the results of the program will appear on `stdout`. For every line that is given on `stdin`, there will be exactly one line of output. That is, the length of the input and output streams are equal.


### Getting Started

In order to run a Trout program, simply execute the Trout interpreter with the Trout file as its only argument. The interpreter will then parse the file and use `stdin` and `stdout` to take input and emit output respectively. On most unix systems this can be accomplished with:

`./troutc myfile.spl`

Where `troutc` is the Trout interpreter in the current directory, and `myfile.spl` is the Trout program.

### Integers

Integers are the main primitive data type in Trout.

The integers in Trout are standard system integers (generally 32 or 64 bits, guaranteed to be at least 30 bits) so integer overflows are possible, though this should not be an issue in normal use and does not affect the power of the language, since it is possible to dynamically allocate more integers.

We can manipulate integers using various operations:

- Addition: `7 + 8 == 15`
- Subtraction: `10 - 2 == 8`
- Multiplication: `13 * 12 == 156`
- Division[^1]: `14 / 2 == 7`

Integer operators can also be used to manipulate integer variables: `a * a == 4`

We can use brackets to do complex calculations: `((a * b) / 3) + (-17)`

[^1]: Division is integer division only, any remainders will be discarded.

### Variables and Assignment

Using the `=` operator, we can assign values to variables. At the end of the following program:

```
x = 4
y = 5
x = x * y
```

`x` is equal to 20 and `y` is equal to 5.

Variable names may contain letters and numbers but have to start with a letter, and cannot clash with reserved words (essentially just `if`).

There are also a number of special variables, `#`, `IN`, integers encapsulated in `[]` and `<>` which will be explained later, and `_` which is the null variable (similar to "hole" in Haskell).

Assignment to `_` has no effect, and attempting to read from `_` produces a runtime error. At first glance this appears to be completely useless, however as we will see in the next section, it has a very specific purpose in Trout.


### Printing

Trout is a print-by-default language. Specifically, if a statement evaluates to a value, and that value is not assigned or otherwise passed to another statement, it is printed.

The rationale behind this is conciseness - most realistic stream processing tasks are expected to be reasonably simple, where a few variables are likely to be assigned, and then evaluated and their result printed. It therefore makes sense to have *not printing* be the difficult thing to do, since a Trout novice is unlikely to ever need to explicitly prevent printing, while an experienced Trout developer can handle the added complexity.

To summarise:

```
x = 4
y = 3
x * y
```

Prints `12` because the statement `x*y` evaluates to 12, and so is printed by default.

So, if we want to have a statement be *evaluated* but neither printed nor assigned, we need to use the null assignment.

```
x = 4
y = 3
_ = x * y
```

This example does not print anything.

For simple programs, suppressing printing is not required, but it becomes quite useful when attempting to efficiently perform useful computation.

Since Trout is dynamically typed and variables can be reassigned, this functionality does not actually add any power to the language, as the user could simply assign to some arbitrary variable (e.g. `null` or `empty`) instead, but having an explicit null variable that cannot be read from makes programs which use it much clearer.

### Blocking IO

In the above section we discussed printing. However, if we take the most trivial program:

```
1
```

We find that execution *does not* immediately print `1` as expected. Instead, we must provide a line of input, at which point the program immediately prints `1` and terminates.

The reason for this is that IO in trout is bi-directionally blocking, but both directions block on *reads*.

Blocking input is a fairly standard concept, and it also exists in Trout - if a computation requires a piece of input, then execution will be blocked until that input arrives on `stdin`.

Trout also has output which blocks on reads from `stdin`. Specifically, if a print occurs that would mean more lines are output than were receieved as input, that print operation is blocked until further input is received, or the input is terminated in which case the program also terminates.

This is why our trivial `1` example requires a line of input - a print occurs, but rather than outputting, it blocks until a line of input is received. Then, as the end of the program has been reached with no further reads, it terminates.

This method of blocking output has two interesting effects - it ensures that we can never have a program output more lines than are input (which is defined as correct behaviour) and it ensures we don't perform obviously unnecessary computation once we know what to print next. The latter behaviour introduces a form of "imperative laziness" which is discussed further in an appendix.

### Booleans

While integers are the primary data type, it is sometimes helpful to evaluate boolean expressions. In the spirit of completeness therefore, trout supports boolean expressions, but in the spirit of minimalism, it only supports them as part of `if` statements.

The basic boolean expressions are `True` and `False`.

We can perform basic logic on bools such as:

- Not: `!False == True`
- Or: `True | False == True`
- And: `True . False == False`

We can also construct a boolean from a comparison:

- Equality: `3 == 3 == True`
- Inequality: `14 != 12 == True`
- Greater than / Less than: `12 > 1 == True`

We can chain these together to build more complex logic: `(6 < a . 3 > b) | b == a`

### Decisions

As mentioned before, trout supportsd if statements. The syntax of an if statement is as follows:

`if (<boolean expression>) <statement>`

So for example, one can write `if (x>3) y=50` to set y to 50 if x is greater than 3.

The `if` statement syntax only supports one following statement, and C-style code blocks with curly braces are not *technically* supported, but certain statements allow workarounds for this restriction.

As a result of the if statement being itself a statement, it is also possible to chain ifs like so:

`if (x>3) if(y>4) z=53`

However this is simply an equivalent and less legible way of writing:

`if (x>3 | y>4) z=53`

### Frames

Here we introduce our first piece of Trout-specific lingua: frames. Let's say we have three parallel streams of input like so:

```
1 2 4
3 5 7
8 6 0
```

From a technical perspective, we receive this input one line at a time. Therefore it makes sense to support a "line of input" as a data type we can manipulate. It also makes sense to call it something shorter than "line of input" so we called it a frame. A frame can be thought of as a 1-dimensional array of integers.

If we wish to construct or combine frames, we can use the `,` operator. For example to recreate the first line of input we can write `1, 2, 4`. Alternatively we could write:

```
a = 1, 2
b = a, 4
```

Which would result in `b` having the value `1, 2, 4`. In this way, the `,` operator can be thought of in Haskell terms as an overloaded combination of `:` and `++` for lists.

### Streams
    
In Trout, we define a "stream" as an array of frames. So rather than processing "3 parallel streams", Trout processes a "3-wide stream" which is defined as a (potentially unbounded) 1-dimensional array of frames of length 3.

Similarly to frames, there is a stream append operator: `&`. Much like `,` this is also overloaded to handle types which can be cast to a stream, so `<a> & <b>` is a valid way to produce a stream so long as `<a>` and `<b>` are expressions which return an integer, or a frame, or a stream.

An example using streams, frames, and integers:

`2 + 3 , 4 & 5 , 8 * 2 & 0 , 3 - 2`

Produces a stream equivalent to the following input:

```
5 4
5 16
0 1
```

There are two pre-defined special stream variables:

`IN`: this is the input stream. Useful for processing an input stream.
`#`: this is an infinite stream consisting of single ones (i.e. the result of `1 & 1 & 1 & 1...`). Useful as a basis for performing arbitrary computation.

This gives us enough tooling to tackle the first problem - prepending zero to the input stream:

`0 & IN`.

We simply combine zero and the input stream, and the blocking output takes care of making sure we print out the correct number of lines.

### Iterators

So we know how to prepend and append streams, but how do we actually modify the *content* of a stream?

Trout provides no way to directly index streams (e.g. there is no explicit way to say "give me the third line of this stream"). This is because streams are by default considered to be unbounded, so attempting to keep an incrementing index seems unwise as it would likely lead to an integer overflow or memory problems (Trout attempts to avoid assigning memory unnecessarily - the program containing just `#` will happily respond to every element of input with a `1` infinitely with almost no increase in memory usage).

Rather than directly indexing streams, trout provides a syntax for iterating over streams. The syntax is as follows:

```
<STREAM> {
  [<STATEMENT>]
}
```

Statements are processed as they would be normally with a few additions. So for example:

```
IN { 152 }
```

Will print an infinite sequence of `152`s. This is because we're iterating over all input items and printing `152` for each of them.

Thanks to laziness, this is actually equivalent to `# { 152 }` since the number of output lines is capped at the number of input lines anyway.

The iterator runs all of its statements for *every frame* of the stream. Unlike streams, frames have a well-known bounded size, so Trout provides syntax to index them directly.

When within an iterator, we can use `[n]` to access index `n` of the current frame. We can also use `<n>` as a variable to write to index `n` of the output frame - iterators don't just run statements on streams, they can also *output* the modified stream. Of course, `n` doesn't have to be an explicit integer, it just needs to be some expression that evaluates to an integer, so `[5 + 3 * y]` is perfectly valid assuming that `y` is an integer.

We can also use a `break` statement, generally in conjunction with an `if` to terminate the iterator and proceed with further lines of code. This is especially helpful when dealing with complex nested iterators or iterating over `#`.

Armed with this knowledge we can now perform custom processing on arbitrary length streams. For example:

```
s {
    if([0] > [1]) <0> = [0]
    if([0] > [1]) <1> = [1]
    if(!([0] > [1])) <0> = [1]
    if(!([0] > [1])) <1> = [0]
}
```

This takes some stream `s` of width 2 and for each frame checks if the first value is greater than the second value, swapping them if not. So as an example:

```
1 2
9 1
2 3
```
(Note, this can be assigned using `s = 1 , 2 & 9 , 1 & 3 , 2`)

will become:

```
2 1
9 1
3 2
```

With the first and last frame having their first and second values swapped.

### Typing and Error Handling

Trout is dynamically typed and supports implicit casts. This means that this is valid trout:

```
(0) {
  3
}
```

The zero is cast from an integer to a 1-length 1-width stream, and then iterated over once, resulting in `3` being printed.

Trout's approach to error-handling is to first and foremost not error where a reasonable approach to computation exists. This is the justification behind our implicit casting. It is also the reason it is perfectly possible to write a Trout program which parses this input:

```
1
7 89 8 7 2 11 2
1 2
6 7 8
```

This is *not* standard input - the width of each input line is fixed according to the task specification. However, since we're processing input one line at a time, there's no good reason we shouldn't be able to handle variable width input, and so we permit it. Of course, this example only works if you either don't access the input, or access only `[0]`. Attempting to access `[1]` will produce an error informing you that this input index does not exist once the first line has been parsed.



### Formatting

Newlines in Trout have syntactic meaning - they separate statements. No statement terminating character (e.g. `;` in Java) exists, so the usage of newlines is required in Trout programs. The Trout interpreter should correctly process both UNIX and DOS line endings.

Whitespace other than as token separation has no syntactic meaning in Trout, however it is recommended that indendation be used in a similar way to that in C programs, in order to maximise readability.