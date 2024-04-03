# dll packages

## ScrabbleServer
Allows you to hook up an arbitrary amount of clients to play
against each other, or several instances of the same client to play against itself.

## ScrabbleUtil
a utility library that contains the minimum required datatypes, a
few boards to play on, and primitives for server communication, and a means to
set up a common dictionary among several bots (you only have to implement one
for yourself).

## ScrabbleLib
A shared library to support the other DLLs. The only function
available to you is a simple parser for the scrabble boards (covered later).

## Oxyphenbutazone
Jesper's scrabble bot. It follows a greedy approach and
always plays the highest-scoring move it can find.

# Program.fs
For communication between clients. Console color can be changed here, different boards can be tried out, and initialising game constants such as words from the dictionary, # of letters on hand, timeout, tiles, seed, port, and list of players. 