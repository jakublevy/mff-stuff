# Huffman coding
A simple implementation of Huffman coding in Haskell.

## Test data
See [DEMO.md](DEMO.md).

## Synopsis
The main goal of this project is to compress a text string by Huffman Coding. This process can be divided into several parts. Each file of the project corresponds to one part.

The encoding process can be described by functions as follows:
~~~
String ⟶ Occurrence
Occurrence ⟶ Tree
Tree ⟶ Assoc
(Assoc, String) ⟶ Code 
~~~
(Definitions of these data types are inside [Types.hs](src/Types.hs)).

Each one of the above listed functions is contained inside its own module as the only exported function.

For example; The function `Occurrence ⟶ Tree` named `occurrence2tree` is contained inside `Occurrence2tree` module that is located in [Occurrence2tree.hs](src/Occurrence2tree.hs). These naming conventions apply for all other functions listed above.

A code user whose goal is to compress a text string would expect encoding function of type:
~~~
String ⟶ (Code, Assoc)
~~~
(We also return a dictionary to be able to decode the data).

Of course, this function is available, it is inside [Coding.hs](src/Coding.hs) and it is named `encode`.

It would be handy to have a decoding function, this one is also located in the latter named file.
