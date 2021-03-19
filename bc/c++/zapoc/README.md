# L-system renderer
A renderer of simple 2D fractals defined by L-systems written in Qt.

## Table of Contents
1. [Compilation and examples of input](#compilation-and-examples-of-input)
1. [Supported L-systems](#supported-l-systems)
1. [Project structure](#project-structure)
    1. [Binaries](#binaries)
    1. [Headers](#headers)
    1. [Source](#source)
    1. [UI files](#ui-files)
    1. [Resources](#resources)
1. [Shortcuts](#shortcuts)
1. [Input example with MainWindow description](#input-example-with-mainwindow-description)
1. [Internal parts of the code](#internal-parts-of-the-code)

## Compilation and examples of input
See the files [INSTALL.md](INSTALL.md) and [DEMO.md](DEMO.md) for compilation and examples of input respectively.

## Supported L-systems
An L-system is some type of a grammar that describes fractals. It is not strictly defined. This project supports and defines the following:

| Regex | Meaning |
| ----- | ------- |
| [A-Z]   | draws a line in the direction of the turtle |
| [a-z] | just moves the turtle in its orientation without drawing a line |
| \\[ | save the current turtle state (position and orientation) to the stack |
| \\] | set the top of the stack as the current turtle state and pop the stack |
| \\+ | turn the turtle anti-clockwise by the preset angle |
| \\- | turn the turtle clockwise by the preset angle |

This is a nice animation of how an L-system is drawn.

<a id="turtle-animation"></a>
![turtle animation](https://upload.wikimedia.org/wikipedia/commons/0/09/KochTurtleAnim.gif)

Credits: https://cs.wikipedia.org/wiki/L-syst%C3%A9m#/media/File:KochTurtleAnim.gif


## Project structure
### Binaries
| Folder | Contains |
| ------ | ----------- |
| [bin/windows](bin/windows)   | binaries for Windows |
| [bin/linux](bin/linux) | binaries for Linux |
### Qt Creator output
| Folder | Contains |
| ------ | ----------- |
| [output/windows](output/windows)   | Qt Creator output for Windows |
| [output/linux](output/linux) | Qt Creator output for Linux |
### Headers
| Folder | Contains |
| ------ | ----------- |
| [include/](include/)   | all header files of the project  |

### Source
| Folder | Contains |
| ------ | ----------- |
| [src/](src/)   | all source files of the project  |

### UI files
The project consists of exactly three forms.

| Form | Contains |
| ------ | ----------- |
| [ui/mainwindow.ui](ui/mainwindow.ui)   | the main application window  |
| [ui/colorchangeform.ui](ui/colorchangeform.ui)   | the user control used for associating colors with drawing characters  |
| [ui/ruleform.ui](ui/ruleform.ui)   | the user control used for associating transcription rules with characters  |

ruleform.ui             |  colorchangeform.ui
:-------------------------:|:-------------------------:
![ruleform.ui](https://i.ibb.co/LkrwCwk/rules.png)  |  ![colorchangeform.ui](https://i.ibb.co/9wt5wqm/colorchange.png)
### Resources
The only external resource of the project is _resources/icon.ico_.

Credits: https://thenounproject.com/term/fractal/95963/

There is also another file _resources/resources.qrc_ that just tells Qt Creator to treat the icon as a project resource.

## Shortcuts
There are 14 different keybinds, 11 of them are just for demonstrating functionality (see [DEMO.md](DEMO.md)). The remaining 3 are:

| Shortcut | Meaning |
| ------ | ----------- |
| `Enter`   | renders the current input  |
| `Esc`   | clears the current input  |
| `F12`   | toggles the visibility of a cross  |


## Input example with MainWindow description

Given an L-system

 ```javascript
 PeanoCurve pc = {
   axiom : "F",
   angle : 90°,
   rules : { "F" ⟶ "FF+F+F+FF+F+F-F" },
   colors : { "F" ⟶ 🔵 }
}
```
We would like to visualize it.

Fill the GUI as shown:

![MainWindow](https://i.ibb.co/j6C64rX/peano.png)

Marked components are those that just manipulate the size, position and details of the fractal. The rest is filled with respect to the input L-system.

1. The cross indicating a starting position of the turtle
2. The length that the turtle moves (see [turtle animation](#turtle-animation))
3. The default orientation of the turtle
4. The requested generation (the number of times transcription rules will be applied)


By changing the marked components, we might get something like these:

high generation, short line length             |  low generation, long line length
:-------------------------:|:-------------------------:
![peanoCurve1](https://i.ibb.co/kJvB5dR/p1.png)  |  ![peanoCurve2](https://i.ibb.co/Qrh99nt/p2.png)

## Internal parts of the code
See [INTERNALS.md](INTERNALS.md)
