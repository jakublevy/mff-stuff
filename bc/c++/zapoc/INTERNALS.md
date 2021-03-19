# Internals
This file contains a brief summary of the meaning of each class.
## Table of Contents
1. [Visual signpost](#visual-signpost)
1. [BadGrammar](#badgrammar)
1. [ColorChangeForm](#colorchangeform)
1. [DrawingInstruction](#drawinginstruction)
1. [DrawingWidget](#drawingwidget)
1. [Lsystem](#lsystem)
1. [MainWindow](#mainwindow)
1. [RenderInfo](#renderinfo)
1. [RuleForm](#ruleform)
1. [SelectColorButton](#selectcolorbutton)
1. [State](#state)
1. [Turtle](#turtle)
## Visual signpost
![signpost](https://i.ibb.co/Vj6hsFP/signpost.png)

1. [MainWindow](#mainwindow)
1. [DrawingWidget](#drawingwidget)
1. [RuleForm](#ruleform)
1. [ColorChangeForm](#colorchangeform)
1. [SelectColorButton](#selectcolorbutton)
## BadGrammar
### Files
* [include/badgrammar.hpp](include/badgrammar.hpp)
### Description
A small class that inherits from `std::exception`. `BadGrammar` is thrown when underlying L-system grammar is invalid.

## ColorChangeForm
### Files
* [include/colorchangeform.hpp](include/colorchangeform.hpp)
* [src/colorchangeform.cpp](src/colorchangeform.cpp)
* [ui/colorchangeform.ui](ui/colorchangeform.ui)
### Description
An user control associating an L-system drawing character with a color. See [Visual signpost](#visual-signpost) 4.

## DrawingInstruction
### Files
* [include/drawinginstruction.hpp](include/drawinginstruction.hpp)
### Description
This class represents one drawing primitive that is used by [DrawingWidget](#drawingwidget).

## DrawingWidget
### Files
* [include/drawingwidget.hpp](include/drawingwidget.hpp)
* [src/drawingwidget.cpp](src/drawingwidget.cpp)
### Description
See [Visual signpost](#visual-signpost) 2, these files represent the main drawing area of the application. All drawing is done here.

## Lsystem
### Files
* [include/lsystem.hpp](include/lsystem.hpp)
* [src/lsystem.cpp](src/lsystem.cpp)
### Description
This class just manipulates the input grammar. Its main functionality is that it outputs the sentence of the requested fractal.

## MainWindow
### Files
* [include/mainwindow.hpp](include/mainwindows.hpp)
* [src/mainwindow.cpp](src/mainwindow.cpp)
* [ui/mainwindow.ui](ui/mainwindow.ui)
### Description
A lot of (not really interesting) Qt related stuff happens here (filling GUI, extracting data from GUI, responding to actions).


## RenderInfo
### Files
* [include/renderinfo.hpp](include/renderinfo.hpp)
* [src/renderinfo.cpp](src/renderinfo.cpp)
### Description
Contains all the information needed for an L-system to render. A class [Turtle](#turtle) gets those information and transform them into an actual collection of [DrawingInstruction](#drawinginstruction) that is drawn by [DrawingWidget](#drawingwidget).

## RuleForm
### Files
* [include/ruleform.hpp](include/ruleform.hpp)
* [src/ruleform.cpp](src/ruleform.cpp)
* [ui/ruleform.ui](ui/ruleform.ui)
### Description
An user control associating an L-system character with a transcription rule. See [Visual signpost](#visual-signpost) 3.

## SelectColorButton
### Files
* [include/selectcolorbutton.hpp](include/selectcolorbutton.hpp)
* [src/selectcolorbutton.cpp](src/selectcolorbutton.cpp)
### Description
It turns out there is no Qt widget that shows color dialog and represents chosen color in some way. This class represents that kind of button (see [Visual signpost](#visual-signpost) 5).

Credits: https://stackoverflow.com/questions/18257281/qt-color-picker-widget

## State 
### Files
* [include/state.hpp](include/state.hpp)
* [src/state.cpp](src/state.cpp)
### Description
Characters `[` and `]` manipulate the current state (position, orientation) of [Turtle](#turtle). This class just represents the state of [Turtle](#turtle).

## Turtle
### Files
* [include/turtle.hpp](include/turtle.hpp)
* [src/turtle.cpp](src/turtle.cpp)
### Description
Given information from GUI about the requested fractal, [Turtle](#turtle) transform them into a collection of [DrawingInstruction](#drawinginstruction) that is drawn by [DrawingWidget](#drawingwidget).
