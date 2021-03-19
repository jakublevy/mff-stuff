#-------------------------------------------------
#
# Project created by QtCreator 2018-10-31T17:51:54
#
#-------------------------------------------------

QT       += core gui

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

TARGET = Lsystem
TEMPLATE = app

# The following define makes your compiler emit warnings if you use
# any feature of Qt which has been marked as deprecated (the exact warnings
# depend on your compiler). Please consult the documentation of the
# deprecated API in order to know how to port your code away from it.
DEFINES += QT_DEPRECATED_WARNINGS

# You can also make your code fail to compile if you use deprecated APIs.
# In order to do so, uncomment the following line.
# You can also select to disable deprecated APIs only up to a certain version of Qt.
#DEFINES += QT_DISABLE_DEPRECATED_BEFORE=0x060000    # disables all the APIs deprecated before Qt 6.0.0

CONFIG += c++17
QMAKE_CXXFLAGS += -std=c++17

INCLUDEPATH += $$PWD/include

@
VPATH += ./src

SOURCES += \
    src/colorchangeform.cpp \
    src/drawingwidget.cpp \
    src/lsystem.cpp \
    src/main.cpp \
    src/mainwindow.cpp \
    src/renderinfo.cpp \
    src/ruleform.cpp \
    src/selectcolorbutton.cpp \
    src/turtle.cpp
@

@
VPATH += ./include
HEADERS += \
    include/badgrammar.hpp \
    include/colorchangeform.hpp \
    include/drawinginstruction.hpp \
    include/drawingwidget.hpp \
    include/lsystem.hpp \
    include/mainwindow.hpp \
    include/renderinfo.hpp \
    include/ruleform.hpp \
    include/selectcolorbutton.hpp \
    include/state.hpp \
    include/turtle.hpp
@

@
VPATH += ./ui

FORMS += \
        mainwindow.ui \
    ruleform.ui \
    colorchangeform.ui
@

# Default rules for deployment.
qnx: target.path = /tmp/$${TARGET}/bin
else: unix:!android: target.path = /opt/$${TARGET}/bin
!isEmpty(target.path): INSTALLS += target

RESOURCES += resources/resources.qrc

win32:RC_ICONS += resources/icon.ico
