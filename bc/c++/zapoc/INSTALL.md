# Compilation
The source is actually a Qt Creator project that uses qmake as a build system. To open and compile source it's necessary to:

1. Install an arbitrary Qt toolchain (https://www.qt.io/download)
1. Install Qt Creator (generally installs with a toolchain)

Then it's possible to open <i>Lsystem.pro</i> with Qt Creator and compile using `F5`.

## Binaries
It's annoying to compile Qt applications. That's why the project contains binaries.

| Folder | Contains |
| ------ | ----------- |
| [bin/windows](bin/windows)   | binaries for Windows |
| [bin/linux](bin/linux) | binaries for Linux |

Those folders have huge size because they contain all Qt dependencies, but they are standalone.

## Qt Creator output
In case of an interest in the actual Qt Creator compilation output, there are respective folders.

| Folder | Contains |
| ------ | ----------- |
| [output/windows](output/windows)   | Qt Creator output for Windows |
| [output/linux](output/linux) | Qt Creator output for Linux |
