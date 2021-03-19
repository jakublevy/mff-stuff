#ifndef LSM_INPUTPARSER_H
#define LSM_INPUTPARSER_H

#include <string>
#include <vector>
#include "InputData.h"

class InputParser {

public:
    InputParser(std::string inputFilePath) : inputPath(std::move(inputFilePath)) {}

    std::vector<InputData> read();

private:
    std::string inputPath;
};


#endif //LSM_INPUTPARSER_H
