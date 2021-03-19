#include <fstream>
#include "../include/InputParser.h"

using std::vector;
using std::ifstream;

vector<InputData> InputParser::read() {
    vector<InputData> out;
    ifstream fHandle(inputPath);
    unsigned height, weight;
    while(fHandle >> height >> weight)
        out.emplace_back(InputData(height, weight));

    return out;
}
