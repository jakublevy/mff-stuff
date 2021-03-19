#ifndef LSM_INPUTDATA_H
#define LSM_INPUTDATA_H

class InputData {

public:
    InputData(unsigned height, unsigned weight) : h(height), w(weight) {}

    unsigned height() const { return h; }
    unsigned weight() const { return w; }

    void setHeight(unsigned height) { h = height; }
    void setWeight(unsigned weight) { w = weight; }

private:
    unsigned h;
    unsigned w;
};


#endif //LSM_INPUTDATA_H
