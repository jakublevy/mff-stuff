#ifndef _LPGENERATOR_HPP_
#define _LPGENERATOR_HPP_
#include <string>

//This class is a base class for Lp_Generator1 and Lp_Generator2
//Those two derived classes generates input for excercise 1 and 2 for glpsol
class Lp_Generator {
public:
    explicit Lp_Generator(uint32_t vertices_count, uint32_t edges_count) : vertices_count(vertices_count),
                                                                  edges_count(edges_count) {}
    virtual ~Lp_Generator() = default;

    //returns a program for glpsol
    virtual std::string generate() const = 0;

protected:
    //returns edges for glpsol
    virtual std::string generate_edges() const = 0;

    uint32_t vertices_count;
    uint32_t edges_count;

};
#endif
