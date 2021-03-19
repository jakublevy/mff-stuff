#ifndef _LP_GENERATOR2_HPP_
#define _LP_GENERATOR2_HPP_

#include <set>
#include <string>
#include <tuple>
#include "lpgenerator.hpp"

//This class generates input for excercise 2 for glpsol

class Lp_Generator2 : public Lp_Generator {
public:
    explicit Lp_Generator2(uint32_t vertices_count, uint32_t edges_count,
                          std::set<std::tuple<std::string,std::string,std::string>> const &successors)
            : Lp_Generator(vertices_count, edges_count), successors(successors) {}

    std::string generate() const override;

private:
    std::string generate_edges() const override;

    //contains pair (i,j,w) if there exists an directed edge from i to j with valuation w
    //note that this is a reference, the actual data belong to an instance of Parser2
    //which must live longer than instance of this class
    std::set<std::tuple<std::string,std::string, std::string>> const &successors;

};
#endif
