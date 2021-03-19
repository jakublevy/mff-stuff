#ifndef _LP_GENERATOR1_HPP_
#define _LP_GENERATOR1_HPP_

#include <set>
#include "lpgenerator.hpp"

//This class generates input for excercise 1 for glpsol

class Lp_Generator1 : public Lp_Generator {
public:
    explicit Lp_Generator1(uint32_t vertices_count, uint32_t edges_count,
                           std::set<std::pair<std::string,std::string>> const &successors)
            : Lp_Generator(vertices_count, edges_count), successors(successors) {}

    std::string generate() const override;

private:
    std::string generate_edges() const override;

    //contains pair (i,j) if there exists an directed edge from i to j
    //note that this is a reference, the actual data belong to an instance of Parser1
    //which must live longer than instance of this class
    std::set<std::pair<std::string,std::string>> const &successors;

};
#endif
