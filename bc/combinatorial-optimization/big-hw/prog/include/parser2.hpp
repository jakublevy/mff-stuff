#ifndef _PARSER2_HPP_
#define _PARSER2_HPP_

#include <set>
#include "parser.hpp"

class Parser2 : public Parser {
public:
    explicit Parser2(std::string &&input) : Parser(std::move(input)) {}

    void parse() override;

    std::set<std::tuple<std::string, std::string, std::string>> const &successors() const noexcept { return successors_; }

public:
    //parses e.g. WEIGHTED DIGRAPH 3 5:
    std::tuple<uint32_t, uint32_t> parse_first_line(std::string const &first_line) override;

    //parses e.g. 0 --> 1 (3)
    std::tuple<std::string, std::string, std::string> parse_edge(std::string const &edge);

    //contains pair (i,j,w) if there exists an directed edge from i to j with valuation w
    std::set<std::tuple<std::string, std::string, std::string>> successors_;

};
#endif
