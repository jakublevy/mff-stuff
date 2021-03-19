#ifndef _PARSER1_HPP_
#define _PARSER1_HPP_

#include <string>
#include <set>
#include "parser.hpp"

class Parser1 : public Parser {
public:
    explicit Parser1(std::string &&input) : Parser(std::move(input)) {}

    void parse() override;

    std::set<std::pair<std::string, std::string>> const &successors() const noexcept { return successors_; }

private:
    //parses e.g. DIGRAPH 3 5:
    std::tuple<uint32_t, uint32_t> parse_first_line(std::string const &first_line) override;

    //parses e.g. 0 --> 1
    std::tuple<std::string, std::string> parse_edge(std::string const &edge);

    //contains pair (i,j) if there exists an directed edge from i to j
    std::set<std::pair<std::string, std::string>> successors_;

};
#endif
