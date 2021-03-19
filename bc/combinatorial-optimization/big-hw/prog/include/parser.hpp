#ifndef _PARSER_HPP_
#define _PARSER_HPP_

#include <string>

class Parser {
public:
    explicit Parser(std::string &&input) : input_(std::move(input)) { }
    virtual ~Parser() = default;

    //initialize the remaining variables,
    //should be called immediately after creating an instance of this class
    virtual void parse() = 0;

    uint32_t vertices_count() const noexcept { return vertices_count_; }
    uint32_t edges_count() const noexcept { return edges_count_; }

protected:
               //vertices_count, edges_count
    virtual std::tuple<uint32_t, uint32_t> parse_first_line(std::string const &first_line) = 0;

    std::string input_;
    uint32_t vertices_count_;
    uint32_t edges_count_;
};
#endif
