#ifndef _UTILS_HPP_
#define _UTILS_HPP_

#include <vector>
#include <string>

class Utils {
public:
    //splits given string according to a regex pattern
    static std::vector<std::string> split_on_regex(std::string const &str, std::string const &pattern);

    //returns first non-empty index of a given vector
    static size_t first_not_empty_idx(std::vector<std::string> const &tokens);
};
#endif
