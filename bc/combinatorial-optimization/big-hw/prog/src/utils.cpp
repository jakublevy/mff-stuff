#include "utils.hpp"
#include <regex>

using std::vector;
using std::string;
using std::regex;
using std::sregex_token_iterator;

vector<string> Utils::split_on_regex(string const &str, string const &pattern) {
    regex rgx(pattern);
    sregex_token_iterator begin(str.cbegin(), str.cend(), rgx, -1);
    sregex_token_iterator end;
    return vector<string>(begin, end);
}

size_t Utils::first_not_empty_idx(vector<string> const &tokens) {
    size_t idx = 0;
    while(tokens[idx].empty())
        ++idx;

    return idx;
}
