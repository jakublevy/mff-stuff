#include <tuple>
#include "parser1.hpp"
#include "utils.hpp"

using std::string;
using std::make_tuple;
using std::pair;
using std::vector;

void Parser1::parse() {
    vector<string> lines = Utils::split_on_regex(input_, "\n");
    auto[vertices_count, edges_count] = parse_first_line(lines.front());
    vertices_count_ = vertices_count;
    edges_count_ = edges_count;

    for (size_t i = 1; i < lines.size(); ++i) {
        auto[from, to] = parse_edge(lines[i]);
        successors_.insert(pair(std::move(from), std::move(to)));
    }
}

std::tuple<uint32_t, uint32_t> Parser1::parse_first_line(string const &first_line) {
    vector<string> split = Utils::split_on_regex(first_line, "[ ]+|[:]+");
    return make_tuple(stoi(split[1]), stoi(split[2]));
}

std::tuple<std::string, std::string> Parser1::parse_edge(string const &edge) {
    vector<string> split = Utils::split_on_regex(edge, "[ ]+");
    size_t firstNotEmptyIdx = Utils::first_not_empty_idx(split);
    return make_tuple(split[firstNotEmptyIdx], split[firstNotEmptyIdx+2]);
}
