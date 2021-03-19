#include <algorithm>
#include <tuple>
#include "parser2.hpp"
#include "utils.hpp"

using std::string;
using std::make_tuple;
using std::vector;
using std::remove_if;

void Parser2::parse() {
    vector<string> lines = Utils::split_on_regex(input_, "\n");
    auto[vertices_count, edges_count] = parse_first_line(lines.front());
    vertices_count_ = vertices_count;
    edges_count_ = edges_count;

    for (size_t i = 1; i < lines.size(); ++i) {
        auto[from, to, w] = parse_edge(lines[i]);
        successors_.insert(make_tuple(std::move(from), std::move(to), std::move(w)));
    }
}

std::tuple<uint32_t, uint32_t> Parser2::parse_first_line(string const &first_line) {
    vector<string> split = Utils::split_on_regex(first_line, "[ ]+|[:]+");
    return make_tuple(stoi(split[2]), stoi(split[3]));
}

std::tuple<std::string, std::string, std::string> Parser2::parse_edge(string const &edge) {
    vector<string> split = Utils::split_on_regex(edge, "[ ]+|[\\(\\)]");
    split.erase(remove_if(split.begin(), split.end(), [](auto &a) { return a.empty(); }), split.end());
    return make_tuple(split[0], split[2], split[3]);
}
