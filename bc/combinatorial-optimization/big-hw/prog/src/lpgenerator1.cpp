#include "lpgenerator1.hpp"

using std::string;
using std::to_string;

string Lp_Generator1::generate() const {
    static string firstPiece = "param N := ";

    static string secondPiece = "set Vertices := (0..N-1);\n"
                                "set Edges := ";
    static string thirdPiece = "var x{i in Vertices}, >=0;\n"
                               "var y;\n"
                               "minimize obj: y;\n"
                               "condition_topological_sort{(i,j) in Edges}: x[i] <= x[j] -1;\n"
                               "condition_y_is_max{i in Vertices}: x[i] <= y;\n"
                               "solve;\n"
                               "printf \"#OUTPUT: %d\\n\", y;\n"
                               "printf{i in Vertices} \"v_%d: %d\\n\", i, x[i];\n"
                               "printf \"#OUTPUT END\\n\";\n"
                               "end;";
    return firstPiece + to_string(vertices_count) + ";\n" + secondPiece + generate_edges() + thirdPiece;
}

string Lp_Generator1::generate_edges() const {
    string edges = "{";
    edges.reserve(edges_count * 10);

    for(auto &token : successors) {
        edges += '('; edges += token.first;
        edges += ','; edges += token.second;
        edges += ')'; edges += ',';
    }
    edges[edges.size() - 1] = '}';
    return edges + ";\n";
}
