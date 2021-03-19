#include "lpgenerator2.hpp"

using std::string;
using std::to_string;
using std::get;

string Lp_Generator2::generate() const {
    static string firstPiece = "param N := ";

    static string secondPiece = "set Vertices := (0..N-1);\n"
                                "set Edges := ";
    static string thirdPiece = "var x{i in Vertices, j in Vertices}, >=0, <=1, integer;\n"
                               "minimize obj: sum{(i,j,w) in Edges} x[i,j] * w;\n"
                               "condition_cycle3 {(i,j,w1) in Edges, (j,k,w2) in Edges, (k,i,w3) in Edges}: (1-x[i,j]) + (1-x[j,k]) + (1-x[k,i]) <= 2;\n"
                               "condition_cycle4 {(i,j,w1) in Edges, (j,k,w2) in Edges, (k,l,w3) in Edges, (l,i,w4) in Edges}: (1-x[i,j]) + (1-x[j,k]) + (1-x[k,l]) + (1-x[l,i]) <= 3;"
                               "solve;\n"
                               "printf \"#OUTPUT: %d\\n\",sum{(i,j,w) in Edges} x[i,j] * w;\n"
                               "printf {(i,j,w) in Edges} (if (x[i,j] = 1) then \"%d --> %d\\n\" else \"\" ),i,j,x[i,j];\n"
                               "printf \"#OUTPUT END\\n\";\n"
                               "end;";
    return firstPiece + to_string(vertices_count) + ";\n" + secondPiece + generate_edges() + thirdPiece;
}

string Lp_Generator2::generate_edges() const {
    string edges = "{";
    edges.reserve(edges_count * 10);

    for(auto &token : successors) {
        auto &[first, second, third] = token;
        edges += '('; edges += first;
        edges += ','; edges += second;
        edges += ','; edges += third;
        edges += ')'; edges += ',';
    }
    edges[edges.size() - 1] = '}';
    return edges + ";\n";
}
