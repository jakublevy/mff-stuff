#include <iostream>
#include <regex>
#include <string>
#include <memory>
#include "parser1.hpp"
#include "parser2.hpp"
#include "lpgenerator1.hpp"
#include "lpgenerator2.hpp"

using std::string;
using std::istreambuf_iterator;
using std::cin;
using std::cout;
using std::unique_ptr;
using std::make_unique;

string read_stdin();
bool contains_lp1(string const &input);

int main() {
    string input = read_stdin();
    unique_ptr<Parser> p;
    unique_ptr<Lp_Generator> lp;
    if(contains_lp1(input)) {
        p = make_unique<Parser1>(std::move(input));
        p->parse();
        auto p1 = dynamic_cast<Parser1 *>(p.get());
        lp = make_unique<Lp_Generator1>(p->vertices_count(), p->edges_count(), p1->successors());
    }
    else {
        p = make_unique<Parser2>(std::move(input));
        p->parse();
        auto p2 = dynamic_cast<Parser2 *>(p.get());
        lp = make_unique<Lp_Generator2>(p->vertices_count(),p->edges_count(),p2->successors());

    }
    cout << lp->generate() << '\n';
    return 0;
}

//reads the whole stdin until EOF is encountered
//doesn't skip white characters (as istream_iterator does)
string read_stdin() {
    istreambuf_iterator<char> begin(cin);
    istreambuf_iterator<char> end;
    return string(begin, end);
}

//true if input contains linear program for excercise 1
//false otherwise
bool contains_lp1(string const &input) {
    return input.front() == 'D';
}

