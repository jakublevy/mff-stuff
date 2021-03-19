#include <sstream>
#include <QTextStream>

#include "lsystem.hpp"
#include "badgrammar.hpp"

void Lsystem::nextGeneration() noexcept {
    QString output;
    QTextStream ss(&output);
    for(auto &c : _sentence) {
        if(movingChar(c)) {
            if(_rules.find(c) != _rules.end()) {
                ss << _rules[c];
            }
            //There exist L-systems that don't rewrite all characters.
            //else {
            //    throw BadGrammar("Missing rule for character");
           // }
        }
        else {
            ss << c;
        }
    }
    _sentence = std::move(output);
    ++_generation;
}

void Lsystem::nthGeneration(int g) noexcept {
    if(g > _generation) {
        for(int i = _generation; i < g; ++i) {
            nextGeneration();
        }
    }
    else if(g < _generation) {
        _generation = 0;
        _sentence = axiom;
        nthGeneration(g);
    }
}

bool Lsystem::movingChar(QChar c) noexcept {
    return c != '+' && c != '-' && c != '[' && c != ']';
}

bool Lsystem::drawingChar(QChar c) noexcept {
    return c.isUpper();
}

QRegExp Lsystem::everythingRegex = QRegExp(R"([a-zA-Z\+\-\[\]]*)");

void Lsystem::swap(Lsystem &&other) noexcept {
    std::swap(axiom, other.axiom);
    std::swap(_sentence, other._sentence);
    std::swap(_rules, other._rules);
    std::swap(_generation, other._generation);
}
