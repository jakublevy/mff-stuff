#ifndef LSYSTEM_HPP
#define LSYSTEM_HPP

#include <map>
#include <QRegExp>

//This class just manipulates the grammar.

class Lsystem
{
public:
    Lsystem(QString const &s) : axiom(s), _sentence(s), _generation(0) { }
    Lsystem(QString &&s) : axiom(s), _sentence(std::move(s)), _generation(0) { }

    //Puts Lsystem into next generation state.
    void nextGeneration() noexcept;

    //Puts Lsystem into nth generation state.
    void nthGeneration(int g) noexcept;

    std::map<QChar, QString> const &rules() const noexcept { return _rules; }
    void setRules(std::map<QChar, QString> const &r) { _rules = r; }
    void setRules(std::map<QChar, QString> &&r) { _rules = std::move(r); }
    int generation() const noexcept { return _generation; }
    QString const &sentence() const noexcept { return _sentence; }

    //Static methods used for detecting the nature of character.
    static bool movingChar(QChar c) noexcept;
    static bool drawingChar(QChar c) noexcept;

    //Regex representing allowed characters in L-system grammar.
    static QRegExp everythingRegex;

    void swap(Lsystem &&other) noexcept;

private:
    QString axiom;
    QString _sentence;
    std::map<QChar, QString> _rules;
    int _generation;
};

#endif // LSYSTEM_HPP
