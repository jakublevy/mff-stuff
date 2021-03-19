#ifndef BADGRAMMAR_HPP
#define BADGRAMMAR_HPP

#include <exception>
#include <string>

class BadGrammar : public std::exception
{
public:
    BadGrammar(char const *msg) : msg(msg) { }
    BadGrammar(std::string const &msg) : msg(msg) { }

    char const *what() const noexcept override { return msg.c_str(); }

private:
    std::string msg;
};

#endif // BADGRAMMAR_HPP
