// Main.cpp : Defines the entry point for the console application.
//
// Created by Jakub Levý on 24.12.18.
// I used Visual Studio for this homework so the code is formatted in its style.

#include <string>
#include <iostream>
#include <fstream>
#include "Parser.hpp"
#include "StringUtils.hpp"

using std::cout;		using std::endl;
using std::ifstream;	using std::istreambuf_iterator;
using std::string;      using std::cin;

string readInput();

int main()
{
    Parser p(readInput());
    cout << p.transcript() << endl;
    return 0;
}

string readInput()
{
    istreambuf_iterator<char> begin(cin);
    istreambuf_iterator<char> end;
    return string(begin, end);
}
