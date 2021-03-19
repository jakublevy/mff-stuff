#ifndef _PARSER_
#define _PARSER_
#include <string>
#include <set>
#include "FunctionManager.hpp"

class Parser
{
public:
	explicit Parser(std::string&& source);

	/// <returns>
	/// A valid C++ program, either the transcripted one of the error one.
	/// </returns>
	std::string transcript();
private:
	/// <summary>
	/// Parses the input.
	/// </summary>
	void parseFunctions();


	/// <summary>
	/// Given expr and endIdx pointing to last alphabetical character.
	/// This function find the longest left alphabetical match.
	/// Example:
	///   let expr = "a;b;fact(6);test(2,2)" and endIdx = 7
	/// then this function would output string "fact"
	/// </summary>
	/// <param name="expr">Expression to find longest alphabetical word in.</param>
	/// <param name="endIdx">Last index of alphabetical word (expr[endIdx] is alphabetical)</param>
	/// <returns>Longest left alphabetical match.</returns>
	std::string longestleftAlphabeticalMatch(const std::string& expr, size_t endIdx) const;


	/// <summary>
	/// Checks for usage of invalid: parameters, function names, collision of names.
	/// </summary>
	/// <param name="fManager">Function manager to find functions in.</param>
	void checkUsageOfFunctions(const FunctionManager& fManager) const;


	/// <summary>
	/// Checks whether functions contain balanced brackets.
	/// </summary>
	/// <param name="fManager">Function manager to find functions in.</param>
	void checkMatchingBrackets(const FunctionManager& fManager) const;


	/// <summary>
	/// Checks if given function body contains character that can never appear in S# programs.
	/// </summary>
	/// <param name="body">Function body to examine.</param>
	/// <returns>Boolean flag indicating whether body contains unallowed character.</returns>
	bool containsUnallowedChars(const std::string& body) const;


	/// <summary>
	/// In S# tilde meaning is logical negation but that is ! in C++.
	/// This method replaces all occurrences of tilde with ! in all functions.
	/// </summary>
	/// <param name="fManager">Function manager to find functions in.</param>
	void replaceTilde(FunctionManager& fManager) const;


	/// <summary>
	/// Checks for missing semicolon between numbers and variables.
	/// </summary>
	/// <param name="body">Function body to check.</param>
	void checkForMissingSemicolons(const std::string& body) const;

	std::string source;
	FunctionManager fManager;
};
#endif
