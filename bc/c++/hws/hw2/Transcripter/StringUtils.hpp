#ifndef _STRING_UTILS_
#define _STRING_UTILS_
#include <string>
#include <vector>
#include <set>

class StringUtils
{
public:
	/// <summary>
	/// In place trim functions copied from https://stackoverflow.com/questions/216823/whats-the-best-way-to-trim-stdstring.
	/// </summary>
	/// <param name="s">String to be updated.</param>
	static void ltrim(std::string& s);
	static void rtrim(std::string& s);
	static void trim(std::string& s);


	/// <summary>
	/// This function returns new string without repetitive white characters and semicolons.
	/// </summary>
	/// <param name="s">String to examine.</param>
	/// <returns>
	/// New string with at most one semicolon and one space consecutively.
	/// </returns>
	static std::string maxOneSpaceAndSemicolon(std::string const& s);


	/// <summary>
	/// Deletes all white characters from supplied string.
	/// </summary>
	/// <param name="s">String to delete from.</param>
	/// <returns>New string without white characters</returns>
	static std::string deleteWhiteSpaces(std::string const& s);


	/// <summary>
	/// Splits a string and throws away matched characters.
	/// </summary>
	/// <param name="s">String to split.</param>
	/// <param name="pattern">Regex pattern to split with accordance to.</param>
	/// <returns>Split string in a collection that does not contain matched characters. It contains only those unmatched.</returns>
	static std::vector<std::string> splitWithoutMatched(std::string const& s, std::string const& pattern);


	/// <summary>
	/// Splits a string and leaves matched characters untouched.
	/// </summary>
	/// <param name="s">String to split.</param>
	/// <param name="pattern">Regex pattern to split with accordance to.</param>
	/// <returns>Split string in a collection that contains matched characters. It does not contain those unmatched.</returns>
	static std::vector<std::string> splitWithMatched(std::string const& s, std::string const& pattern);


	/// <summary>
	/// Performs a match test of string s to regex pattern.
	/// </summary>
	/// <param name="s"></param>
	/// <param name="pattern"></param>
	/// <returns>
	/// Boolean flag indicating whether s matches pattern
	/// </returns>
	static bool regexMatch(std::string const& s, std::string const& pattern);


	/// <summary>
	/// Finds the idx of matching (closing) bracket (brace or parenthesis).
	/// Assumes that source[startPos] == '(' || source[startPos] == '{'
	/// </summary>
	/// <param name="source">String to find brackets in.</param>
	/// <param name="startPos">Position that contains opening bracket.</param>
	/// <returns>Position of closing bracket.</returns>
	static size_t matchingBracketIdx(std::string const& source, size_t startPos);


	/// <summary>
	/// Gets an expression containing function call.
	/// Returns number of arguments that function call consists of.
	/// </summary>
	/// <param name="expr">String to calculate arguments in.</param>
	/// <param name="offset">offset such that expr[offset] == '(' </param>
	/// <param name="count">number of characters to examine (expr[offset + count - 1] == ')' ).</param>
	/// <returns>Number of arguments given function call consists of.</returns>
	static size_t argumentsCount(const std::string& expr, size_t offset, size_t count);


	/// <summary>
	/// Returns new string created from original where each occurence of string
	/// from is replaced with string to. Copied from https://stackoverflow.com/questions/2896600/how-to-replace-all-occurrences-of-a-character-in-string.
	/// </summary>
	/// <param name="original">String from which the new one is created.</param>
	/// <param name="from">Occurrences of what to find.</param>
	/// <param name="to">Replacement for occurrences.</param>
	/// <returns></returns>
	static std::string replaceAll(std::string const& original, std::string const& from, std::string const& to);


	/// <summary>
	/// Checks whether supplied string is a unary operator in S#
	/// </summary>
	/// <param name="str">String to examine.</param>
	/// <returns>True if str is a unary operator.</returns>
	static bool isUnaryOperator(std::string const& str);


	/// <summary>
	/// Checks whether supplied string is a binary operator in S#.
	/// </summary>
	/// <param name="str">String to match.</param>
	/// <returns>True if str is a binary operator.</returns>
	static bool isBinaryOperator(std::string const& str);


	/// <summary>
	/// Checks whether the supplied string is a reserved keyword in C/C++
	/// </summary>
	/// <param name="str">String to examine.</param>
	/// <returns>True if str is a reserved keyword, false otherwise.</returns>
	static bool isReservedKeyword(std::string const& str);


	/// <summary>
	/// This function matches supplied string against valid function prototype format.
	/// </summary>
	/// <param name="prototype">String to match against valid function prototype format.</param>
	/// <returns>Boolean flag indicating whether prototype is a valid function prototype.</returns>
	static bool fPrototypeMatch(std::string const& prototype);

private:
	/// <summary>
	/// Both public split functions (splitWithoutMatched, splitWithMatched)
	/// would have essentially the same body. So they both call this implementation
	/// function.
	/// </summary>
	/// <param name="s">String to split.</param>
	/// <param name="pattern">Regex pattern to split with accordance to.</param>
	/// <param name="sub">
	/// {-1} for splitWithoutMatched
	/// {-1,0} for splitWithMatched
	/// </param>
	/// <returns></returns>
	static std::vector<std::string> split(std::string const& s, std::string const& pattern,
		std::initializer_list<int>&& sub);


	/// <summary>
	/// Hardcoded set of reserved keywords in C/C++.
	/// </summary>
	static std::set<std::string> reservedKeywords;
};
#endif
