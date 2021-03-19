#include <locale>
#include <algorithm>
#include <cctype>
#include <regex>
#include "StringUtils.hpp"
#include "ContainerUtils.hpp"

using std::string;				using std::regex;
using std::vector;				using std::set;
using std::initializer_list;	using std::sregex_token_iterator;

void StringUtils::ltrim(string& s)
{
	s.erase(s.cbegin(), find_if(s.cbegin(), s.cend(), [](int ch)
	{
		return !isspace(ch);
	}));
}

void StringUtils::rtrim(string& s)
{
	s.erase(find_if(s.crbegin(), s.crend(), [](int ch)
	{
		return !isspace(ch);
	}).base(), s.end());
}

void StringUtils::trim(string& s)
{
	ltrim(s);
	rtrim(s);
}

string StringUtils::maxOneSpaceAndSemicolon(string const& s)
{
	string output;
	output.reserve(s.size());
	for (char c : s)
	{
		if (c != '\n' && c != '\r' && c != '\t')
		{
			if (output.empty() || (!output.empty() &&
				(output.back() != ' ' || (output.back() == ' ' && c != ' '))))
			{
				if (c != ';' || ((output.empty()) || (!output.empty() && output.back() != ';')))
					output += c;
			}
		}
	}
	return output;
}

string StringUtils::deleteWhiteSpaces(string const& s)
{
	string output;
	output.reserve(s.size());
	for (char c : s)
	{
		if (c != '\n' && c != '\r' && c != '\t' && c != ' ')
			output += c;
	}
	return output;
}

vector<string> StringUtils::splitWithoutMatched(string const& s, string const& pattern)
{
	return split(s, pattern, { -1 });
}

vector<string> StringUtils::splitWithMatched(string const& s, string const& pattern)
{
	return split(s, pattern, { -1, 0 });
}

bool StringUtils::regexMatch(string const& s, string const& pattern)
{
	regex r(pattern);
	return regex_match(s.cbegin(), s.cend(), r);
}

size_t StringUtils::matchingBracketIdx(string const& source, size_t startPos)
{
	char opening = source[startPos];
	char closing = '}';
	if (opening == '(')
		closing = ')';
	else if (opening == '[')
		closing = ']';

	size_t count = 1;
	while (count > 0 && startPos < source.size())
	{
		++startPos;
		if (source[startPos] == opening)
		{
			++count;
		}
		else if (source[startPos] == closing)
		{
			--count;
		}
	}
	return startPos;
}

size_t StringUtils::argumentsCount(const string& expr, size_t offset, size_t count)
{
	size_t commaCount = 0;
	size_t parentheses = 0;

	if (matchingBracketIdx(expr, offset) == offset + 1)
	{
		return 0;
	}

	for (size_t i = offset; i < offset + count; ++i)
	{
		if (expr[i] == '(')
			++parentheses;
		else if (expr[i] == ')')
			--parentheses;

		if (parentheses == 1 && expr[i] == ',')
		{
			++commaCount;
		}
	}
	return commaCount + 1;
}

string StringUtils::replaceAll(string const& original, string const& from, string const& to)
{
	string results;
	string::const_iterator end = original.cend();
	string::const_iterator current = original.cbegin();
	string::const_iterator next = search(current, end, from.cbegin(), from.cend());
	while (next != end)
	{
		results.append(current, next);
		results.append(to);
		current = next + from.size();
		next = search(current, end, from.cbegin(), from.cend());
	}
	results.append(current, next);
	return results;
}

bool StringUtils::isUnaryOperator(string const& str)
{
	return str == "!" || str == "~";
}

bool StringUtils::isBinaryOperator(string const& str)
{
	return str == "+" || str == "-" || str == "*" || str == "/" || str == "%" || str == "<" || str == ">" || str == "=="
		|| str == "!=" || str == "&&" || str == "||";
}

bool StringUtils::isReservedKeyword(std::string const &str)
{
	return reservedKeywords.find(str) != reservedKeywords.cend();
}


bool StringUtils::fPrototypeMatch(string const& prototype)
{
	return regexMatch(prototype, "^[a-zA-Z ]+$");
}

vector<string> StringUtils::split(string const& s, string const& pattern,
	initializer_list<int>&& sub)
{
	vector<string> output;
	regex rgx(pattern);
	sregex_token_iterator iter(s.cbegin(), s.cend(), rgx, sub);
	sregex_token_iterator end;
	output.insert(output.cend(), iter, end);
	ContainerUtils::removeEmptyEntries(output);
	return output;
}

set<string> StringUtils::reservedKeywords =
{
		"alignas", "alignof", "and", "and_eq", "asm", "atomic_cancel", "atomic_commit",
		"atomic_noexcept", "auto", "bitand", "bitor", "bool", "break", "case", "catch",
		"char", "char8_t", "char16_t", "char32_t", "class", "compl", "concept", "const",
		"constval", "constexpr", "const_cast", "const_cast", "continue", "decltype",
		"default", "delete", "do", "double", "dynamic_cast", "else", "enum", "explicit",
		"export", "extern", "false", "float", "for", "friend", "goto", "if", "inline",
		"int", "long", "mutable", "namespace", "new", "noexcept", "not", "not_eq", "nullptr",
		"operator", "or", "or_eq", "private", "protected", "public", "register", "reinterpret_cast",
		"requires", "return", "short", "signed", "sizeof", "static", "static_assert", "static_cast",
		"struct", "switch", "template", "this", "thread_local", "throw", "true", "try", "typedef",
		"typeid", "typename", "union", "unsigned", "using", "virtual", "void", "volatile", "wchar_t",
		"while", "xor", "xor_eq", "override", "final", "audit", "axiom", "read", "write"
};
