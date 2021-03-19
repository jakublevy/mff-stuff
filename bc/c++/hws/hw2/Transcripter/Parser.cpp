#include "Parser.hpp"
#include <locale>
#include <algorithm>
#include "Function.hpp"
#include <regex>
#include <map>
#include "StringUtils.hpp"
#include "ContainerUtils.hpp"
#include "ErrorProgram.hpp"

using std::rethrow_exception;	using std::current_exception;
using std::string;				using std::vector;
using std::runtime_error;



Parser::Parser(string&& source) : source(std::move(source))
{
	StringUtils::trim(this->source);
	this->source = StringUtils::maxOneSpaceAndSemicolon(this->source);
}

string Parser::transcript()
{
	try
	{
		parseFunctions();
	}
	catch (runtime_error const& e)
	{
		return ErrorProgram::generate(e.what());
	}

	vector<Function> compiled;

	return fManager.transcript();
}

void Parser::parseFunctions()
{
	string sourceCopy = source;
	size_t pos = sourceCopy.find('{');
	while (pos != string::npos)
	{
		string header = sourceCopy.substr(0, pos);
		StringUtils::trim(header);
		if (!StringUtils::fPrototypeMatch(header))
		{
			throw runtime_error("Function prototype contains unallowed chars.");
		}
		vector<string> headerSplit = StringUtils::splitWithoutMatched(header, "[ ]+");
		if (!headerSplit.empty())
		{
			string fName = headerSplit[0];
			if (ContainerUtils::containsReservedKeywords(headerSplit.cbegin(), headerSplit.cend()))
			{
				throw runtime_error("Inappropriate name for function/parameter.");
			}
			headerSplit.erase(headerSplit.cbegin());
			if (ContainerUtils::containsDuplicates(headerSplit.cbegin(), headerSplit.cend()))
			{
				throw runtime_error("Duplicate parameter name.");
			}

			FunctionHeader fh(std::move(fName), std::move(headerSplit));

			if (fh.getName() == "main" && fh.parametersCount() > 0)
			{
				throw runtime_error("Main function can't take parameters.");
			}

			size_t endPos = StringUtils::matchingBracketIdx(sourceCopy, pos);
			string body = sourceCopy.substr(pos + 1, endPos - pos - 1);
			try
			{
				checkForMissingSemicolons(body);
			}
			catch (runtime_error const&)
			{
				rethrow_exception(current_exception());
			}
			body = StringUtils::deleteWhiteSpaces(body);

			if (containsUnallowedChars(body))
			{
				throw runtime_error("Unallowed characters inside function " + fh.getName() + " detected.");
			}

			Function f(std::move(fh), std::move(body));
			if (!fManager.containsFunction(f.name()))
			{
				fManager.addFunction(f);
			}
			else
			{
				throw runtime_error("Redefinition of a function " + f.name());
			}

			sourceCopy = sourceCopy.substr(endPos + 1, sourceCopy.size() - endPos);
			pos = sourceCopy.find('{');
		}
		else
		{
			throw runtime_error("Missing function name.");
		}
	}
	if (!fManager.containsFunction("main"))
	{
		throw runtime_error("Missing main function.");
	}
	if (!sourceCopy.empty())
	{
		throw runtime_error(("Malformed \\\"" + sourceCopy + "\\\"").c_str());
	}
	try
	{
		checkMatchingBrackets(fManager);
		checkUsageOfFunctions(fManager);
	}
	catch (runtime_error const&)
	{
		rethrow_exception(current_exception());
	}
	replaceTilde(fManager);
}

string Parser::longestleftAlphabeticalMatch(string const& expr, size_t endIdx) const
{
	size_t startIdx = endIdx - 1;
	while (startIdx > 0 && isalpha(expr[startIdx]))
	{
		--startIdx;
	}
	if (startIdx == 0 && isalpha(expr[startIdx]))
	{
		return expr.substr(0, endIdx);
	}
	return expr.substr(startIdx + 1, endIdx - startIdx - 1);
}

void Parser::checkUsageOfFunctions(FunctionManager const& fManager) const
{
	for (size_t i = 0; i < fManager.functionsCount(); ++i)
	{
		string body = fManager.getFunction(i).getBody();

		size_t off = 0;
		size_t start = body.find_first_of('(', off);

		while (start != string::npos)
		{
			size_t end = StringUtils::matchingBracketIdx(body, start);
			size_t argCount = StringUtils::argumentsCount(body, start, end - start + 1);

			string llm;
			if (start > 0)
			{
				if (isalpha(body[start - 1]))
				{
					llm = longestleftAlphabeticalMatch(body, start);
				}
				else if (argCount != 1 || isalpha(body[start - 1]))
				{
					throw runtime_error("Syntactic error.");
				}


				if (((llm == "if" || llm == "write" || !isalpha(body[start - 1])) && argCount == 1) || (llm == "read" &&
					argCount == 0))
				{
					off = start + 1;
					start = body.find_first_of('(', off);
					continue;
				}
			}
			if (!llm.empty())
			{
				if (StringUtils::isReservedKeyword(llm))
				{
					throw runtime_error("Calling function name is reserved keyword in C/C++");
				}
				if (!fManager.containsFunction(llm))
				{
					throw runtime_error("Calling function name does not exist.");
				}

				if (fManager.getFunction(i).parameterExists(llm))
				{
					throw runtime_error(
						"Parameter and calling function name collision occured inside " + fManager
						.getFunction(i).name());
				}

				if (fManager.getFunction(llm).parametersCount() != argCount)
				{
					throw runtime_error("Invalid number of arguments calling function " + llm);
				}
			}
			off = start + 1;
			start = body.find_first_of('(', off);
		}
	}
}

void Parser::checkMatchingBrackets(FunctionManager const& fManager) const
{
	int braces = 0;
	int parentheses = 0;
	for (size_t i = 0; i < fManager.functionsCount(); ++i)
	{
		string body = fManager.getFunction(i).getBody();
		for (char j : body) {
			if (braces < 0 || parentheses < 0)
			{
				throw runtime_error("Unmatched brackets.");
			}

			if (j == '(')
			{
				++parentheses;
			}
			else if (j == ')')
			{
				--parentheses;
			}
			else if (j == '{')
			{
				++braces;
			}
			else if (j == '}')
			{
				--braces;
			}
		}
	}
	if (braces != 0 || parentheses != 0)
	{
		throw runtime_error("Unmatched brackets.");
	}
}

bool Parser::containsUnallowedChars(string const& body) const
{
	for (size_t i = 0; i < body.size(); ++i)
	{
		if (isalpha(body[i]) || isdigit(body[i]))
			continue;

		if (body[i] == '{' || body[i] == '}' || body[i] == '(' || body[i] == ')' || body[i] == ';' || body[i] == '+' ||
			body[i] == '-'
			|| body[i] == '*' || body[i] == '/' || body[i] == '%' || body[i] == '<' || body[i] == '>' || body[i] == ','
			|| body[i] == '~')
		{
			continue;
		}
		if (body[i] + 1 == body.size())
		{
			return true;
		}
		if (body[i] == '=' && body[i + 1] == '=')
		{
			++i;
			continue;
		}
		if (body[i] == '!' && body[i + 1] == '=')
		{
			++i;
			continue;
		}

		if (body[i] == '&' && body[i + 1] == '&')
		{
			++i;
			continue;
		}
		if (body[i] == '|' && body[i + 1] == '|')
		{
			++i;
			continue;
		}
		return true;
	}
	return false;
}

void Parser::replaceTilde(FunctionManager& fManager) const
{
	for (size_t i = 0; i < fManager.functionsCount(); ++i)
	{
		string const& body = fManager.getFunction(i).getBody();
		string r = StringUtils::replaceAll(body, "~", "!");
		fManager.getFunction(i).setBody(std::move(r));
	}
}

void Parser::checkForMissingSemicolons(string const& body) const
{
	for (size_t i = 1; i < body.size() - 1; ++i)
	{
		if (body[i] == ' ')
		{
			if ((isalpha(body[i - 1]) || isdigit(body[i - 1])) && (isalpha(body[i + 1]) || isdigit(body[i + 1])))
			{
				throw runtime_error("Missing semicolon.");
			}
		}
	}
}
