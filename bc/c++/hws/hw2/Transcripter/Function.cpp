#include "Function.hpp"
#include "FunctionManager.hpp"
#include "StringUtils.hpp"
#include "ContainerUtils.hpp"
#include <regex>
#include <tuple>

using std::string;				using std::vector;
using std::current_exception;	using std::rethrow_exception;
using std::runtime_error;		using std::find;
using std::tuple;				using std::get;
using std::make_tuple;			using std::set;

vector<Function> Function::transcript(FunctionManager* fManager)
{
	vector<Function> newFunctions;
	while (body.find('{') != string::npos && body.find('}') != string::npos)
	{
		auto inner = mostInnerExpression(body);
		if (inner.empty())
		{
			throw runtime_error("Empty { } found.");
		}
		if (inner.find("if") != string::npos)
		{
			try
			{
				replaceIfWithFunction(inner, fManager, newFunctions);
			}
			catch (runtime_error const&)
			{
				rethrow_exception(current_exception());
			}
		}
		else
		{
			string callString;
			try
			{
				callString = makeFunctionReplacement(inner, fManager, newFunctions);
			}
			catch (runtime_error const&)
			{
				rethrow_exception(current_exception());
			}
			inner.insert(0, "{");
			inner.insert(inner.size(), "}");
			substitute(inner, callString);
		}
	}
	try
	{
		replaceIfsWithFunctions(fManager, newFunctions);
	}
	catch (runtime_error const&)
	{
		rethrow_exception(current_exception());
	}
	vector<string> variables = vars();
	if (!variablesValid(variables))
	{
		throw runtime_error("Usage of invalid parameter.");
	}

	try
	{
		check(body);
	}
	catch (runtime_error const&)
	{
		rethrow_exception(current_exception());
	}

	FunctionHeader fh = header;
	if (body.find("return") == string::npos)
	{
		addReturnStatementToLastExpr();
	}
	string b = body;
	Function f(std::move(fh), std::move(b));
	newFunctions.push_back(std::move(f));
	return newFunctions;
}

string Function::pretty() const
{
	string output = header.transcript() + " {\n";
	output += body + "\n}";
	return output;
}

string Function::prototype() const
{
	return header.transcript() + ";";
}

vector<string> Function::vars(vector<string> const& exprs) const
{
	vector<string> variables;
	for (const auto &expr : exprs) {
		vector<string> split = splitToTokens(expr);
		try
		{
			check(split);
		}
		catch (runtime_error const&)
		{
			rethrow_exception(current_exception());
		}
		for (size_t j = 0; j < split.size(); ++j)
		{
			if (StringUtils::regexMatch(split[j], "[a-zA-Z]+") && (j == split.size() - 1 || split[j + 1] != "("))
			{
				if (find(variables.cbegin(), variables.cend(), split[j]) == variables.cend())
					variables.push_back(split[j]);
			}
		}
	}
	return variables;
}

vector<string> Function::vars() const
{
	vector<string> split = StringUtils::splitWithoutMatched(body, ";[ ]*");
	return vars(split);
}

void Function::substitute(string const& searchfor, string const& replacewith)
{
	size_t idx = body.find(searchfor);
	while (idx != string::npos)
	{
		size_t parenthesesEquality = 0;
		for (size_t i = 0; i < idx; ++i)
		{
			if (body[i] == '(')
				++parenthesesEquality;
			else if (body[i] == ')')
				--parenthesesEquality;
		}
		string op1;
		if (idx + searchfor.size() < body.size())
		{
			op1 = string(1, body[idx + searchfor.size()]);
		}

		if (op1 == ";")
		{
			string op3;
			if (idx + searchfor.size() + 1 < body.size())
			{
				op3 = string(1, body[idx + searchfor.size() + 1]);
			}
			string op4;
			if (idx + searchfor.size() + 2 < body.size())
			{
				op4 = string{ body[idx + searchfor.size() + 1], body[idx + searchfor.size() + 2] };
			}

			if (StringUtils::isBinaryOperator(op3) || StringUtils::isBinaryOperator(op4))
			{
				throw runtime_error("Unmatched binary operator.");
			}
		}


		if (idx + searchfor.size() >= body.size() && op1 != ";")
		{
			body.replace(idx, searchfor.size(), replacewith + ";");
		}
		else
		{
			body.replace(idx, searchfor.size(), replacewith);
		}

		idx = body.find(searchfor);
	}
}

void Function::addReturnStatementToLastExpr()
{
	while (body.back() == ';')
	{
		body = body.substr(0, body.size() - 1);
	}
	size_t idx = body.find_last_of(';');
	if (idx == string::npos)
	{
		body = "return " + body + ";";
	}
	else
	{
		body.insert(idx + 1, "\nreturn ");
		body += ';';
	}
}

size_t Function::firstParenthesis(string const& str, size_t start) const
{
	size_t ret = str.find_first_of('(', start);
	if (ret != string::npos)
	{
		return ret;
	}
	throw runtime_error("Unmatched brackets.");
}

void Function::substituteIf(
	tuple<string const &, string const &, string const &, string const &>&& ifParsed,
	FunctionManager* fManager, vector<Function>& newFunctions)
{
	string const& ifString = get<0>(ifParsed);
	string const& trueString = get<1>(ifParsed);
	string const& falseString = get<2>(ifParsed);
	string const& falseTrimmed = get<3>(ifParsed);

	vector<string> parameters = vars(vector<string>{ifString, trueString, falseTrimmed});
	string newBody = ifString + " {\n" + "return " + trueString + ";\n}" + " else {\n" + "return " + falseTrimmed +
		";\n}";
	Function toAdd = generateAuxFunction(std::move(parameters), std::move(newBody), fManager);
	string callStr = callString(toAdd);
	substitute(ifString + trueString + falseString, callStr);
	newFunctions.push_back(std::move(toAdd));
}

void Function::replaceIfsWithFunctions(FunctionManager* fManager, vector<Function>& newFunctions)
{
	size_t i = body.rfind("if");
	while (i != string::npos)
	{
		replaceIfWithFunction(body, fManager, newFunctions, i, true);
		i = body.rfind("if");
	}
}

void Function::replaceIfWithFunction(const string& expr, FunctionManager* fManager,
	vector<Function>& newFunctions, size_t ifPos, bool ifFound)
{
	tuple<string, string, string, string> tuple;
	size_t idx = ifPos;
	if (!ifFound)
	{
		idx = expr.rfind("if");
	}

	try
	{
		tuple = parseIf(expr, idx);
	}
	catch (runtime_error const&)
	{
		rethrow_exception(current_exception());
	}
	string ifString = get<0>(tuple);
	string trueString = get<1>(tuple);
	string falseString = get<2>(tuple);
	string falseTrimmed = get<3>(tuple);
	substituteIf(make_tuple(ifString, trueString, falseString, falseTrimmed), fManager, newFunctions);
}

bool Function::ifFound(const string& expr, size_t startIdx) const
{
	if (expr[startIdx] == 'i')
	{
		return startIdx + 1 < expr.size() && expr[startIdx + 1] == 'f';
	}
	return false;
}

tuple<string, string, string, string> Function::parseIf(
	const string& expr, size_t startIdx) const
{
	if (expr[startIdx + 2] != '(')
	{
		throw runtime_error("Unallowed character after keyword if.");
	}
	size_t ifClosing = StringUtils::matchingBracketIdx(expr, startIdx + 2);
	size_t trueFunctionStart = ifClosing + 1;
	size_t trueFunctionEnd;
	size_t falseFunctionStart;
	size_t falseFunctionEnd;
	try {
		trueFunctionEnd = StringUtils::matchingBracketIdx(expr, firstParenthesis(expr, trueFunctionStart));
		falseFunctionStart = trueFunctionEnd + 1;
		falseFunctionEnd = StringUtils::matchingBracketIdx(expr, firstParenthesis(expr, falseFunctionStart));
	}
	catch (runtime_error const&)
	{
		rethrow_exception(current_exception());
	}
	while (falseFunctionEnd + 1 < expr.size() && expr[falseFunctionEnd + 1] == ';')
		++falseFunctionEnd;


	string ifString = expr.substr(startIdx, ifClosing - startIdx + 1);
	string trueString = expr.substr(trueFunctionStart, trueFunctionEnd - trueFunctionStart + 1);
	string falseString = expr.substr(falseFunctionStart, falseFunctionEnd - falseFunctionStart + 1);
	if (!isalpha(falseString.front()) || !isalpha(trueString.front()))
	{
		throw runtime_error("Invalid character inside if.");
	}
	if (falseString.back() == ';')
	{
		size_t idx = expr.find(falseString) + falseString.size();
		if (idx < expr.size())
		{
			throw runtime_error("Invalid semicolon detected.");
		}
	}
	if (falseString.back() != ';' && falseString.back() != ')')
	{
		throw runtime_error("Invalid character inside if.");
	}
	string falseTrimmed;

	for (char i : falseString) {
		if (i != ';')
			falseTrimmed += i;
	}
	return make_tuple(ifString, trueString, falseString, falseTrimmed);
}

void Function::check(string const& expr) const
{
	vector<string> exprs = StringUtils::splitWithoutMatched(expr, ";[ ]*");
	for (const auto &e : exprs) {
		check(splitToTokens(e));
	}

	if (exprs.empty())
	{
		throw runtime_error("Function " + name() + " does not return any value.");
	}
}

void Function::check(vector<string> const& split) const
{
	auto availableParameters = header.getAllParameters();
	size_t parentheses = 0;
	set<size_t> dissallowed;
	for (size_t j = 0; j < split.size(); ++j)
	{
		if (dissallowed.find(j) != dissallowed.cend())
			continue;

		if (split[j] == "(")
		{
			if (j >= 1 && split[j - 1] == "if")
			{
				dissallowed.insert(
					ContainerUtils::findMatchingParenthesis(split.cbegin(), split.cend(), split.cbegin() + j));
				continue;
			}
			++parentheses;
			if (j == split.size() - 1)
			{
				throw runtime_error("Unmatched (.");
			}
			if (split[j + 1] == ")")
			{
				if (j == 0 || !StringUtils::regexMatch(split[j - 1], "[a-zA-Z]+"))
					throw runtime_error("Found empty ().");
			}
		}

		else if (StringUtils::regexMatch(split[j], "[a-zA-Z]+"))
		{
			if (j + 1 < split.size())
			{
				if (!StringUtils::isBinaryOperator(split[j + 1]) && split[j + 1] != "(" && split[j + 1] != ";" && split[
					j + 1] != "," && split[j + 1] != ")")
				{
					throw runtime_error("Missing semicolon.");
				}
			}
		}

		else if (StringUtils::regexMatch(split[j], "[0-9]+"))
		{
			if (j + 1 < split.size())
			{
				if (split[j + 1] != ";" && split[j + 1] != ")" && !StringUtils::isBinaryOperator(split[j + 1]) && split[
					j + 1] != ",")
				{
					throw runtime_error("Missing semicolon.");
				}
			}
		}

		else if (StringUtils::isUnaryOperator(split[j]))
		{
			if (j + 1 == split.size())
			{
				throw runtime_error("Unmatched unary operator.");
			}
			if (split[j + 1] != "(" && !StringUtils::isUnaryOperator(split[j + 1]) && !StringUtils::regexMatch(
				split[j + 1], "[a-zA-Z0-9]+"))
			{
				throw runtime_error("Unmatched unary operator.");
			}
		}
		else if (StringUtils::isBinaryOperator(split[j]))
		{
			if (j == 0 || j + 1 == split.size())
			{
				throw runtime_error("Unmatched binary operator.");
			}
			if ((split[j - 1] != ")" && !StringUtils::regexMatch(split[j - 1], "[a-zA-Z0-9]+")) ||
				(split[j + 1] != "(" && !StringUtils::regexMatch(split[j + 1], "[a-zA-Z0-9]+")))
			{
				if (!StringUtils::isUnaryOperator(split[j + 1]))
					throw runtime_error("Unmatched binary operator.");
			}
		}


		if (split[j] == ")")
		{
			--parentheses;
			if (j + 1 < split.size() && split[j + 1] == "(")
			{
				throw runtime_error("Missing semicolon.");
			}
			if (parentheses == 0 && j + 1 < split.size())
			{
				if (split[j + 1] != ";" && !StringUtils::isBinaryOperator(split[j + 1]) && (split[j + 1] != ")" ||
					dissallowed.find(j + 1) == dissallowed.cend()))
				{
					throw runtime_error("Missing semicolon.");
				}
			}
		}
	}
	if (parentheses != 0)
	{
		throw runtime_error("Invalid semicolon detected.");
	}
}

bool Function::variablesValid(vector<string> const& variables) const
{
	for (const auto &variable : variables) {
		if (!parameterExists(variable))
		{
			return false;
		}
	}
	return true;
}

string Function::mostInnerExpression(string expr) const
{
	size_t startIdx = 0;
	while (startIdx < expr.size() && ((expr[startIdx] != '{' && expr[startIdx] != '}')))
	{
		++startIdx;
	}

	if (expr[startIdx] == '}')
	{
		throw runtime_error("Malformed function body.");
	}

	if (expr[startIdx] == '{')
	{
		size_t endIdx = StringUtils::matchingBracketIdx(expr, startIdx);
		return mostInnerExpression(expr.substr(startIdx + 1, endIdx - startIdx - 1));
	}
	return expr;
}

vector<string> Function::splitToTokens(string const& expr) const
{
	return StringUtils::splitWithMatched(expr, R"([\+\-\*\~\/\%\<\>\(\)\{\}\,]|!=|==|&&|\|\||\!)");
}

Function Function::generateAuxFunction(vector<string>&& parameters, string&& body,
	FunctionManager* fManager) const
{
	return Function(FunctionHeader(fManager->availableFunctionName(this->parameters()), std::move(parameters)),
		std::move(body));
}

string Function::makeFunctionReplacement(string const& expr, FunctionManager* fManager,
	vector<Function>& newFunctions) const
{
	vector<string> split = StringUtils::splitWithoutMatched(expr, ";[ ]*");

	vector<string> variables = vars(split);

	split.back() = "return " + split.back();
	string output;
	for (auto &i : split) {
		if (i.size() > 1 && i[0] == 'i' && i[1] == 'f')
		{
			size_t end = StringUtils::matchingBracketIdx(i, 2);
			i.insert(end + 1, " return ");
		}

		output += i + ";\n";
	}
	Function f = generateAuxFunction(std::move(variables), std::move(output), fManager);
	output = callString(f);
	newFunctions.push_back(std::move(f));

	return output;
}

string Function::callString(Function const& f)
{
	string o1 = f.name() + "(";
	string o3 = ")";
	string o2;
	for (size_t i = 0; i < f.parametersCount() - 1 && f.parametersCount() > 0; ++i)
	{
		o2 += f.parameterName(i) + ", ";
	}
	if (f.parametersCount() > 0)
		o2 += f.parameterName(f.parametersCount() - 1);

	return o1 + o2 + o3;
}
