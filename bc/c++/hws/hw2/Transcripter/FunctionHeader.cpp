#include "FunctionHeader.hpp"

using std::string;

string FunctionHeader::transcript() const
{
	string parametersConcated;
	for (size_t i = 0; i < parametersCount() - 1 && parametersCount() > 0; ++i)
	{
		parametersConcated += "uint_least64_t " + parameters[i] + ", ";
	}
	if (!parameters.empty())
	{
		parametersConcated += "uint_least64_t " + parameters.back();
	}
	if (name != "main")
	{
		return "uint_least64_t " + name + "(" + parametersConcated + ")";
	}
	return "int " + name + "(" + parametersConcated + ")";
}
