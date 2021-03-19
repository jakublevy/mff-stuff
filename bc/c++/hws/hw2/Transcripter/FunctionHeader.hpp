#ifndef _FUNCTION_HEADER_
#define _FUNCTION_HEADER_
#include <string>
#include <vector>
#include <algorithm>

/// <summary>
/// This class contains information about the prototype of a function.
/// </summary>
class FunctionHeader
{
public:
	FunctionHeader(std::string&& name, std::vector<std::string>&& parameters) : name(std::move(name)),
		parameters(std::move(parameters))
	{
	}

	std::string const& getName() const noexcept { return name; }
	size_t parametersCount() const noexcept { return parameters.size(); }
	std::string const& parameterName(size_t idx) const { return parameters[idx]; }

	bool parameterExists(std::string const& name) const
	{
		return std::find(parameters.begin(), parameters.end(), name) != parameters.end();
	}

	std::vector<std::string> const& getAllParameters() const noexcept { return parameters; }

	/// <returns>
	/// Signature of a function in C++.
	/// </returns>
	std::string transcript() const;
private:
	std::string name;
	std::vector<std::string> parameters;
};
#endif
