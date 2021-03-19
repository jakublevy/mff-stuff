#include "FunctionManager.hpp"
#include "ContainerUtils.hpp"
#include "ErrorProgram.hpp"

using std::string;		using std::vector;
using std::find;		using std::find_if;
using std::runtime_error;

void FunctionManager::addFunction(Function const& f)
{
	functionsSet.insert(f.name());
	functions.push_back(f);
}

void FunctionManager::addFunction(Function&& f)
{
	functionsSet.insert(f.name());
	functions.push_back(std::move(f));
}

Function const& FunctionManager::getFunction(string const& name) const
{
	if (functionsSet.find(name) != functionsSet.end())
	{
		return *find_if(functions.cbegin(), functions.cend(), [&name](Function const& f)
		{
			return f.name() == name;
		});
	}
	throw runtime_error("Function not available.");
}

string FunctionManager::availableFunctionName(vector<string> const& variables)
{
	string result = "f";
	while (containsFunction(result) && find(variables.cbegin(), variables.cend(), result) == variables.cend())
		result += "f";

	functionsSet.insert(result);
	return result;
}

bool FunctionManager::containsFunction(string const& name) const
{
	return functionsSet.find(name) != functionsSet.end();
}

void FunctionManager::addWriteAndReadFunctions(vector<Function>& vec) const
{
	Function write(FunctionHeader("write", vector<string>{"val"}),
		"std::cout << val << \'\\n\';\n return 0;");
	Function read(FunctionHeader("read", vector<string>()),
		"uint_least64_t ret;\n std::cin >> ret;\n return ret;");
	vec.push_back(std::move(write));
	vec.push_back(std::move(read));
}

string FunctionManager::transcript()
{
	string output;
	vector<Function> outputFunctions;
	addWriteAndReadFunctions(outputFunctions);
	for (auto &function : functions) {
		vector<Function> transripted;
		try
		{
			transripted = function.transcript(this);
		}
		catch (runtime_error const& e)
		{
			return ErrorProgram::generate(e.what());
		}
		outputFunctions.insert(outputFunctions.cend(), transripted.cbegin(), transripted.cend());
	}
	string prototypes;
	for (auto &outputFunction : outputFunctions) {
		prototypes += outputFunction.prototype() + "\n";
		output += outputFunction.pretty();
		output += "\n\n";
	}
	prototypes += "\n";
	return "#include <cstdint>\n#include <iostream>\n\n" + prototypes + output;
}
