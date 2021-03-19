#ifndef _FUNCTION_MANAGER_
#define _FUNCTION_MANAGER_
#include <set>
#include <vector>
#include "Function.hpp"

/// <summary>
/// This function manages original S# functions and transcripted new C++ functions.
/// </summary>
class FunctionManager
{
public:
	FunctionManager() = default;

	void addFunction(Function const& f);
	void addFunction(Function&& f);

	Function const& getFunction(std::string const& name) const;
	Function const& getFunction(size_t idx) const { return functions[idx]; }
	Function& getFunction(size_t idx) { return functions[idx]; }


	/// <summary>
	/// If piece of code in S# needs to be transcripted into a function call of a new function
	/// this method procures available function name.
	/// </summary>
	/// <param name="variables">Variables used inside of a function body where is the new function will be called.</param>
	/// <returns>
	/// Available function name.
	/// </returns>
	std::string availableFunctionName(std::vector<std::string> const& variables);


	bool containsFunction(std::string const& name) const;


	/// <summary>
	/// Adds two functions (read, write) to supplied collection.
	/// </summary>
	/// <param name="vec">Collection to be updated.</param>
	void addWriteAndReadFunctions(std::vector<Function>& vec) const;


	/// <returns>
	/// A valid C++ program, either the transcripted one of the error one.
	/// </returns>
	std::string transcript();

	size_t functionsCount() const { return functions.size(); }

private:
	/// <summary>
	/// A set of all functions' names (from S# and new ones from C++).
	/// Used for fast lookups.
	/// </summary>
	std::set<std::string> functionsSet;


	/// <summary>
	/// A vector of all function objects.
	/// </summary>
	std::vector<Function> functions;
};
#endif
