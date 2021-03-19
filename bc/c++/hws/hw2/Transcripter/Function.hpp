#ifndef _FUNCTION_
#define _FUNCTION_
#include "FunctionHeader.hpp"

class FunctionManager;

class Function
{
public:
	explicit Function(FunctionHeader&& header) : header(std::move(header))
	{
	}

	Function(FunctionHeader&& header, std::string&& body) : header(std::move(header)), body(std::move(body))
	{
	}

	size_t parametersCount() const noexcept { return header.parametersCount(); }
	bool parameterExists(std::string const& name) const { return header.parameterExists(name); }

	std::string const& name() const noexcept { return header.getName(); }

	std::vector<Function> transcript(FunctionManager* fManager);


	/// <returns>
	/// Formatted string of function header and body separated by {}
	/// </returns>
	std::string pretty() const;


	/// <returns>
	/// Returns function's prototype (header + ';')
	/// </returns>
	std::string prototype() const;

	std::string const& getBody() const noexcept { return body; }
	std::string& getBody() noexcept { return body; }
	void setBody(std::string&& body) { this->body = std::move(body); }

private:
	std::vector<std::string> const& parameters() const noexcept { return header.getAllParameters(); }
	std::string parameterName(size_t idx) const { return header.parameterName(idx); }


	/// <param name="exprs">Tokenized collection where should this function search for variables.</param>
	/// <returns>
	/// Returns a collection of variables used inside this function.
	/// </returns>
	std::vector<std::string> vars(std::vector<std::string> const& exprs) const;


	/// <returns>
	/// Returns a collection of variables used inside this function.
	/// </returns>
	std::vector<std::string> vars() const;


	/// <summary>
	/// Replaces occurence of first string with second string.
	/// Used for replacing parts of code in S# to function calls in C++.
	/// </summary>
	/// <param name="searchfor"></param>
	/// <param name="replacewith"></param>
	void substitute(std::string const& searchfor, std::string const& replacewith);


	/// <summary>
	/// Adds return statement before last expression (expressions are separated by ;)
	/// </summary>
	void addReturnStatementToLastExpr();


	/// <param name="str">String to search for ( occurence</param>
	/// <param name="start">Starting idx to search from.</param>
	/// <returns>Idx of first occurrence of (</returns>
	size_t firstParenthesis(std::string const& str, size_t start) const;


	/// <summary>
	/// Creates a new auxiliary function from ifParsed, replaces if statement with function call.
	/// </summary>
	/// <param name="ifParsed">If statement to be replaced.</param>
	/// <param name="fManager">Function manager of already existing functions from S# (required for getting available function name).</param>
	/// <param name="newFunctions">Collection where auxiliary function will be added.</param>
	void substituteIf(
		std::tuple<std::string const &, std::string const &, std::string const &, std::string const &>&& ifParsed,
		FunctionManager* fManager, std::vector<Function>& newFunctions);


	/// <summary>
	/// Iterated version of replaceIfWithFunction. Cycle stops when there aren't any if statements left.
	/// body is used as an expr.
	/// </summary>
	/// <param name="fManager">Required for calling substituteIf function.</param>
	/// <param name="newFunctions">Required for calling substituteIf function.</param>
	void replaceIfsWithFunctions(FunctionManager* fManager, std::vector<Function>& newFunctions);


	/// <summary>
	/// Calls substituteIf function for if statement inside expr (if statement are parsed using parseIf).
	/// </summary>
	/// <param name="expr">String to find if statements in.</param>
	/// <param name="fManager">Required for calling substituteIf function.</param>
	/// <param name="newFunctions">Required for calling substituteIf function.</param>
	/// <param name="ifPos">Position of if statement.</param>
	/// <param name="ifFound">Flag indicating whether if statement position is yet to be found.</param>
	///
	void replaceIfWithFunction(const std::string& expr, FunctionManager* fManager, std::vector<Function>& newFunctions,
		size_t ifPos = 0, bool ifFound = false);


	/// <param name="expr">String to find if statement in.</param>
	/// <param name="startIdx">Where to find if statement.</param>
	/// <returns>Returns flag indicating whether on startIdx in expr starts if statement.</returns>
	bool ifFound(std::string const& expr, size_t startIdx) const;


	/// <param name="expr">Expr from which if statement should be parsed.</param>
	/// <param name="startIdx">Idx where if statement starts.</param>
	/// <returns>Tuple of parsed if.</returns>
	//ifString  trueString   falseString  falseTrimmed
	std::tuple<std::string, std::string, std::string, std::string> parseIf(
		std::string const& expr, size_t startIdx) const;


	/// <summary>
	/// Tokenizes exprs and calls second overload of check function
	/// that performs syntactic check for given string.
	/// Calls the second function check.
	/// </summary>
	/// <param name="exprs">String to syntactically check.</param>
	void check(std::string const& exprs) const;


	/// <summary>
	/// Performs syntactic check of tokenized input.
	/// </summary>
	/// <param name="split">tokenized expression</param>
	void check(std::vector<std::string> const& split) const;


	/// <summary>
	/// Checks whether used variables are defined.
	/// </summary>
	/// <param name="variables">Collection of variables.</param>
	/// <returns>Boolean flag indicating whether all variables are defined.</returns>
	bool variablesValid(std::vector<std::string> const& variables) const;


	/// <summary>
	/// Find the left most inner expression (the most inside {{{{}}}})
	/// If there are more expressions outputs first the one it finds first (the one on the left)
	/// Example:
	///    expr = "x * if (x>1) {fact(x-1)} {1}"
	///    output = fact(x-1)
	/// Notice: Passing by value is necessary.
	/// </summary>
	/// <param name="expr">String to look for expressions.</param>
	/// <returns>left most inner expression.</returns>
	std::string mostInnerExpression(std::string expr) const;


	std::vector<std::string> splitToTokens(std::string const& expr) const;


	/// <param name="parameters">Parameters of the new function</param>
	/// <param name="body">Body of the new function.</param>
	/// <param name="fManager">Function manager to add function to.</param>
	/// <returns>New function with available name with supplied parameters and body.</returns>
	Function generateAuxFunction(std::vector<std::string>&& parameters, std::string&& body,
		FunctionManager* fManager) const;


	/// <summary>
	/// Creates new function for a piece of code in expr. Adds that function to a collection of newFunctions.
	/// </summary>
	/// <param name="expr">Expr to create function for.</param>
	/// <param name="fManager">Function manager of S# functions.</param>
	/// <param name="newFunctions">Collection of auxiliary functions created while transcripting S# to C++.</param>
	/// <returns>Call string for new function.</returns>
	std::string makeFunctionReplacement(std::string const& expr, FunctionManager* fManager,
		std::vector<Function>& newFunctions) const;


	/// <summary>
	/// Returns a string to call function with.
	/// </summary>
	/// <param name="f">Function we want to call.</param>
	/// <returns>String that can be used to call function f.</returns>
	static std::string callString(Function const& f);

	FunctionHeader header;
	std::string body;
};
#endif
