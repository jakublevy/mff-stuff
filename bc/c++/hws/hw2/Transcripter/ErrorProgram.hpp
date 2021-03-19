#ifndef _ERROR_PROGRAM_
#define _ERROR_PROGRAM_
#include <string>

class ErrorProgram
{
public:
	/// <summary>
	/// When exception is throw during analysis of input, this method
	/// is responsible for generating program reporting error.
	/// </summary>
	/// <param name="msg">Unused, because "error" is sufficient report.</param>
	/// <returns>
	/// A valid C++ program that outputs "error" to std::cerr
	/// and returns non-zero value (55).
	/// </returns>
	static std::string generate(char const* msg);
};
#endif
