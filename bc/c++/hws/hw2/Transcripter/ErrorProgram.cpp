#include "ErrorProgram.hpp"

using std::string;

//It is sufficient to just return non-zero value.
//No error messages required.
string ErrorProgram::generate(char const* msg)
{
	/*	std::string s1 =
			"#include <iostream>\n" \
			"int main() {\n" \
			"   std::cout << ";
		std::string s2 =
			" << std::endl;\n" \
			"   return 55;\n" \
			"}";
	*/
	//return s1 + "\"" + msg + "\"" + s2;


	string s = "#include <iostream>\n\n"
		"int main() {\n"
		"   std::cerr << \"error\" << \'\\n\';\n"
		"   return 55;\n"
		"}";
	return s;
}
