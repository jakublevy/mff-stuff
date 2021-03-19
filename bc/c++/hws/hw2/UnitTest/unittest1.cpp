#include "stdafx.h"
#include "CppUnitTest.h"
#include <fstream>
#include <cstdio>
#include <stdexcept>
#include <string>
#include <array>
#include "../Transcripter/Parser.hpp"
#include <Windows.h>


#define MULTILINE(...) #__VA_ARGS__

using namespace Microsoft::VisualStudio::CppUnitTestFramework;
using namespace std;

namespace UnitTest
{
	TEST_CLASS(UnitTest1)
	{
	public:

		string ExePath() {
			char buffer[MAX_PATH];
			GetModuleFileNameA(NULL, buffer, MAX_PATH);
			string::size_type pos = string(buffer).find_last_of("\\/");
			return string(buffer).substr(0, pos);
		}

		BOOL FileExists(LPCSTR szPath)
		{
			DWORD dwAttrib = GetFileAttributesA(szPath);

			return (dwAttrib != INVALID_FILE_ATTRIBUTES &&
				!(dwAttrib & FILE_ATTRIBUTE_DIRECTORY));
		}

		bool exec(string const &cmd, string const& fCheck)
		{
			DWORD errorCode;
			STARTUPINFOA si;
			PROCESS_INFORMATION pi;

			ZeroMemory(&si, sizeof(si));
			si.cb = sizeof(si);
			ZeroMemory(&pi, sizeof(pi));


			char *cmdW = new char[cmd.size() + 1];
			copy(cmd.begin(), cmd.end(), cmdW);
			cmdW[cmd.size()] = '\0';																//CREATE_NEW_CONSOLE
			if (CreateProcessA(NULL, cmdW, NULL, NULL, FALSE, NORMAL_PRIORITY_CLASS | CREATE_NO_WINDOW | CREATE_UNICODE_ENVIRONMENT, NULL, NULL, &si, &pi))
			{
				WaitForSingleObject(pi.hProcess, INFINITE);
				CloseHandle(pi.hProcess);
				CloseHandle(pi.hThread);

			}
			else
			{
				return 55;
			}
			delete[] cmdW;
			string d = ExePath();
			return FileExists(fCheck.c_str());

		}

		string transcript(string const &str)
		{
			string s = str;
			Parser p(move(s));
			return p.transcript();
		}
		tuple<int, string> compile(string const &src)
		{
			string fileName = (string(tmpnam(nullptr)) + ".cpp");
			string shouldExists = fileName;
			FILE *handle = fopen(fileName.c_str(), "w+");
			fputs(src.c_str(), handle);
			fclose(handle);
			string path = R"(C:\Users\jakub\Documents\Visual Studio 2017\Projects\Transcripter\cl.bat)";
			string cmd = string(R"(")" + fileName + R"(")");
			shouldExists = shouldExists.replace(fileName.size() - 1 - 2, 3, "exe");
			bool ret = exec(path + " " + fileName + " /Fe:" + shouldExists, shouldExists);

			remove(fileName.c_str());

			return make_tuple(ret, shouldExists);

		}

		tuple<int, string> execProgram(const string& fileName, const vector<int>& params)
		{
			string fakeStdInFileName = (string(tmpnam(nullptr)) + ".txt");
			string fakeStdIn = "< " + fakeStdInFileName;
			string toSave;
			for (size_t i = 0; i < params.size(); ++i)
			{
				toSave += to_string(params[i]) + " ";
			}
			FILE *handle = fopen(fakeStdInFileName.c_str(), "w+");
			fputs(toSave.c_str(), handle);
			fclose(handle);


			string stdOutFileName = (string(tmpnam(nullptr)) + ".txt");
			string fakeStdOut = "> " + stdOutFileName;

			FILE *handle2 = fopen(stdOutFileName.c_str(), "w+");
			fputs("", handle2);
			fclose(handle2);

			string cmd = fileName + " " + fakeStdIn + " " + fakeStdOut;
			char *cmdW = new char[cmd.size() + 1];
			copy(cmd.begin(), cmd.end(), cmdW);
			cmdW[cmd.size()] = '\0';

			int ret = system(cmdW);
			delete[] cmdW;
			if (ret != 55)
			{
				ifstream in(stdOutFileName);
				string output = string(istreambuf_iterator<char>(in), istreambuf_iterator<char>());

				remove(fakeStdInFileName.c_str());
				remove(stdOutFileName.c_str());

				return make_tuple(ret, output);
			}


			remove(fakeStdInFileName.c_str());
			remove(stdOutFileName.c_str());

			return make_tuple(ret, "chybovy program");




		}

		tuple<int, string> all(string const &str, vector<int> const &parameters)
		{
			string transcripted = transcript(str);
			auto ret = compile(transcripted);
			string toRun = get<1>(ret);
			if (get<0>(ret) == true)
			{
				return execProgram(toRun, parameters);
			}
			else
			{
				return make_tuple(-1, "nezkompilovalo se to");
			}
		}

		void check(tuple<int, string> const &got, tuple<int, string> const &expected)
		{
			int r1 = get<0>(got);
			string o1 = get<1>(got);

			int r2 = get<0>(expected);
			string o2 = get<1>(expected);

			Assert::AreEqual(r2, r1);
			Assert::AreEqual(o2, o1);
		}

		void check(int gotRetVal, int expectedRetVal)
		{
			Assert::AreEqual(expectedRetVal, gotRetVal);
		}
		TEST_METHOD(kratochvilsInputs)
		{
			string i = MULTILINE(test u v{ u + v + u * v }
				fact x{ x * if (x > 1) { fact(x - 1) } {1} }
				main{
				  write(test(read(),
							 read()));
				  write(fact(5));
				  0
				});

			auto t = all(i, vector<int>{167, 11});
			check(t, make_tuple(0, "2015\n120\n"));

			t = all(i, vector<int>{2346, 1234});
			check(t, make_tuple(0, "2898544\n120\n"));




			i = MULTILINE(fun x{ write(x); if (x > 0) { fun(x - 1); } {0} }
			main{ fun(read()) });

			t = all(i, vector<int>{15});
			check(t, make_tuple(0, "15\n14\n13\n12\n11\n10\n9\n8\n7\n6\n5\n4\n3\n2\n1\n0\n"));




			i = MULTILINE(main{ write({write(3); 4}) });
			t = all(i, vector<int>());
			check(t, make_tuple(0, "3\n4\n"));


			i = MULTILINE(func x1 x1{ func2(x2,x1) }
			Main x{ func(1,2,3) - });
			t = all(i, vector<int>{12});
			check(get<0>(t), 55);
		}

		TEST_METHOD(mainMethod)
		{
			string i = MULTILINE(main x{ write(10) });
			auto t = all(i, vector<int>{155});
			check(get<0>(t), 55);

			i = MULTILINE(Main x{ write(10) });
			t = all(i, vector<int>{155});
			check(get<0>(t), 55);

			i = MULTILINE(Main{ write(10) });
			t = all(i, vector<int>());
			check(get<0>(t), 55);

			i = MULTILINE(main{ write(10) });
			t = all(i, vector<int>());
			check(t, make_tuple(0, "10\n"));

			i = MULTILINE(main{ write(10) }
			main{ 0;; });
			t = all(i, vector<int>());
			check(get<0>(t), 55);

			i = MULTILINE(Main xYzYY{ write(xYzYY); }
			main{ Main(read()) });
			t = all(i, vector<int>{2055});
			check(t, make_tuple(0, "2055\n"));
		}

		TEST_METHOD(usageOfVariables)
		{
			string i = MULTILINE(test x{ ; 1; }
			main{ test() });

			auto t = all(i, vector<int>{12});
			check(get<0>(t), 55);

			i = MULTILINE(test x{ ; 1; }
			main{ test(1,2) });
			t = all(i, vector<int>{22});
			check(get<0>(t), 55);


			i = MULTILINE(test x{ ; 1; }
			main{ test(1) });
			t = t = all(i, vector<int>());
			check(get<0>(t), 1);

			i = MULTILINE(test x{ ; 1; }
			main{ test });
			t = t = all(i, vector<int>());
			check(get<0>(t), 55);

			i = MULTILINE(test x, y, z{ ; 1; }
			main{ test(1,2,3) });
			t = t = all(i, vector<int>());
			check(get<0>(t), 55);

			i = MULTILINE(test x Y Z{ ; x + Y * Z; }
				jedna{ 1 }
			dva{ ;;; {; {; {; 2; };; };; }; }
			cislo A{ ; A;; }
			main{ test(jedna(), dva(), cislo(read())) });
			t = all(i, vector<int>{12});
			check(get<0>(t), 25);

			i = MULTILINE(test x Y Z{ ; x + Y * Z; }
				jedna{ 1 }
			dva a b{ ;;; {; {; {; 2; };; };; }; }
				dummy a{ 1 }
			cislo A{ ; A;; }
			main{ test(jedna(), dva(5,dummy(3)), cislo(read())) });
			t = all(i, vector<int>{15});
			check(get<0>(t), 31);

			i = MULTILINE(test x{ X }
			main{ test(5) });
			t = all(i, vector<int>());
			check(get<0>(t), 55);

			i = MULTILINE(test x y z{ x }
			main{ test(2); });
			t = all(i, vector<int>());
			check(get<0>(t), 55);

			i = MULTILINE(test x{ x }
			main{ test(2,); });
			t = all(i, vector<int>());
			check(get<0>(t), 55);
		}

		TEST_METHOD(unaryOperator)
		{
			string i = MULTILINE(test x y{ ~(x + y) }
			main{ test(read(),read()) });

			auto t = all(i, vector<int>{1, 2});
			check(get<0>(t), 0);
			t = all(i, vector<int>{0, 0});
			check(get<0>(t), 1);


			i = MULTILINE(test x y{ ~x + y }
			sub x{ test(x, read()); }
			main{ sub(read()) });
			t = all(i, vector<int>{0, 5});
			check(get<0>(t), 6);
			t = all(i, vector<int>{3, 5});
			check(get<0>(t), 5);

			i = MULTILINE(test x y{ ~~x + y }
			sub x{ test(x, read()); }
			main{ sub(read()) });
			t = all(i, vector<int>{15, 2});
			check(get<0>(t), 3);

			i = MULTILINE(test x y{ ~~~~x + y }
			sub x{ test(x, read()); }
			main{ sub(read()) });
			t = all(i, vector<int>{15, 2});
			check(get<0>(t), 3);

			i = MULTILINE(test x y{ ~~~x + y }
			sub x{ test(x, read()); }
			main{ sub(read()) });
			t = all(i, vector<int>{0, 3});
			check(get<0>(t), 4);

			i = MULTILINE(test x y{ ~(~(~x)) + y }
			sub x{ test(x, read()); }
			main{ sub(read()) });
			t = all(i, vector<int>{0, 3});
			check(get<0>(t), 4);


			i = MULTILINE(test x y{ (~(~(~(x)))) + y }
			sub x{ test(x, read()); }
			main{ sub(read()) });
			t = all(i, vector<int>{0, 3});
			check(get<0>(t), 4);

			i = MULTILINE(test x y{ (~((~(~(x)))) + y) }
			sub x{ test(x, read()); }
			main{ sub(read()) });
			t = all(i, vector<int>{5, 6});
			check(get<0>(t), 6);

			i = MULTILINE(f ~{1}
			main{ f(); });
			t = all(i, vector<int>());
			check(get<0>(t), 55);

			i = MULTILINE(f x{ ~(if (x > 5) { 1 } {0}) }
			main{ write(f(1)) });
			t = all(i, vector<int>());
			check(t, make_tuple(0, "1\n"));

			i = MULTILINE(f x{ ~if (x > 5) { 1 } {0} }
			main{ write(f(1)) });
			t = all(i, vector<int>());
			check(t, make_tuple(0, "1\n"));

			i = MULTILINE(f x{ ~if (x > 5) { 1 } {0} }
			main{ write(f(6)) });
			t = all(i, vector<int>());
			check(t, make_tuple(0, "0\n"));

			i = MULTILINE(f x{ ~if (x > 5) ~{ 1 } {0} }
			main{ write(f(6)) });
			t = all(i, vector<int>());
			check(get<0>(t), 55);

			i = MULTILINE(f x{ ~if (x > 5) { 1 } ~{0} }
			main{ write(f(6)) });
			t = all(i, vector<int>());
			check(get<0>(t), 55);

			i = MULTILINE(f x{ ~if (x > 5) { 1 } {~{1;; };; } }
			main{ write(f(1)) });
			t = all(i, vector<int>());
			check(t, make_tuple(0, "1\n"));



		}

		TEST_METHOD(binaryOperators)
		{
			string i = MULTILINE(test{ (1 + 2) == (5 + 6) }
			main{ write(test()) });
			auto t = all(i, vector<int>());
			check(t, make_tuple(0, "0\n"));

			i = MULTILINE(test{ (((1 + 2) == (5 + 6))) }
			main{ write(test()) });
			t = all(i, vector<int>());
			check(t, make_tuple(0, "0\n"));

			i = MULTILINE(test{ 1 + 2 == 5 + 6 }
			main{ write(test()) });
			t = all(i, vector<int>());
			check(t, make_tuple(0, "0\n"));

			i = MULTILINE(test{ 1 + (2 == 5) + 6 }
			main{ write(test()) });
			t = all(i, vector<int>());
			check(t, make_tuple(0, "7\n"));

			i = MULTILINE(test{ 1 + (2 = 5) + 6 }
			main{ write(test()) });
			t = all(i, vector<int>());
			check(get<0>(t), 55);

			i = MULTILINE(gcd a b{ if (~(b != 0)) { a } { gcd(b, a % b) } }
				sub a{ gcd(a, read()) }
			main{ write(sub(read())) });
			t = all(i, vector<int>{54, 66});
			check(t, make_tuple(0, "6\n"));
			t = all(i, vector<int>{8, 48});
			check(t, make_tuple(0, "8\n"));
			t = all(i, vector<int> {21346, 12350});
			check(t, make_tuple(0, "26\n"));


			i = MULTILINE(gcd a b{ if ~(b != 0) { a } { gcd(b, a % b) } }
				sub a{ gcd(a, read()) }
			main{ write(sub(read())) });
			t = all(i, vector<int>{123, 14253});
			check(get<0>(t), 55);

			i = MULTILINE(test x{ if ({write(x); x > 1 && x < 1001}) { test(x / 2) } {0} }
			main{ test(read()) });
			t = all(i, vector<int>{1000});
			check(t, make_tuple(0, "1000\n500\n250\n125\n62\n31\n15\n7\n3\n1\n"));

			t = all(i, vector<int>{1001});
			check(t, make_tuple(0, "1001\n"));


			i = MULTILINE(test x{ if ({write(x); x > 1 & x < 1001}) { test(x / 2) } {0} }
			main{ test(read()) });
			t = all(i, vector<int>{1000});
			check(get<0>(t), 55);


			i = MULTILINE(test x{ if ({write(x); x < 21 | x > 49}) { test(x / 2) } {0} }
			main{ test(read()) });
			t = all(i, vector<int>{1000});
			check(get<0>(t), 55);

			i = MULTILINE(test x{ if ({write(x); x > 1 && (x < 21 || x > 49)}) { test(x / 2) } {0} }
			main{ test(read()) });
			t = all(i, vector<int>{25});
			check(t, make_tuple(0, "25\n"));
			t = all(i, vector<int>{10});
			check(t, make_tuple(0, "10\n5\n2\n1\n"));
			t = all(i, vector<int>{50});
			check(t, make_tuple(0, "50\n25\n"));

			i = MULTILINE(f x{ ~if (x > 5) { 1 } {0 - ~{1;; } +1; } }
			main{ write(f(1)) });
			t = all(i, vector<int>());
			check(t, make_tuple(0, "0\n"));

			i = MULTILINE(f x{ ~if (x > 5) { 1 } {0 - {0;; } +1; } }
			main{ write(f(1)) });
			t = all(i, vector<int>());
			check(t, make_tuple(0, "0\n"));

			i = MULTILINE(f x{ ~if (x > 5) { 1 } {0 - {0;; } +1; } }
			main{ write(f(6)) });
			t = all(i, vector<int>());
			check(t, make_tuple(0, "0\n"));

			i = MULTILINE(test{ {1} *{ {2} +{ {3}}} }
			main{ write(test()) });
			t = all(i, vector<int>());
			check(t, make_tuple(0, "5\n"));

			i = MULTILINE(test{ {1} *({2} +{ {3}}) }
			main{ write(test()) });
			t = all(i, vector<int>());
			check(t, make_tuple(0, "5\n"));
		}

		TEST_METHOD(conditions)
		{
			string i = MULTILINE(test x{ if (x > 5 || x == 5) }
			main{ test(4); test(5) });
			auto t = all(i, vector<int>());
			check(get<0>(t), 55);

			i = MULTILINE(test x{ if (x > 5 || x == 5) { 44; } }
			main{ test(4); test(5) });
			t = all(i, vector<int>());
			check(get<0>(t), 55);

			i = MULTILINE(test x{ if (x > 5 || x == 5) + 1 + { 44; } }
			main{ test(4); test(5) });
			t = all(i, vector<int>());
			check(get<0>(t), 55);

			i = MULTILINE(test x{ if (x > 5 || x == 5) { 44; } +1 }
			main{ test(4); test(5) });
			t = all(i, vector<int>());
			check(get<0>(t), 55);

			i = MULTILINE(test x{ 3 * if (x > 5 || x == 5) { 44; } }
			main{ test(4); test(5) });
			t = all(i, vector<int>());
			check(get<0>(t), 55);

			i = MULTILINE(test x{ if x > 5 || x == 5 {44; } {4} }
			main{ test(4); test(5) });
			t = all(i, vector<int>());
			check(get<0>(t), 55);

			i = MULTILINE(ahoj y{ if (y == 5) { 1 } {0} +if (y == 5) { 1 } {0} }
			main{ ahoj(read()) });
			t = all(i, vector<int>{5});
			check(get<0>(t), 2);

			t = all(i, vector<int>{4});
			check(get<0>(t), 0);

			i = MULTILINE(test x{ if (x == 5 || x == 6) { 15 } {33} }
				ahoj y{ if (if (y == 5) { 1 } {0} || if (y == 6) { 1 } {0} == 1) { 15 } {33} }
				check a{ test(a) == ahoj(a) }
			main{ check(read()) });
			t = all(i, vector<int>{5});
			check(get<0>(t), 1);

			t = all(i, vector<int>{6});
			check(get<0>(t), 1);

			t = all(i, vector<int>{4});
			check(get<0>(t), 1);

			t = all(i, vector<int>{2});
			check(get<0>(t), 1);
		}

		TEST_METHOD(functions)
		{
			string i = MULTILINE(h z{ z; }
				g y{ h(y) }
			f x{ g({write(10);x }) }
			main{ write(f(5)); });
			auto t = all(i, vector<int>());
			check(t, make_tuple(0, "10\n5\n"));

			i = MULTILINE(b a{ a }
				c a g{ b(a) }
				d a b e{ c(a,b) }
				e a b c f{ d(a,b,c) }
				g a b c d h{ e(a,b,c,d) }
				h a b c d e f{ g(a,b,c,d,e) }
				f a b c d e g i{ h(a,b,c,d,e,g) }

			main{ f(10,2,3,4,5,6,7); });
			t = all(i, vector<int>());
			check(get<0>(t), 10);

			i = MULTILINE(b a{ a }
				c a g{ b(a) }
				d a b e{ c(a,b) }
				e a b c f{ d(a,b,c) }
				g a b c d e{ e(a,b,c,e) }
				h a b c d e f{ g(a,b,c,d,e) }
				f a b c d e g i{ h(a,b,c,d,e,g) }

			main{ f(10,2,3,4,5,6,7); });
			t = all(i, vector<int>());
			check(get<0>(t), 55);
		}

		TEST_METHOD(syntax)
		{
			string i = MULTILINE(fact x{ if (x > 0) { fa{}ct(x - 1) } { 1 } }
			main{ fact(6) });

			auto t = all(i, vector<int>());
			check(get<0>(t), 55);

			i = "test x y z{ x * y % z }main{ test(5,61,2 }";
			t = all(i, vector<int>());
			check(get<0>(t), 55);

			i = "test x y z { x * y % z } main{ test5,61,2) }";
			t = all(i, vector<int>());
			check(get<0>(t), 55);

			i = MULTILINE(test x y z{ x * y % z }
			main{ test((5,61,2)) });
			t = all(i, vector<int>());
			check(get<0>(t), 55);

			i = MULTILINE(test x y z{ x  y % z }
			main{ test((5,61,2)) });
			t = all(i, vector<int>());
			check(get<0>(t), 55);

			i = MULTILINE(test x y z{ x * y % z }
			main{ test(((5)),((((61)))),(2)) });
			t = all(i, vector<int>());
			check(get<0>(t), 1);

			i = MULTILINE(test param1 param2 param3{ param1 * param2 % param3 }
			main{ test(((5)),((((61)))),(2)) });
			t = all(i, vector<int>());
			check(get<0>(t), 55);

			i = MULTILINE({ x * y % z }
			main{ 0 });
			t = all(i, vector<int>());
			check(get<0>(t), 55);

			i = MULTILINE(main(0));
			t = all(i, vector<int>());
			check(get<0>(t), 55);

		}

		TEST_METHOD(edgeCases)
		{
			string i = MULTILINE(f x y z{ x * y * z }
			main{ write(f(3 * 2 + if (1 > 0) { if (10 > 9) { 10 } {9} } {3} +16,5,5)) });

			auto t = all(i, vector<int>());
			check(t, make_tuple(0, "800\n"));


			i = MULTILINE(f x y z{ x * y * z }
			main{ write(f(3 * 2 + if (1 > 0) { if (10 > 9) { 10 }; {9} } {3} +16,5,5)) });
			t = all(i, vector<int>());
			check(get<0>(t), 55);

			i = MULTILINE(f x y z{ x * y * z }
			main{ write(f(3 * 2 + if (1 > 0) { if (10 > 9) { 10 } {9; };; } {3} +16,5,5)) });
			t = all(i, vector<int>());
			check(t, make_tuple(0, "800\n"));

			i = MULTILINE(f x y z{ x * y * z }
			main{ write(f(3 + 2 * if (1 > 0) { if (10 > 9) { 10 } {9; };; } {3} +16,5,5)) });
			t = all(i, vector<int>());
			check(t, make_tuple(0, "975\n"));

			i = MULTILINE(f x y z{ x * y * z }
			main{ write(f((3 + 2) * if (1 > 0) { if (10 > 9) { 10 } {9; };; } {3} +16,5,5)) });
			t = all(i, vector<int>());
			check(t, make_tuple(0, "1650\n"));

			i = MULTILINE(f x y z{ x * y * z }
			main{ write(f((3 + 2) * ((if (1 > 0) { 1 * if (10 > 9) { 10 } {9; };; } {3})) + 16,5,5)) });
			t = all(i, vector<int>());
			check(t, make_tuple(0, "1650\n"));

			i = MULTILINE(f x y z{ x * y * z }
			main{ write(f((3 + 2) * ((if (1 > 0) { 1 * ((if (10 > 9) { 10 } {9; } *2));; } {3})) + 16,5,5)) });
			t = all(i, vector<int>());
			check(t, make_tuple(0, "2900\n"));



			i = MULTILINE(f x y z{ x * y * z }
			main{ write(f((3 + 2) * ((if (1 > 0) { 1 * ((if (10 > 9) ({ 10 }) ({9; }) * 2));; } {3})) + 16,5,5)) });
			t = all(i, vector<int>());
			check(get<0>(t), 55);

			i = MULTILINE(f x y z{ x * y * z }
			main{ write(f((3 + 2) * ((if (1 > 0) { 1 * ((if (10 > 9) { (10 }) { 9; } *2));; } {3})) + 16,5,5)) });
			t = all(i, vector<int>());
			check(get<0>(t), 55);

			i = MULTILINE(f x y z{ x * y * z }
			main{ write(f((3 + 2) * ((if (1 > 0) { 1 * ((if (10 > 9) { 10 } > {9; } *2));; } {3})) + 16,5,5)) });
			t = all(i, vector<int>());
			check(get<0>(t), 55);

			i = MULTILINE(f x y z{ x * y * z }
			main{ write(f((3 + 2) * ((if (1 > 0) { 1 * ((if (10 > 9) ({ 10 }) ({9; } *2)));; } {3})) + 16,5,5)) });
			t = all(i, vector<int>());
			check(get<0>(t), 55);

			i = MULTILINE(f x y z{ x * y * z }

			main{ write(f(if ({write(50); 1 > 0}) { 6 } {1234},if ({write(40); 1 < 0}) { 123 } {7},if ({write(30); 10 < 9}) { 12 } {5})) });
			t = all(i, vector<int>());
			check(t, make_tuple(0, "30\n40\n50\n210\n"));


			i = MULTILINE(f x y z{ x * y * z }

			main{ write(f(if ({write(50); 1 > 0}) { 6 } {1234},if ({write(40); 1 < 0}) { 123 } {7},if ({write(30) 10 < 9}) { 12 } {5})) });
			t = all(i, vector<int>());
			check(get<0>(t), 55);

			i = MULTILINE(f x{ x*x }

				main{
					write(f(
					(1 + if (10 > 1) {
						if (50 == 50) {
							if (20 % 2 == 1)
							{
								write(111);
							}
							{
								500;
							}
						}
						{
							write(111);
						}
					}
					{
						if (10 < 1)
						{
							if (15 * 2 == 30)
							{
								write(111);
							}
							{
								write(111);
							}
						}
						{
							if (1 == 1 && 2 != 2)
							{
								write(111);
							}
							{
								write(111);
							}
						}
					}) * 2))
				});
			t = all(i, vector<int>());
			check(t, make_tuple(0, "1004004\n"));

			i = MULTILINE(f x{ x*x }

				main{
					write(f(
					1 + if (10 > 1) {
						if (50 == 50) {
							if (20 % 2 == 1)
							{
								write(111);
							}
							{
								500;
							}
						}
						{
							write(111);
						}
					}
					{
						if (10 < 1)
						{
							if (15 * 2 == 30)
							{
								write(111);
							}
							{
								write(111);
							}
						}
						{
							if (1 == 1 && 2 != 2)
							{
								write(111);
							}
							{
								write(111);
							}
						}
					} *2))
				});
			t = all(i, vector<int>());
			check(t, make_tuple(0, "1002001\n"));
		}

		TEST_METHOD(semicolons)
		{
			string i = MULTILINE(test{ {1} *({2} +{ {3}};;) }
			main{ write(test()) });
			auto t = all(i, vector<int>());
			check(get<0>(t), 55);

			i = MULTILINE(test{ {1} *({2} +{ {3}});; }
			main{ write(test()) });
			t = all(i, vector<int>());
			check(t, make_tuple(0, "5\n"));

			i = MULTILINE(test x{ x };
			main{ test(44); });
			t = all(i, vector<int>());
			check(get<0>(t), 55);


			i = MULTILINE(test x{ ; {;; {; {x;; }; ; }; }; };
			main{ test(44); });
			t = all(i, vector<int>());
			check(get<0>(t), 55);

			i = MULTILINE(test x{ ; {;; {; {x;; }; ; }; }; }
			main{ test(44); });
			t = all(i, vector<int>());
			check(get<0>(t), 44);

			i = MULTILINE(test x{ x }
			main{ test(44); });
			t = all(i, vector<int>());
			check(get<0>(t), 44);

			i = MULTILINE(test x{ x }
			main{ test(22) test(44); });
			t = all(i, vector<int>());
			check(get<0>(t), 55);


			i = MULTILINE(test x{ x }
			main{ test(22) test(44) });
			t = all(i, vector<int>());
			check(get<0>(t), 55);

			i = MULTILINE(test x{ x }
			main{ test(22); test(43) });
			t = all(i, vector<int>());
			check(get<0>(t), 43);

			i = MULTILINE(test x{ if x > 5 { 10 } { 1 } }
			main{ test(22); test(44) });
			t = all(i, vector<int>());
			check(get<0>(t), 55);

			i = MULTILINE(test x{ if (((x > 5))) { 10 } { 1 } }
			main{ test(1); test(44) });
			t = all(i, vector<int>());
			check(get<0>(t), 10);

			i = MULTILINE(test x{ if (x > 5;) { 10 } { 1 } }
			main{ test(1); test(44) });
			t = all(i, vector<int>());
			check(get<0>(t), 55);

			i = MULTILINE(test x{ if (x > 5); { 10 } { 1 } }
			main{ test(1); test(44) });
			t = all(i, vector<int>());
			check(get<0>(t), 55);

			i = MULTILINE(test x{ if (x > 5) { 10 }; { 1 } }
			main{ test(1); test(44) });
			t = all(i, vector<int>());
			check(get<0>(t), 55);

			i = MULTILINE(test x{ if {x > 5} { 10 } { 1 } }
			main{ test(1); test(44) });
			t = all(i, vector<int>());
			check(get<0>(t), 55);

			i = MULTILINE(test x{ if ({x > 5; }) { 10 } { 1 } }
			main{ test(1); test(44) });
			t = all(i, vector<int>());
			check(get<0>(t), 10);

			i = MULTILINE(fact x{ if (x > 0) { fact(x - 1) } { 1 } }
			main{ fa,ct(6) });
			t = all(i, vector<int>());
			check(get<0>(t), 55);


			i = MULTILINE(fact x{ if (x > 0) { fact(x - 1) } { 1 } }
			main{ fa; ct(6) });
			t = all(i, vector<int>());
			check(get<0>(t), 55);

		}

		TEST_METHOD(nameCollisions)
		{
			string i = MULTILINE(write x{ x }
			main{ write(15) });
			auto t = all(i, vector<int>());
			check(get<0>(t), 55);

			i = MULTILINE(read{ 15 }
			main{ read() });
			t = all(i, vector<int>());
			check(get<0>(t), 55);

			i = MULTILINE(const{ 15 }
			main{ write(const()) });
			t = all(i, vector<int>());
			check(get<0>(t), 55);

			i = MULTILINE(test x{ x }
				test{ 10 }
			main{ write(test()) });
			t = all(i, vector<int>());
			check(get<0>(t), 55);

			i = MULTILINE(test x{ x }
				ahoj test{ test(test) }
			main{ write(ahoj(read())) });
			t = all(i, vector<int>());
			check(get<0>(t), 55);

			i = MULTILINE(test x{ x }
				ahoj test{ test }
			main{ write(ahoj(read())) });
			t = all(i, vector<int>{43});
			check(t, make_tuple(0, "43\n"));
		}
	};
}