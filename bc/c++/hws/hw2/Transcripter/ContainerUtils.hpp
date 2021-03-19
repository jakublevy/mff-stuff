#ifndef _CONTAINER_UTILS_
#define _CONTAINER_UTILS_
#include <string>
#include "StringUtils.hpp"
#include <algorithm>

class ContainerUtils
{
public:
	/// <returns>
	/// true if there exists two iterators i1, i2 belonging to [start, end]
	/// such that *i1 == *i2
	/// false otherwise
	/// </returns>
	template <typename It>
	static bool containsDuplicates(It start, It end);


	/// <summary>
	/// Checks the usage of unallowed C/C++ reserved keywords (https://en.cppreference.com/w/cpp/keyword).
	/// </summary>
	template <typename It>
	static bool containsReservedKeywords(It start, It end);


	/// <summary>
	/// Assumes that iterators point to Container<string>.
	/// Deletes all occurences of white chars in each element.
	/// </summary>
	template <typename It>
	static void deleteWhiteChars(It start, It end);


	/// <summary>
	/// Assumes that iterators point to Container<string>.
	/// Assumes that *openingParenthesis == '('.
	/// Finds a matching parenthesis.
	/// </summary>
	/// <returns>
	/// Offset from the iterator start to occurence of a matching parenthesis.
	/// </returns>
	template <typename It>
	static size_t findMatchingParenthesis(It start, It end, It openingParenthesis);


	/// <summary>
	/// Deletes empty() elements of c
	/// </summary>
	template <typename Container>
	static void removeEmptyEntries(Container& c);
};

template <typename It>
bool ContainerUtils::containsDuplicates(It start, It end)
{
	for (It i = start; i != end; ++i)
	{
		for (It j = start; j != end; ++j)
		{
			if (i != j)
			{
				if (*i == *j)
				{
					return true;
				}
			}
		}
	}
	return false;
}

template <typename It>
bool ContainerUtils::containsReservedKeywords(It start, It end)
{
	while (start != end)
	{
		if (StringUtils::isReservedKeyword(*start))
		{
			return true;
		}
		++start;
	}
	return false;
}

template <typename It>
void ContainerUtils::deleteWhiteChars(It start, It end)
{
	while (start != end)
	{
		StringUtils::trim(*start);
		*start = StringUtils::deleteWhiteSpaces(*start);
		++start;
	}
}

template <typename It>
size_t ContainerUtils::findMatchingParenthesis(It start, It end, It openingParenthesis)
{
	size_t e = 1;
	for (It ite = openingParenthesis + 1; ite != end; ++ite)
	{
		if (*ite == "(")
		{
			++e;
		}
		else if (*ite == ")")
		{
			--e;
			if (e == 0)
			{
				return ite - start;
			}
		}
	}
	return 0;
}

template <typename Container>
void ContainerUtils::removeEmptyEntries(Container& c)
{
	auto it = std::remove_if(std::begin(c), std::end(c), [](auto&& val) { return val.empty(); });
	c.erase(it, std::cend(c));
}
#endif
