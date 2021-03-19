/*

DU12SEM.CPP

JY

Mlaskal's semantic interface for DU1-2

*/

// CHANGE THIS LINE TO #include "du3456sem.hpp" WHEN THIS FILE IS COPIED TO du3456sem.cpp
#include "du3456sem.hpp"
#include "duerr.hpp"
#include <regex>

namespace mlc {

	//LexUtils
	using std::string;
	using std::tuple;
	using std::reverse;
	using std::make_tuple;
	using std::to_string;
	using std::regex;
	using std::smatch;

	string LexUtils::toUpperCase(const char *text)
	{
		string ret(text);
		transform(ret.begin(), ret.end(), ret.begin(), toupper);
		return ret;
	}

	tuple<int, bool> LexUtils::uint_parse(string const& num)
	{
		string bin = toBinary(num);
		bool ok = true;
		if(bin.size() > 31)
		{
			bin = bin.erase(0, bin.size() - 31);
			ok = false;
		}
		return make_tuple(stoi(bin, nullptr, 2), ok);
	}

	tuple<string, string> LexUtils::split_uint_identifier(const char* text, size_t len)
	{
		size_t i = 0;
		while(isdigit(text[i]))
			++i;
		
		string out_int(text, i);
		string out_id;
		for (size_t j = i; j < len; ++j)
			out_id += text[j];
		
		return make_tuple(out_int, out_id);
	}

	tuple<string, string> LexUtils::split_real_identifier(const char* text, size_t len)
	{
		string s(text);
		regex pattern(R"(([0-9]+\.[0-9]*[eE][\+\-]?[0-9]+|[0-9]+[eE][\+\-]?[0-9]+|[0-9]+\.[0-9]*)([a-zA-Z][a-zA-Z0-9]*))");
		smatch matches;
		regex_search(s, matches, pattern);
		return make_tuple(matches[1].str(), matches[2].str());
	}

	string LexUtils::toBinary(string const& num)
	{
		string c = num;
		string ret;
		while (c != "0")
		{
			auto [q, r] = longDivision(c, 2);
			ret += r;
			c = q;
		}
		reverse(ret.begin(), ret.end());
		return ret;
	}
	tuple<string, string> LexUtils::longDivision(string const& a, int q)
	{
		string z = "0";
		string b;
		string out;

		size_t j;
		for (j = 0; j < a.size(); ++j)
		{
			if (stoi(z + b) < q)
				b += a[j];
			else break;
		}

		for (size_t i = j; i < a.size(); ++i)
		{
			out += to_string(stoi(z + b) / q);
			z = to_string(stoi(z + b) % q);
			b = "";

			b += a[i];
		}
		out += to_string(stoi(z + b) / q);
		z = to_string(stoi(z + b) % q);
		return make_tuple(out, z);
	}






	//SemUtils
	using std::deque;
	using std::get;
	using std::get_if;
	using std::variant;
	using std::vector;
	
	void SemUtils::addNewLabels(MlaskalCtx const *ctx, deque<ls_int_index> const &valIdxs)
	{
		for(auto &v : valIdxs) 
			ctx->tab->add_label_entry(ctx->curline, v, ctx->tab->new_label());
		
	}

	void SemUtils::addNewVariables(MlaskalCtx const* ctx, deque<ls_id_index> const &vars, ls_id_index type)
	{
		auto [_, ts] = existingType(ctx, type);
			for (auto& var : vars)
				ctx->tab->add_var(ctx->curline, var, ts);
	}

	void SemUtils::addNewNamedConstant(MlaskalCtx const* ctx, ls_id_index id,
		variant<ls_int_index, ls_real_index, ls_str_index, tuple<DUTOKGE_OPER_SIGNADD, ls_int_index>, tuple<DUTOKGE_OPER_SIGNADD, ls_real_index>, std::variant<bool, ls_id_index>> val)
	{
		if(auto v = get_if<0>(&val))
			ctx->tab->add_const_int(ctx->curline, id, *v);
		
		else if(auto v = get_if<1>(&val))
			ctx->tab->add_const_real(ctx->curline, id, *v);
		
		else if (auto v = get_if<2>(&val))
			ctx->tab->add_const_str(ctx->curline, id, *v);
		
		else if (auto v = get_if<3>(&val))
		{
			DUTOKGE_OPER_SIGNADD unary = get<0>(*v);
			ls_int_index cVal = get<1>(*v);
			if(unary == DUTOKGE_OPER_SIGNADD::DUTOKGE_MINUS) {
				auto fixedV = ctx->tab->ls_int().add(-*cVal);
				ctx->tab->add_const_int(ctx->curline, id, fixedV);
			}
			else 
				ctx->tab->add_const_int(ctx->curline, id, cVal);
			
		}
		else if (auto v = get_if<4>(&val))
		{
			DUTOKGE_OPER_SIGNADD unary = get<0>(*v);
			ls_real_index cVal = get<1>(*v);
			if (unary == DUTOKGE_OPER_SIGNADD::DUTOKGE_MINUS) {
				auto fixedV = ctx->tab->ls_real().add(-*cVal);
				ctx->tab->add_const_real(ctx->curline, id, fixedV);
			}
			else
				ctx->tab->add_const_real(ctx->curline, id, cVal);
		}
		else if (auto v = get_if<5>(&val)) {

			if(auto vv = get_if<0>(v))
			{
				ctx->tab->add_const_bool(ctx->curline, id, *vv);
			}
			else if(auto vv = get_if<1>(v))
			{
				const_symbol_reference sym = ctx->tab->find_symbol(*vv)->access_const();
				if (!(!sym->access_bool_const()))
				{
					addNewNamedConstant(ctx, id, sym->access_bool_const()->bool_value());
				}
				if (!(!sym->access_int_const()))
				{
					addNewNamedConstant(ctx, id, sym->access_int_const()->int_value());
				}
				if (!(!sym->access_real_const()))
				{
					addNewNamedConstant(ctx, id, sym->access_real_const()->real_value());
				}
				if (!(!sym->access_str_const()))
				{
					addNewNamedConstant(ctx, id, sym->access_str_const()->str_value());
				}
			}
			
		}
		
	}

	void SemUtils::addNewProcedure(MlaskalCtx const *ctx, ls_id_index procId,
		vector<tuple<bool, deque<ls_id_index>, ls_id_index>> args)
	{
		parameter_list_ptr pl = create_parameter_list();
		for(auto& argsOfType : args)
		{
			bool byValue = get<0>(argsOfType);
			deque<ls_id_index> params = get<1>(argsOfType);
			ls_id_index paramsType = get<2>(argsOfType);

			auto [_, validType] = existingType(ctx, paramsType);
			for (auto& p : params)
			{
				if (byValue)
					pl->append_parameter_by_value(p, validType);

				else //as reference
					pl->append_parameter_by_reference(p, validType);
			}

		}
		ctx->tab->add_proc(ctx->curline, procId, pl);
		openScope(ctx, procId);
	}

	void SemUtils::addNewFunction(MlaskalCtx const* ctx, ls_id_index funcId,
		std::vector<std::tuple<bool, std::deque<ls_id_index>, ls_id_index>> args, ls_id_index retType)
	{
		parameter_list_ptr pl = create_parameter_list();
		auto [b, validRetType] = existingType(ctx, retType);

		if (b) {
			if (!canBeReturnedFromFunc(validRetType))
				message(DUERR_NOTSCALAR, ctx->curline, *retType);
		}

		for (auto& argsOfType : args)
		{
			bool byValue = get<0>(argsOfType);
			deque<ls_id_index> params = get<1>(argsOfType);
			ls_id_index paramsType = get<2>(argsOfType);

			auto [_, validType] = existingType(ctx, paramsType);

			for (auto& p : params)
			{
				if (byValue)
					pl->append_parameter_by_value(p, validType);

				else //as reference
					pl->append_parameter_by_reference(p, validType);
			}


		}
		ctx->tab->add_fnc(ctx->curline, funcId, validRetType, pl);
		openScope(ctx, funcId);
	}

	void SemUtils::addNewType(MlaskalCtx const* ctx, ls_id_index synonym,
		variant<vector<tuple<deque<ls_id_index>, ls_id_index>>, ls_id_index> type)
	{
		if (auto v = get_if<0>(&type)) //record case
		{
			field_list_ptr fields = create_field_list();
			
			for(auto& varsOfOneType : *v)
			{
				deque<ls_id_index> vars = get<0>(varsOfOneType);
				ls_id_index varsType = get<1>(varsOfOneType);
				auto [_, t] = existingType(ctx, varsType);

				for (auto& var : vars)
					fields->append_field(var, t);


			}
			type_pointer rec = ctx->tab->create_record_type(fields, ctx->curline);
			ctx->tab->add_type(ctx->curline, synonym, rec);
		}
		else if (auto v = get_if<1>(&type)) //simple identifier case
		{
			auto [_, validType] = existingType(ctx, *v);
			ctx->tab->add_type(ctx->curline, synonym, validType);
		}
	}

	std::variant<bool, ls_id_index> SemUtils::fetchValue(MlaskalCtx const* ctx, ls_id_index b)
	{
		const_symbol_reference sym = ctx->tab->find_symbol(b)->access_const();
		if(! (!sym->access_bool_const()) )
		{
			return sym->access_bool_const()->bool_value();
		}
		if (!(!sym->access_int_const()))
		{
			return b;
		}
		if (!(!sym->access_real_const()))
		{
			return b;
		}
		if (!(!sym->access_str_const()))
		{
			return b;
		}
		
		string val = *b;
		if(val == "TRUE")
		{
			return true;
		}
		if(val == "FALSE")
		{
			return false;
		}
		message(DUERR_CANNOTCONVERT, ctx->curline, *b);
		
		return false;
	}

	bool SemUtils::canBeReturnedFromFunc(type_pointer const ptr)
	{
		auto c = ptr->cat();
		return c == TCAT_INT || c == TCAT_REAL || c == TCAT_STR || c == TCAT_BOOL || c == TCAT_RANGE;
	}

	void SemUtils::openScope(MlaskalCtx const* ctx, ls_id_index id)
	{
		ctx->tab->enter(ctx->curline, id);
	}

	std::tuple<bool, type_pointer> SemUtils::existingType(MlaskalCtx const* ctx, ls_id_index id)
	{
		type_pointer type = ctx->tab->find_symbol(id)->access_type()->type();
		if(!type)
		{
			message(DUERR_NOTTYPE, ctx->curline, *id);
			return std::make_tuple(false, type_pointer());
		}
		
		return std::make_tuple(true, type);
	}
};

/*****************************************/
