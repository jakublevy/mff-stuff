/*

	DU12SEM.H

	DB

	Mlaskal's semantic interface for DU1-2

*/

#ifndef __DU12SEM_H
#define __DU12SEM_H

#include <string>
#include "literal_storage.hpp"
#include "flat_icblock.hpp"
#include "dutables.hpp"
#include "abstract_instr.hpp"
#include "gen_ainstr.hpp"
#include <tuple>

#include <deque>
#include <variant>

namespace mlc {

	class LexUtils
	{
	public:
		static std::string toUpperCase(const char *text);
		static std::tuple<int, bool> uint_parse(std::string const &num);
		static std::tuple<std::string, std::string> split_uint_identifier(const char *text, size_t len);
		static std::tuple<std::string, std::string> split_real_identifier(const char *text, size_t len);
	private:
		static std::string toBinary(std::string const &num);
		static std::tuple<std::string, std::string> longDivision(std::string const& a, int q);
	};

	class SemUtils
	{
	public:
		static void addNewLabels(MlaskalCtx const *ctx, std::deque<ls_int_index> const& valIdxs);
		static void addNewVariables(MlaskalCtx const* ctx, std::deque<ls_id_index> const& vars, ls_id_index type);
		static void addNewNamedConstant(MlaskalCtx const* ctx, ls_id_index id, std::variant<ls_int_index
			, ls_real_index
			, ls_str_index
			, std::tuple<DUTOKGE_OPER_SIGNADD, ls_int_index>
			, std::tuple<DUTOKGE_OPER_SIGNADD, ls_real_index>
			, std::variant<bool, ls_id_index>
		> val
		);
		static void addNewProcedure(MlaskalCtx const * ctx, ls_id_index procId, std::vector< std::tuple< bool, std::deque<ls_id_index>, ls_id_index > > args);
		static void addNewFunction(MlaskalCtx const* ctx, ls_id_index funcId, std::vector< std::tuple< bool, std::deque<ls_id_index>, ls_id_index > > args, ls_id_index retType);

		static void addNewType(MlaskalCtx const* ctx, ls_id_index synonym, std::variant< std::vector<std::tuple< std::deque<mlc::ls_id_index>, mlc::ls_id_index > >
			, mlc::ls_id_index
		> type);

		static std::variant<bool, ls_id_index> fetchValue(MlaskalCtx const* ctx, ls_id_index b);

	private:
		static bool canBeReturnedFromFunc(type_pointer const ptr);
		static void openScope(MlaskalCtx const* ctx, ls_id_index id);
		static std::tuple<bool, type_pointer> existingType(MlaskalCtx const* ctx, ls_id_index id);
	};
}

#endif
