// A Bison parser, made by GNU Bison 3.4.2.

// Skeleton implementation for Bison LALR(1) parsers in C++

// Copyright (C) 2002-2015, 2018-2019 Free Software Foundation, Inc.

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

// As a special exception, you may create a larger work that contains
// part or all of the Bison parser skeleton and distribute that work
// under terms of your choice, so long as that work isn't itself a
// parser generator using the skeleton or a modified version thereof
// as a parser skeleton.  Alternatively, if you modify or redistribute
// the parser skeleton itself, you may (at your option) remove this
// special exception, which will cause the skeleton and the resulting
// Bison output files to be licensed under the GNU General Public
// License without this special exception.

// This special exception was added by the Free Software Foundation in
// version 2.2 of Bison.

// Undocumented macros, especially those whose name start with YY_,
// are private implementation details.  Do not rely on them.





#include "du3456g.tab.hh"


// Unqualified %code blocks.
#line 37 "../private-src/du3456g.y"

	// this code is emitted to du3456g.cpp

	// declare yylex here 
	#include "bisonflex.hpp"
	YY_DECL;

	// allow access to context 
	#include "dutables.hpp"

	// other user-required contents
	#include <assert.h>
	#include <stdlib.h>

    /* local stuff */
    using namespace mlc;


#line 64 "du3456g.tab.cc"


#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> // FIXME: INFRINGES ON USER NAME SPACE.
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

// Whether we are compiled with exception support.
#ifndef YY_EXCEPTIONS
# if defined __GNUC__ && !defined __EXCEPTIONS
#  define YY_EXCEPTIONS 0
# else
#  define YY_EXCEPTIONS 1
# endif
#endif

#define YYRHSLOC(Rhs, K) ((Rhs)[K].location)
/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

# ifndef YYLLOC_DEFAULT
#  define YYLLOC_DEFAULT(Current, Rhs, N)                               \
    do                                                                  \
      if (N)                                                            \
        {                                                               \
          (Current).begin  = YYRHSLOC (Rhs, 1).begin;                   \
          (Current).end    = YYRHSLOC (Rhs, N).end;                     \
        }                                                               \
      else                                                              \
        {                                                               \
          (Current).begin = (Current).end = YYRHSLOC (Rhs, 0).end;      \
        }                                                               \
    while (false)
# endif


// Enable debugging if requested.
#if YYDEBUG

// A pseudo ostream that takes yydebug_ into account.
# define YYCDEBUG if (yydebug_) (*yycdebug_)

# define YY_SYMBOL_PRINT(Title, Symbol)         \
  do {                                          \
    if (yydebug_)                               \
    {                                           \
      *yycdebug_ << Title << ' ';               \
      yy_print_ (*yycdebug_, Symbol);           \
      *yycdebug_ << '\n';                       \
    }                                           \
  } while (false)

# define YY_REDUCE_PRINT(Rule)          \
  do {                                  \
    if (yydebug_)                       \
      yy_reduce_print_ (Rule);          \
  } while (false)

# define YY_STACK_PRINT()               \
  do {                                  \
    if (yydebug_)                       \
      yystack_print_ ();                \
  } while (false)

#else // !YYDEBUG

# define YYCDEBUG if (false) std::cerr
# define YY_SYMBOL_PRINT(Title, Symbol)  YYUSE (Symbol)
# define YY_REDUCE_PRINT(Rule)           static_cast<void> (0)
# define YY_STACK_PRINT()                static_cast<void> (0)

#endif // !YYDEBUG

#define yyerrok         (yyerrstatus_ = 0)
#define yyclearin       (yyla.clear ())

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab
#define YYRECOVERING()  (!!yyerrstatus_)

namespace yy {
#line 155 "du3456g.tab.cc"


  /* Return YYSTR after stripping away unnecessary quotes and
     backslashes, so that it's suitable for yyerror.  The heuristic is
     that double-quoting is unnecessary unless the string contains an
     apostrophe, a comma, or backslash (other than backslash-backslash).
     YYSTR is taken from yytname.  */
  std::string
   mlaskal_parser ::yytnamerr_ (const char *yystr)
  {
    if (*yystr == '"')
      {
        std::string yyr;
        char const *yyp = yystr;

        for (;;)
          switch (*++yyp)
            {
            case '\'':
            case ',':
              goto do_not_strip_quotes;

            case '\\':
              if (*++yyp != '\\')
                goto do_not_strip_quotes;
              else
                goto append;

            append:
            default:
              yyr += *yyp;
              break;

            case '"':
              return yyr;
            }
      do_not_strip_quotes: ;
      }

    return yystr;
  }


  /// Build a parser object.
   mlaskal_parser :: mlaskal_parser  (mlc::yyscan_t2 yyscanner_yyarg, mlc::MlaskalCtx* ctx_yyarg)
    :
#if YYDEBUG
      yydebug_ (false),
      yycdebug_ (&std::cerr),
#endif
      yyscanner (yyscanner_yyarg),
      ctx (ctx_yyarg)
  {}

   mlaskal_parser ::~ mlaskal_parser  ()
  {}

   mlaskal_parser ::syntax_error::~syntax_error () YY_NOEXCEPT YY_NOTHROW
  {}

  /*---------------.
  | Symbol types.  |
  `---------------*/



  // by_state.
   mlaskal_parser ::by_state::by_state () YY_NOEXCEPT
    : state (empty_state)
  {}

   mlaskal_parser ::by_state::by_state (const by_state& that) YY_NOEXCEPT
    : state (that.state)
  {}

  void
   mlaskal_parser ::by_state::clear () YY_NOEXCEPT
  {
    state = empty_state;
  }

  void
   mlaskal_parser ::by_state::move (by_state& that)
  {
    state = that.state;
    that.clear ();
  }

   mlaskal_parser ::by_state::by_state (state_type s) YY_NOEXCEPT
    : state (s)
  {}

   mlaskal_parser ::symbol_number_type
   mlaskal_parser ::by_state::type_get () const YY_NOEXCEPT
  {
    if (state == empty_state)
      return empty_symbol;
    else
      return yystos_[state];
  }

   mlaskal_parser ::stack_symbol_type::stack_symbol_type ()
  {}

   mlaskal_parser ::stack_symbol_type::stack_symbol_type (YY_RVREF (stack_symbol_type) that)
    : super_type (YY_MOVE (that.state), YY_MOVE (that.location))
  {
    switch (that.type_get ())
    {
      case 44: // FOR_DIRECTION
        value.YY_MOVE_OR_COPY< mlc::DUTOKGE_FOR_DIRECTION > (YY_MOVE (that.value));
        break;

      case 43: // OPER_MUL
        value.YY_MOVE_OR_COPY< mlc::DUTOKGE_OPER_MUL > (YY_MOVE (that.value));
        break;

      case 41: // OPER_REL
        value.YY_MOVE_OR_COPY< mlc::DUTOKGE_OPER_REL > (YY_MOVE (that.value));
        break;

      case 42: // OPER_SIGNADD
        value.YY_MOVE_OR_COPY< mlc::DUTOKGE_OPER_SIGNADD > (YY_MOVE (that.value));
        break;

      case 26: // IDENTIFIER
        value.YY_MOVE_OR_COPY< mlc::ls_id_index > (YY_MOVE (that.value));
        break;

      case 27: // UINT
        value.YY_MOVE_OR_COPY< mlc::ls_int_index > (YY_MOVE (that.value));
        break;

      case 28: // REAL
        value.YY_MOVE_OR_COPY< mlc::ls_real_index > (YY_MOVE (that.value));
        break;

      case 29: // STRING
        value.YY_MOVE_OR_COPY< mlc::ls_str_index > (YY_MOVE (that.value));
        break;

      default:
        break;
    }

#if 201103L <= YY_CPLUSPLUS
    // that is emptied.
    that.state = empty_state;
#endif
  }

   mlaskal_parser ::stack_symbol_type::stack_symbol_type (state_type s, YY_MOVE_REF (symbol_type) that)
    : super_type (s, YY_MOVE (that.location))
  {
    switch (that.type_get ())
    {
      case 44: // FOR_DIRECTION
        value.move< mlc::DUTOKGE_FOR_DIRECTION > (YY_MOVE (that.value));
        break;

      case 43: // OPER_MUL
        value.move< mlc::DUTOKGE_OPER_MUL > (YY_MOVE (that.value));
        break;

      case 41: // OPER_REL
        value.move< mlc::DUTOKGE_OPER_REL > (YY_MOVE (that.value));
        break;

      case 42: // OPER_SIGNADD
        value.move< mlc::DUTOKGE_OPER_SIGNADD > (YY_MOVE (that.value));
        break;

      case 26: // IDENTIFIER
        value.move< mlc::ls_id_index > (YY_MOVE (that.value));
        break;

      case 27: // UINT
        value.move< mlc::ls_int_index > (YY_MOVE (that.value));
        break;

      case 28: // REAL
        value.move< mlc::ls_real_index > (YY_MOVE (that.value));
        break;

      case 29: // STRING
        value.move< mlc::ls_str_index > (YY_MOVE (that.value));
        break;

      default:
        break;
    }

    // that is emptied.
    that.type = empty_symbol;
  }

#if YY_CPLUSPLUS < 201103L
   mlaskal_parser ::stack_symbol_type&
   mlaskal_parser ::stack_symbol_type::operator= (stack_symbol_type& that)
  {
    state = that.state;
    switch (that.type_get ())
    {
      case 44: // FOR_DIRECTION
        value.move< mlc::DUTOKGE_FOR_DIRECTION > (that.value);
        break;

      case 43: // OPER_MUL
        value.move< mlc::DUTOKGE_OPER_MUL > (that.value);
        break;

      case 41: // OPER_REL
        value.move< mlc::DUTOKGE_OPER_REL > (that.value);
        break;

      case 42: // OPER_SIGNADD
        value.move< mlc::DUTOKGE_OPER_SIGNADD > (that.value);
        break;

      case 26: // IDENTIFIER
        value.move< mlc::ls_id_index > (that.value);
        break;

      case 27: // UINT
        value.move< mlc::ls_int_index > (that.value);
        break;

      case 28: // REAL
        value.move< mlc::ls_real_index > (that.value);
        break;

      case 29: // STRING
        value.move< mlc::ls_str_index > (that.value);
        break;

      default:
        break;
    }

    location = that.location;
    // that is emptied.
    that.state = empty_state;
    return *this;
  }
#endif

  template <typename Base>
  void
   mlaskal_parser ::yy_destroy_ (const char* yymsg, basic_symbol<Base>& yysym) const
  {
    if (yymsg)
      YY_SYMBOL_PRINT (yymsg, yysym);
  }

#if YYDEBUG
  template <typename Base>
  void
   mlaskal_parser ::yy_print_ (std::ostream& yyo,
                                     const basic_symbol<Base>& yysym) const
  {
    std::ostream& yyoutput = yyo;
    YYUSE (yyoutput);
    symbol_number_type yytype = yysym.type_get ();
#if defined __GNUC__ && ! defined __clang__ && ! defined __ICC && __GNUC__ * 100 + __GNUC_MINOR__ <= 408
    // Avoid a (spurious) G++ 4.8 warning about "array subscript is
    // below array bounds".
    if (yysym.empty ())
      std::abort ();
#endif
    yyo << (yytype < yyntokens_ ? "token" : "nterm")
        << ' ' << yytname_[yytype] << " ("
        << yysym.location << ": ";
    YYUSE (yytype);
    yyo << ')';
  }
#endif

  void
   mlaskal_parser ::yypush_ (const char* m, YY_MOVE_REF (stack_symbol_type) sym)
  {
    if (m)
      YY_SYMBOL_PRINT (m, sym);
    yystack_.push (YY_MOVE (sym));
  }

  void
   mlaskal_parser ::yypush_ (const char* m, state_type s, YY_MOVE_REF (symbol_type) sym)
  {
#if 201103L <= YY_CPLUSPLUS
    yypush_ (m, stack_symbol_type (s, std::move (sym)));
#else
    stack_symbol_type ss (s, sym);
    yypush_ (m, ss);
#endif
  }

  void
   mlaskal_parser ::yypop_ (int n)
  {
    yystack_.pop (n);
  }

#if YYDEBUG
  std::ostream&
   mlaskal_parser ::debug_stream () const
  {
    return *yycdebug_;
  }

  void
   mlaskal_parser ::set_debug_stream (std::ostream& o)
  {
    yycdebug_ = &o;
  }


   mlaskal_parser ::debug_level_type
   mlaskal_parser ::debug_level () const
  {
    return yydebug_;
  }

  void
   mlaskal_parser ::set_debug_level (debug_level_type l)
  {
    yydebug_ = l;
  }
#endif // YYDEBUG

   mlaskal_parser ::state_type
   mlaskal_parser ::yy_lr_goto_state_ (state_type yystate, int yysym)
  {
    int yyr = yypgoto_[yysym - yyntokens_] + yystate;
    if (0 <= yyr && yyr <= yylast_ && yycheck_[yyr] == yystate)
      return yytable_[yyr];
    else
      return yydefgoto_[yysym - yyntokens_];
  }

  bool
   mlaskal_parser ::yy_pact_value_is_default_ (int yyvalue)
  {
    return yyvalue == yypact_ninf_;
  }

  bool
   mlaskal_parser ::yy_table_value_is_error_ (int yyvalue)
  {
    return yyvalue == yytable_ninf_;
  }

  int
   mlaskal_parser ::operator() ()
  {
    return parse ();
  }

  int
   mlaskal_parser ::parse ()
  {
    // State.
    int yyn;
    /// Length of the RHS of the rule being reduced.
    int yylen = 0;

    // Error handling.
    int yynerrs_ = 0;
    int yyerrstatus_ = 0;

    /// The lookahead symbol.
    symbol_type yyla;

    /// The locations where the error started and ended.
    stack_symbol_type yyerror_range[3];

    /// The return value of parse ().
    int yyresult;

#if YY_EXCEPTIONS
    try
#endif // YY_EXCEPTIONS
      {
    YYCDEBUG << "Starting parse\n";


    /* Initialize the stack.  The initial state will be set in
       yynewstate, since the latter expects the semantical and the
       location values to have been already stored, initialize these
       stacks with a primary value.  */
    yystack_.clear ();
    yypush_ (YY_NULLPTR, 0, YY_MOVE (yyla));

  /*-----------------------------------------------.
  | yynewstate -- push a new symbol on the stack.  |
  `-----------------------------------------------*/
  yynewstate:
    YYCDEBUG << "Entering state " << yystack_[0].state << '\n';

    // Accept?
    if (yystack_[0].state == yyfinal_)
      YYACCEPT;

    goto yybackup;


  /*-----------.
  | yybackup.  |
  `-----------*/
  yybackup:
    // Try to take a decision without lookahead.
    yyn = yypact_[yystack_[0].state];
    if (yy_pact_value_is_default_ (yyn))
      goto yydefault;

    // Read a lookahead token.
    if (yyla.empty ())
      {
        YYCDEBUG << "Reading a token: ";
#if YY_EXCEPTIONS
        try
#endif // YY_EXCEPTIONS
          {
            symbol_type yylookahead (yylex (yyscanner, ctx));
            yyla.move (yylookahead);
          }
#if YY_EXCEPTIONS
        catch (const syntax_error& yyexc)
          {
            YYCDEBUG << "Caught exception: " << yyexc.what() << '\n';
            error (yyexc);
            goto yyerrlab1;
          }
#endif // YY_EXCEPTIONS
      }
    YY_SYMBOL_PRINT ("Next token is", yyla);

    /* If the proper action on seeing token YYLA.TYPE is to reduce or
       to detect an error, take that action.  */
    yyn += yyla.type_get ();
    if (yyn < 0 || yylast_ < yyn || yycheck_[yyn] != yyla.type_get ())
      goto yydefault;

    // Reduce or error.
    yyn = yytable_[yyn];
    if (yyn <= 0)
      {
        if (yy_table_value_is_error_ (yyn))
          goto yyerrlab;
        yyn = -yyn;
        goto yyreduce;
      }

    // Count tokens shifted since error; after three, turn off error status.
    if (yyerrstatus_)
      --yyerrstatus_;

    // Shift the lookahead token.
    yypush_ ("Shifting", yyn, YY_MOVE (yyla));
    goto yynewstate;


  /*-----------------------------------------------------------.
  | yydefault -- do the default action for the current state.  |
  `-----------------------------------------------------------*/
  yydefault:
    yyn = yydefact_[yystack_[0].state];
    if (yyn == 0)
      goto yyerrlab;
    goto yyreduce;


  /*-----------------------------.
  | yyreduce -- do a reduction.  |
  `-----------------------------*/
  yyreduce:
    yylen = yyr2_[yyn];
    {
      stack_symbol_type yylhs;
      yylhs.state = yy_lr_goto_state_ (yystack_[yylen].state, yyr1_[yyn]);
      /* Variants are always initialized to an empty instance of the
         correct type. The default '$$ = $1' action is NOT applied
         when using variants.  */
      switch (yyr1_[yyn])
    {
      case 44: // FOR_DIRECTION
        yylhs.value.emplace< mlc::DUTOKGE_FOR_DIRECTION > ();
        break;

      case 43: // OPER_MUL
        yylhs.value.emplace< mlc::DUTOKGE_OPER_MUL > ();
        break;

      case 41: // OPER_REL
        yylhs.value.emplace< mlc::DUTOKGE_OPER_REL > ();
        break;

      case 42: // OPER_SIGNADD
        yylhs.value.emplace< mlc::DUTOKGE_OPER_SIGNADD > ();
        break;

      case 26: // IDENTIFIER
        yylhs.value.emplace< mlc::ls_id_index > ();
        break;

      case 27: // UINT
        yylhs.value.emplace< mlc::ls_int_index > ();
        break;

      case 28: // REAL
        yylhs.value.emplace< mlc::ls_real_index > ();
        break;

      case 29: // STRING
        yylhs.value.emplace< mlc::ls_str_index > ();
        break;

      default:
        break;
    }


      // Default location.
      {
        stack_type::slice range (yystack_, yylen);
        YYLLOC_DEFAULT (yylhs.location, range, yylen);
        yyerror_range[1].location = yylhs.location;
      }

      // Perform the reduction.
      YY_REDUCE_PRINT (yyn);
#if YY_EXCEPTIONS
      try
#endif // YY_EXCEPTIONS
        {
          switch (yyn)
            {

#line 693 "du3456g.tab.cc"

            default:
              break;
            }
        }
#if YY_EXCEPTIONS
      catch (const syntax_error& yyexc)
        {
          YYCDEBUG << "Caught exception: " << yyexc.what() << '\n';
          error (yyexc);
          YYERROR;
        }
#endif // YY_EXCEPTIONS
      YY_SYMBOL_PRINT ("-> $$ =", yylhs);
      yypop_ (yylen);
      yylen = 0;
      YY_STACK_PRINT ();

      // Shift the result of the reduction.
      yypush_ (YY_NULLPTR, YY_MOVE (yylhs));
    }
    goto yynewstate;


  /*--------------------------------------.
  | yyerrlab -- here on detecting error.  |
  `--------------------------------------*/
  yyerrlab:
    // If not already recovering from an error, report this error.
    if (!yyerrstatus_)
      {
        ++yynerrs_;
        error (yyla.location, yysyntax_error_ (yystack_[0].state, yyla));
      }


    yyerror_range[1].location = yyla.location;
    if (yyerrstatus_ == 3)
      {
        /* If just tried and failed to reuse lookahead token after an
           error, discard it.  */

        // Return failure if at end of input.
        if (yyla.type_get () == yyeof_)
          YYABORT;
        else if (!yyla.empty ())
          {
            yy_destroy_ ("Error: discarding", yyla);
            yyla.clear ();
          }
      }

    // Else will try to reuse lookahead token after shifting the error token.
    goto yyerrlab1;


  /*---------------------------------------------------.
  | yyerrorlab -- error raised explicitly by YYERROR.  |
  `---------------------------------------------------*/
  yyerrorlab:
    /* Pacify compilers when the user code never invokes YYERROR and
       the label yyerrorlab therefore never appears in user code.  */
    if (false)
      YYERROR;

    /* Do not reclaim the symbols of the rule whose action triggered
       this YYERROR.  */
    yypop_ (yylen);
    yylen = 0;
    goto yyerrlab1;


  /*-------------------------------------------------------------.
  | yyerrlab1 -- common code for both syntax error and YYERROR.  |
  `-------------------------------------------------------------*/
  yyerrlab1:
    yyerrstatus_ = 3;   // Each real token shifted decrements this.
    {
      stack_symbol_type error_token;
      for (;;)
        {
          yyn = yypact_[yystack_[0].state];
          if (!yy_pact_value_is_default_ (yyn))
            {
              yyn += yyterror_;
              if (0 <= yyn && yyn <= yylast_ && yycheck_[yyn] == yyterror_)
                {
                  yyn = yytable_[yyn];
                  if (0 < yyn)
                    break;
                }
            }

          // Pop the current state because it cannot handle the error token.
          if (yystack_.size () == 1)
            YYABORT;

          yyerror_range[1].location = yystack_[0].location;
          yy_destroy_ ("Error: popping", yystack_[0]);
          yypop_ ();
          YY_STACK_PRINT ();
        }

      yyerror_range[2].location = yyla.location;
      YYLLOC_DEFAULT (error_token.location, yyerror_range, 2);

      // Shift the error token.
      error_token.state = yyn;
      yypush_ ("Shifting", YY_MOVE (error_token));
    }
    goto yynewstate;


  /*-------------------------------------.
  | yyacceptlab -- YYACCEPT comes here.  |
  `-------------------------------------*/
  yyacceptlab:
    yyresult = 0;
    goto yyreturn;


  /*-----------------------------------.
  | yyabortlab -- YYABORT comes here.  |
  `-----------------------------------*/
  yyabortlab:
    yyresult = 1;
    goto yyreturn;


  /*-----------------------------------------------------.
  | yyreturn -- parsing is finished, return the result.  |
  `-----------------------------------------------------*/
  yyreturn:
    if (!yyla.empty ())
      yy_destroy_ ("Cleanup: discarding lookahead", yyla);

    /* Do not reclaim the symbols of the rule whose action triggered
       this YYABORT or YYACCEPT.  */
    yypop_ (yylen);
    while (1 < yystack_.size ())
      {
        yy_destroy_ ("Cleanup: popping", yystack_[0]);
        yypop_ ();
      }

    return yyresult;
  }
#if YY_EXCEPTIONS
    catch (...)
      {
        YYCDEBUG << "Exception caught: cleaning lookahead and stack\n";
        // Do not try to display the values of the reclaimed symbols,
        // as their printers might throw an exception.
        if (!yyla.empty ())
          yy_destroy_ (YY_NULLPTR, yyla);

        while (1 < yystack_.size ())
          {
            yy_destroy_ (YY_NULLPTR, yystack_[0]);
            yypop_ ();
          }
        throw;
      }
#endif // YY_EXCEPTIONS
  }

  void
   mlaskal_parser ::error (const syntax_error& yyexc)
  {
    error (yyexc.location, yyexc.what ());
  }

  // Generate an error message.
  std::string
   mlaskal_parser ::yysyntax_error_ (state_type yystate, const symbol_type& yyla) const
  {
    // Number of reported tokens (one for the "unexpected", one per
    // "expected").
    size_t yycount = 0;
    // Its maximum.
    enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
    // Arguments of yyformat.
    char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];

    /* There are many possibilities here to consider:
       - If this state is a consistent state with a default action, then
         the only way this function was invoked is if the default action
         is an error action.  In that case, don't check for expected
         tokens because there are none.
       - The only way there can be no lookahead present (in yyla) is
         if this state is a consistent state with a default action.
         Thus, detecting the absence of a lookahead is sufficient to
         determine that there is no unexpected or expected token to
         report.  In that case, just report a simple "syntax error".
       - Don't assume there isn't a lookahead just because this state is
         a consistent state with a default action.  There might have
         been a previous inconsistent state, consistent state with a
         non-default action, or user semantic action that manipulated
         yyla.  (However, yyla is currently not documented for users.)
       - Of course, the expected token list depends on states to have
         correct lookahead information, and it depends on the parser not
         to perform extra reductions after fetching a lookahead from the
         scanner and before detecting a syntax error.  Thus, state
         merging (from LALR or IELR) and default reductions corrupt the
         expected token list.  However, the list is correct for
         canonical LR with one exception: it will still contain any
         token that will not be accepted due to an error action in a
         later state.
    */
    if (!yyla.empty ())
      {
        int yytoken = yyla.type_get ();
        yyarg[yycount++] = yytname_[yytoken];
        int yyn = yypact_[yystate];
        if (!yy_pact_value_is_default_ (yyn))
          {
            /* Start YYX at -YYN if negative to avoid negative indexes in
               YYCHECK.  In other words, skip the first -YYN actions for
               this state because they are default actions.  */
            int yyxbegin = yyn < 0 ? -yyn : 0;
            // Stay within bounds of both yycheck and yytname.
            int yychecklim = yylast_ - yyn + 1;
            int yyxend = yychecklim < yyntokens_ ? yychecklim : yyntokens_;
            for (int yyx = yyxbegin; yyx < yyxend; ++yyx)
              if (yycheck_[yyx + yyn] == yyx && yyx != yyterror_
                  && !yy_table_value_is_error_ (yytable_[yyx + yyn]))
                {
                  if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                    {
                      yycount = 1;
                      break;
                    }
                  else
                    yyarg[yycount++] = yytname_[yyx];
                }
          }
      }

    char const* yyformat = YY_NULLPTR;
    switch (yycount)
      {
#define YYCASE_(N, S)                         \
        case N:                               \
          yyformat = S;                       \
        break
      default: // Avoid compiler warnings.
        YYCASE_ (0, YY_("syntax error"));
        YYCASE_ (1, YY_("syntax error, unexpected %s"));
        YYCASE_ (2, YY_("syntax error, unexpected %s, expecting %s"));
        YYCASE_ (3, YY_("syntax error, unexpected %s, expecting %s or %s"));
        YYCASE_ (4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
        YYCASE_ (5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
#undef YYCASE_
      }

    std::string yyres;
    // Argument number.
    size_t yyi = 0;
    for (char const* yyp = yyformat; *yyp; ++yyp)
      if (yyp[0] == '%' && yyp[1] == 's' && yyi < yycount)
        {
          yyres += yytnamerr_ (yyarg[yyi++]);
          ++yyp;
        }
      else
        yyres += *yyp;
    return yyres;
  }


  const signed char  mlaskal_parser ::yypact_ninf_ = -119;

  const signed char  mlaskal_parser ::yytable_ninf_ = -1;

  const short
   mlaskal_parser ::yypact_[] =
  {
      23,    28,    14,    34,  -119,    66,    45,    51,    42,    69,
      46,    47,    49,    55,    76,  -119,    66,    66,  -119,    65,
      86,    67,  -119,  -119,    58,    58,     9,  -119,    76,    70,
      71,    62,  -119,    73,    89,    46,    90,    72,    74,     9,
      78,    61,    61,     9,    80,   -15,    75,   -12,    77,   103,
    -119,  -119,  -119,    51,    51,    13,    81,  -119,    87,  -119,
    -119,  -119,    17,    87,  -119,    91,   106,  -119,  -119,    22,
     100,  -119,    11,    99,    98,    82,    94,    61,    61,  -119,
      95,    97,    61,     9,  -119,  -119,  -119,    66,    66,  -119,
    -119,  -119,  -119,    39,    96,  -119,    43,    92,  -119,   101,
      90,  -119,   102,   104,  -119,    22,    25,    61,  -119,    -5,
      88,   107,     9,  -119,  -119,    61,  -119,     9,    61,    61,
    -119,    93,   105,  -119,    -1,  -119,  -119,  -119,  -119,   109,
     110,  -119,  -119,   111,     6,  -119,  -119,   112,   117,  -119,
      43,    87,   118,  -119,  -119,  -119,   113,  -119,  -119,    22,
    -119,    22,  -119,   115,  -119,  -119,  -119,    85,  -119,    61,
    -119,  -119,    51,    51,   114,  -119,  -119,   116,     4,   119,
      92,   121,   120,  -119,  -119,    -5,    88,     9,    61,    93,
    -119,  -119,    13,    43,  -119,    12,   122,  -119,  -119,    87,
     126,  -119,  -119,  -119,   127,  -119,   123,  -119,  -119,   124,
      43,  -119,   125,  -119,     9,   111,    43,   130,    43,  -119,
    -119,  -119,   119,   131,  -119,    87,  -119
  };

  const unsigned char
   mlaskal_parser ::yydefact_[] =
  {
       0,     0,     0,     0,     1,     7,     0,    24,     0,     9,
      32,     0,     0,     0,     0,    25,     7,     7,     2,     0,
      14,     0,    31,     8,    99,    99,    79,     5,     0,     0,
       0,     0,    10,     0,    19,    32,   103,     0,     0,    79,
       0,    40,    40,    79,     0,    62,     0,     0,    83,     0,
      80,    70,     6,    28,    28,     0,     0,    15,     0,     3,
      33,   104,     0,     0,    97,     0,     0,    72,    41,     0,
       0,    42,    47,     0,     0,     0,     0,    40,    40,    71,
       0,     0,    40,    84,    82,     4,    26,     7,     7,    27,
      37,    38,    39,     0,     0,    34,     0,    88,    20,     0,
     103,   100,     0,     0,    73,     0,    62,    40,    57,    52,
      55,    58,     0,    44,    45,    40,    46,     0,    40,    40,
      68,    65,     0,    86,     0,    69,    67,    85,    81,     0,
       0,    35,    36,    12,     0,    95,    96,     0,     0,    87,
       0,     0,     0,    98,    61,    59,     0,    51,    50,     0,
      49,     0,    54,    75,    48,    76,    77,     0,    43,    40,
      64,    63,    28,    28,     0,    11,    94,     0,     0,    17,
      88,     0,     0,   102,    60,    52,    55,     0,    40,    65,
      29,    30,     0,     0,    93,     0,     0,    16,    89,    22,
       0,    53,    56,    74,     0,    66,     0,    91,    92,     0,
       0,    21,     0,   101,     0,    12,     0,     0,     0,    78,
      13,    90,    17,     0,    18,    22,    23
  };

  const short
   mlaskal_parser ::yypgoto_[] =
  {
    -119,  -119,   128,   129,  -119,    -8,  -119,  -119,  -119,   -75,
    -119,  -119,   -64,  -119,  -119,   -59,  -119,  -119,   -52,  -119,
     132,   -20,   -54,  -119,   -32,   -14,  -119,   -74,  -119,    48,
    -119,   -10,    19,    -7,   -93,    60,  -119,    -9,   -62,  -106,
      20,   108,  -119,   133,   -58,     1,  -119,  -119,  -118,   165,
     166,   149,  -119,    79
  };

  const short
   mlaskal_parser ::yydefgoto_[] =
  {
      -1,     2,    28,    27,     8,    29,     9,    20,    32,   165,
      34,    57,   187,    59,    98,   201,    14,    15,    86,    11,
      22,    94,   108,    69,    70,   157,   115,    71,   116,    72,
     149,   150,   109,   152,   110,    79,   122,   160,    47,    48,
      49,    50,    84,    51,   202,   139,   168,   136,   137,    87,
      88,    37,    62,    63
  };

  const unsigned char
   mlaskal_parser ::yytable_[] =
  {
      99,    95,    89,   121,   123,   102,   153,   111,   127,    30,
      73,   155,   144,   184,     4,   166,    76,    39,   147,    81,
      77,   198,   171,    40,    41,    78,     1,    42,    82,    43,
      76,    44,    97,   146,   185,    45,    46,   148,    97,    78,
      90,    91,    92,   111,   113,   158,   105,   100,   106,    90,
      91,    92,   114,   101,     3,    93,    76,   107,   176,    66,
      77,    12,    13,    74,     5,   197,   131,   132,   134,   135,
       6,   193,    10,    18,    19,    24,   167,    23,    21,   129,
     130,    25,   207,   172,    26,   179,   156,   111,   211,   111,
     213,    31,    33,    36,    35,    55,    58,    61,   209,    56,
      53,    54,    64,    68,   158,    67,    75,    83,    65,    80,
     180,   181,    85,    97,    96,   104,   112,   103,   117,   118,
     120,   124,   119,   126,   138,   159,   133,   199,    95,   178,
     210,   151,   177,     7,   143,   140,   142,   164,    81,   162,
     163,   161,   169,   170,   173,   186,   204,   182,   214,   174,
     183,   189,   203,   205,   190,   200,   216,    52,   206,   208,
     212,   215,   196,   154,   194,   191,   145,    60,   175,   192,
     195,   188,    16,    17,    38,     0,     0,     0,     0,   141,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   128,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   125
  };

  const short
   mlaskal_parser ::yycheck_[] =
  {
      58,    55,    54,    77,    78,    63,   112,    69,    82,    17,
      42,   117,   105,     9,     0,     9,    31,     8,    23,    31,
      35,     9,   140,    14,    15,    40,     3,    18,    40,    20,
      31,    22,    26,   107,    30,    26,    27,    42,    26,    40,
      27,    28,    29,   105,    33,   119,    24,    30,    26,    27,
      28,    29,    41,    36,    26,    42,    31,    35,   151,    39,
      35,    10,    11,    43,    30,   183,    27,    28,    25,    26,
       4,   177,    27,    31,     5,    26,   134,    30,    32,    87,
      88,    26,   200,   141,     8,   159,   118,   149,   206,   151,
     208,    26,     6,    35,    27,    33,     7,     7,   204,    26,
      30,    30,    30,    42,   178,    27,    26,    30,    34,    34,
     162,   163,     9,    26,    33,     9,    16,    26,    19,    21,
      26,    26,    40,    26,    32,    32,    30,   185,   182,    44,
     205,    43,    17,     5,    30,    34,    34,    26,    31,    30,
      30,    36,    30,    26,    26,    26,    19,    33,   212,    36,
      34,    30,    26,    30,    34,    33,   215,    28,    34,    34,
      30,    30,   182,   115,   178,   175,   106,    35,   149,   176,
     179,   170,     7,     7,    25,    -1,    -1,    -1,    -1,   100,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    83,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    80
  };

  const unsigned char
   mlaskal_parser ::yystos_[] =
  {
       0,     3,    46,    26,     0,    30,     4,    47,    49,    51,
      27,    64,    10,    11,    61,    62,    94,    95,    31,     5,
      52,    32,    65,    30,    26,    26,     8,    48,    47,    50,
      50,    26,    53,     6,    55,    27,    35,    96,    96,     8,
      14,    15,    18,    20,    22,    26,    27,    83,    84,    85,
      86,    88,    48,    30,    30,    33,    26,    56,     7,    58,
      65,     7,    97,    98,    30,    34,    85,    27,    42,    68,
      69,    72,    74,    69,    85,    26,    31,    35,    40,    80,
      34,    31,    40,    30,    87,     9,    63,    94,    95,    63,
      27,    28,    29,    42,    66,    67,    33,    26,    59,    89,
      30,    36,    89,    26,     9,    24,    26,    35,    67,    77,
      79,    83,    16,    33,    41,    71,    73,    19,    21,    40,
      26,    72,    81,    72,    26,    88,    26,    72,    86,    50,
      50,    27,    28,    30,    25,    26,    92,    93,    32,    90,
      34,    98,    34,    30,    79,    80,    72,    23,    42,    75,
      76,    43,    78,    84,    74,    84,    69,    70,    72,    32,
      82,    36,    30,    30,    26,    54,     9,    89,    91,    30,
      26,    93,    89,    26,    36,    77,    79,    17,    44,    72,
      63,    63,    33,    34,     9,    30,    26,    57,    90,    30,
      34,    76,    78,    84,    70,    82,    66,    93,     9,    89,
      33,    60,    89,    26,    19,    30,    34,    93,    34,    84,
      54,    93,    30,    93,    57,    30,    60
  };

  const unsigned char
   mlaskal_parser ::yyr1_[] =
  {
       0,    45,    46,    47,    48,    49,    50,    51,    51,    52,
      52,    53,    54,    54,    55,    55,    56,    57,    57,    58,
      58,    59,    60,    60,    61,    61,    62,    62,    63,    63,
      63,    64,    65,    65,    66,    66,    66,    67,    67,    67,
      68,    68,    69,    70,    71,    71,    72,    73,    73,    74,
      75,    75,    76,    76,    77,    78,    78,    79,    79,    79,
      79,    79,    80,    80,    81,    82,    82,    83,    83,    84,
      84,    84,    84,    84,    84,    84,    84,    84,    84,    85,
      85,    86,    86,    87,    87,    88,    88,    89,    90,    90,
      91,    91,    92,    92,    92,    93,    93,    94,    95,    96,
      96,    97,    97,    98,    98
  };

  const unsigned char
   mlaskal_parser ::yyr2_[] =
  {
       0,     2,     5,     4,     3,     3,     2,     0,     3,     0,
       2,     5,     0,     5,     0,     2,     5,     0,     5,     0,
       2,     5,     0,     5,     0,     1,     4,     4,     0,     4,
       4,     2,     0,     3,     1,     2,     2,     1,     1,     1,
       0,     1,     1,     1,     1,     1,     2,     0,     2,     3,
       1,     1,     0,     3,     2,     0,     3,     1,     1,     2,
       3,     2,     0,     3,     2,     0,     3,     3,     3,     3,
       1,     2,     2,     3,     6,     4,     4,     4,     8,     0,
       1,     3,     2,     0,     1,     3,     3,     2,     0,     3,
       5,     3,     4,     3,     2,     1,     1,     4,     6,     0,
       3,     6,     4,     0,     1
  };



  // YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
  // First, the terminals, then, starting at \a yyntokens_, nonterminals.
  const char*
  const  mlaskal_parser ::yytname_[] =
  {
  "\"end of file\"", "error", "$undefined", "PROGRAM", "LABEL", "CONST",
  "TYPE", "VAR", "BEGIN", "END", "PROCEDURE", "FUNCTION", "ARRAY", "OF",
  "GOTO", "IF", "THEN", "ELSE", "WHILE", "DO", "REPEAT", "UNTIL", "FOR",
  "OR", "NOT", "RECORD", "IDENTIFIER", "UINT", "REAL", "STRING",
  "SEMICOLON", "DOT", "COMMA", "EQ", "COLON", "LPAR", "RPAR", "DOTDOT",
  "LSBRA", "RSBRA", "ASSIGN", "OPER_REL", "OPER_SIGNADD", "OPER_MUL",
  "FOR_DIRECTION", "$accept", "mlaskal", "block_common", "body", "block_p",
  "block", "block_p1_opt", "block_p2_opt", "block_p2_list",
  "block_p2_next", "block_p3_opt", "block_p3_list", "block_p3_next",
  "block_p4_opt", "block_p4_list", "block_p4_next", "block_p5_opt",
  "block_p5_list", "block_p5_next", "uint_list_1", "uint_next", "constant",
  "unsigned_constant", "pm_opt", "boolean_expression",
  "ordinal_expression", "expression_cmp", "expression", "expression_next",
  "simple_expression", "simple_expression_bin_op",
  "simple_expression_next", "term", "term_next", "factor",
  "factor_parameters_opt", "real_parameters", "real_parameters_next",
  "variable", "statement", "statement_list_opt", "statements",
  "semicolon_string_opt", "stmt_1", "identifier_list_1", "identifier_next",
  "field_list", "structured_type_3", "type", "procedure_header",
  "function_header", "method_parameters_opt", "formal_parameters",
  "var_string_opt", YY_NULLPTR
  };

#if YYDEBUG
  const unsigned short
   mlaskal_parser ::yyrline_[] =
  {
       0,   114,   114,   121,   126,   131,   136,   140,   141,   144,
     145,   147,   149,   150,   153,   154,   156,   158,   159,   162,
     163,   165,   167,   168,   171,   172,   174,   175,   177,   178,
     179,   182,   185,   186,   189,   190,   191,   195,   196,   197,
     200,   201,   205,   207,   210,   211,   214,   217,   218,   221,
     225,   226,   228,   229,   234,   238,   239,   244,   245,   248,
     249,   250,   253,   254,   257,   260,   261,   264,   265,   269,
     270,   273,   274,   275,   276,   277,   278,   279,   282,   285,
     286,   289,   290,   293,   294,   297,   299,   302,   305,   306,
     311,   312,   315,   316,   317,   320,   321,   324,   327,   330,
     331,   334,   335,   338,   339
  };

  // Print the state stack on the debug stream.
  void
   mlaskal_parser ::yystack_print_ ()
  {
    *yycdebug_ << "Stack now";
    for (stack_type::const_iterator
           i = yystack_.begin (),
           i_end = yystack_.end ();
         i != i_end; ++i)
      *yycdebug_ << ' ' << i->state;
    *yycdebug_ << '\n';
  }

  // Report on the debug stream that the rule \a yyrule is going to be reduced.
  void
   mlaskal_parser ::yy_reduce_print_ (int yyrule)
  {
    unsigned yylno = yyrline_[yyrule];
    int yynrhs = yyr2_[yyrule];
    // Print the symbols being reduced, and their result.
    *yycdebug_ << "Reducing stack by rule " << yyrule - 1
               << " (line " << yylno << "):\n";
    // The symbols being reduced.
    for (int yyi = 0; yyi < yynrhs; yyi++)
      YY_SYMBOL_PRINT ("   $" << yyi + 1 << " =",
                       yystack_[(yynrhs) - (yyi + 1)]);
  }
#endif // YYDEBUG


} // yy
#line 1235 "du3456g.tab.cc"

#line 341 "../private-src/du3456g.y"



namespace yy {

	void mlaskal_parser::error(const location_type& l, const std::string& m)
	{
		message(DUERR_SYNTAX, l, m);
	}

}

