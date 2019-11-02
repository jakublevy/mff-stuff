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





#include "du3456g.hpp"


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


#line 64 "du3456g.cpp"


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
#line 155 "du3456g.cpp"


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

#line 693 "du3456g.cpp"

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


  const short  mlaskal_parser ::yypact_ninf_ = -147;

  const signed char  mlaskal_parser ::yytable_ninf_ = -85;

  const short
   mlaskal_parser ::yypact_[] =
  {
      15,     4,    41,    30,  -147,    32,    49,    58,    47,    82,
      63,    66,    64,    71,    80,  -147,    69,    74,  -147,    75,
      99,    79,  -147,  -147,    20,    20,    39,  -147,    32,    32,
      76,  -147,    84,   105,    63,    87,    85,    86,  -147,  -147,
      89,    39,    91,    72,    72,    39,    90,    24,    92,    88,
    -147,    81,    93,    94,    39,   115,  -147,    80,    97,   102,
      -2,    96,  -147,    87,  -147,  -147,   100,   109,  -147,   110,
     111,   129,  -147,  -147,    -7,   123,  -147,     1,   121,   120,
    -147,   103,   116,    72,   -10,  -147,    72,   122,    39,  -147,
    -147,  -147,    58,    58,  -147,  -147,  -147,    56,   117,  -147,
     119,  -147,    60,  -147,   118,   110,    85,  -147,   126,  -147,
    -147,  -147,    -7,    25,    72,    81,  -147,     6,    27,   127,
      39,  -147,  -147,    72,    39,    72,    72,   128,  -147,  -147,
    -147,   130,   112,    62,  -147,  -147,  -147,  -147,  -147,   131,
     133,  -147,  -147,  -147,   108,   124,    36,  -147,  -147,  -147,
    -147,  -147,  -147,   134,    60,   126,  -147,    26,  -147,  -147,
     132,  -147,  -147,  -147,    -7,  -147,  -147,  -147,  -147,    -7,
    -147,   148,  -147,  -147,  -147,   125,  -147,   -10,  -147,  -147,
    -147,    32,    32,  -147,   135,  -147,  -147,   136,    14,   140,
     137,  -147,    87,   138,  -147,     6,    27,    39,    72,   130,
      62,   143,   146,    -2,    60,  -147,   168,   145,  -147,    87,
     147,   110,  -147,  -147,  -147,   160,  -147,  -147,    58,    58,
     150,   152,  -147,    60,  -147,   149,   110,   126,    39,  -147,
    -147,   124,    87,  -147,   154,    60,   126,  -147,  -147,  -147,
     151,   140,   156,  -147,    60,  -147,    87,   152,  -147,  -147
  };

  const unsigned char
   mlaskal_parser ::yydefact_[] =
  {
       0,     0,     0,     0,     1,     7,     0,    24,     0,     9,
      32,     0,     0,     0,     0,    25,     0,     0,     2,     0,
      14,     0,    31,     8,   121,   121,     0,     5,     7,     7,
       0,    10,     0,    19,    32,     0,   101,     0,   122,   114,
       0,     0,     0,    51,    51,     0,     0,    38,     0,     0,
      82,    75,    84,     0,    96,     0,    86,     0,     0,     0,
       0,     0,    15,     0,     3,    33,     0,     0,   100,     0,
       0,     0,    88,    52,     0,     0,    53,    57,     0,     0,
      39,     0,     0,    51,    51,    87,    51,     0,    96,    95,
       4,     6,    28,    28,    34,    48,    49,     0,     0,    47,
       0,    44,     0,    20,     0,     0,   101,    41,   118,    43,
     115,    89,     0,    34,    51,    75,    70,    62,    68,    71,
       0,    55,    56,    51,     0,    51,    51,    35,    85,    99,
      36,    79,     0,    79,    98,    37,    83,    97,    26,     0,
       0,    27,    45,    46,     0,    12,     0,    40,   106,   112,
     111,   113,   107,     0,     0,   118,   102,     0,   117,    74,
       0,    72,    61,    60,     0,    59,    67,    65,    66,     0,
      64,    91,    58,    92,    93,     0,    54,    51,    77,    76,
      78,     7,     7,    50,     0,    11,   110,     0,     0,    17,
       0,   116,     0,     0,    73,    62,    68,     0,    51,    79,
      79,     0,     0,     0,     0,   109,     0,     0,    16,    22,
       0,     0,    63,    69,    90,     0,    80,    81,    28,    28,
       0,   104,   108,     0,    21,     0,     0,   118,     0,    29,
      30,    12,     0,   103,     0,     0,   118,   120,    94,    13,
       0,    17,     0,   119,     0,    18,    22,   104,    23,   105
  };

  const short
   mlaskal_parser ::yypgoto_[] =
  {
    -147,  -147,   182,   139,  -147,   -27,  -147,  -147,  -147,   -43,
    -147,  -147,   -52,  -147,  -147,   -55,  -147,  -147,   -88,  -147,
     158,  -147,   -61,  -147,  -147,  -147,  -147,  -147,   -66,  -147,
    -147,   -13,   -56,  -147,   -33,    -5,  -147,   -77,    77,  -147,
      -1,    31,  -147,     2,   -97,    95,  -147,  -119,   -62,  -147,
     -53,    34,   113,   141,   -35,    98,  -147,   -48,  -147,  -147,
    -146,   195,   196,  -147,  -145,   180
  };

  const short
   mlaskal_parser ::yydefgoto_[] =
  {
      -1,     2,    57,    27,     8,    58,     9,    20,    31,   185,
      33,    62,   208,    64,   103,   224,    14,    15,   138,    11,
      22,    99,    49,    50,   136,    51,    81,   148,   149,   150,
     110,   100,   116,    74,    75,   175,   123,    76,    77,   164,
     165,   117,   169,   170,   118,    85,   132,   178,    52,    53,
      54,    55,    89,    56,    37,    68,   188,   233,   151,   152,
     153,   139,   140,    38,   158,    39
  };

  const short
   mlaskal_parser ::yytable_[] =
  {
      66,    88,    59,   108,   101,   141,   129,   131,   190,   134,
     191,    78,   119,   115,   180,   159,   130,   112,     1,   113,
      95,    96,   133,   205,    94,    95,    96,    35,   104,   162,
       3,   114,    73,   192,   121,    88,     6,   160,    98,   155,
      97,     4,   122,    98,   206,   186,    36,    41,   163,   176,
     119,   115,    36,    42,    43,   -36,   -36,    44,   221,    45,
       5,    46,    36,   -35,   -35,    47,    48,   171,    12,    13,
     166,   173,   196,   167,   168,    71,    10,   234,    18,    79,
     216,   217,   237,   142,   143,   146,   147,    19,    26,   242,
      24,   243,   174,   -84,   177,    21,    23,    25,   247,    28,
     199,    30,   119,   115,    29,    32,    34,   119,   115,    60,
      61,   187,    63,    36,    73,   200,    80,    67,    72,    84,
      69,   176,   193,    70,    90,    87,    82,    92,    83,   102,
     229,   230,    93,    86,   105,   106,   107,   109,   111,   120,
     124,   125,   127,   126,   214,   227,   144,   101,   135,   145,
     184,   179,   154,   183,   201,   202,   157,   210,   -84,   -36,
     236,   181,   177,   182,   189,   197,   207,   209,   203,   198,
     204,   194,   211,   218,   225,   238,   219,   222,   223,   228,
     231,   226,   232,   235,   241,   244,   246,     7,   239,   245,
     220,   248,    65,   215,   212,   195,    91,   240,   213,   249,
     172,   137,    16,    17,   156,    40,     0,     0,     0,     0,
     161,   225,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   128
  };

  const short
   mlaskal_parser ::yycheck_[] =
  {
      35,    54,    29,    69,    60,    93,    83,    84,   154,    86,
     155,    44,    74,    74,   133,   112,    26,    24,     3,    26,
      27,    28,    84,     9,    26,    27,    28,     7,    63,    23,
      26,    38,    42,     7,    33,    88,     4,   114,    45,   105,
      42,     0,    41,    45,    30,     9,    26,     8,    42,   126,
     112,   112,    26,    14,    15,    31,    31,    18,   204,    20,
      30,    22,    26,    38,    40,    26,    27,   120,    10,    11,
      43,   124,   169,    46,    47,    41,    27,   223,    31,    45,
     199,   200,   227,    27,    28,    25,    26,     5,     8,   235,
      26,   236,   125,    31,    32,    32,    30,    26,   244,    30,
     177,    26,   164,   164,    30,     6,    27,   169,   169,    33,
      26,   146,     7,    26,    42,   177,    26,    32,    27,    38,
      34,   198,   157,    34,     9,    31,    34,    30,    40,    33,
     218,   219,    30,    40,    34,    26,    26,    26,     9,    16,
      19,    21,    26,    40,   197,   211,    29,   203,    26,    30,
      26,    39,    34,    45,   181,   182,    30,   192,    31,    31,
     226,    30,    32,    30,    30,    17,    26,    30,    33,    44,
      34,    39,    34,    30,   209,   228,    30,     9,    33,    19,
      30,    34,    30,    34,    30,    34,    30,     5,   231,   241,
     203,   246,    34,   198,   195,   164,    57,   232,   196,   247,
     123,    88,     7,     7,   106,    25,    -1,    -1,    -1,    -1,
     115,   246,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    82
  };

  const unsigned char
   mlaskal_parser ::yystos_[] =
  {
       0,     3,    49,    26,     0,    30,     4,    50,    52,    54,
      27,    67,    10,    11,    64,    65,   109,   110,    31,     5,
      55,    32,    68,    30,    26,    26,     8,    51,    30,    30,
      26,    56,     6,    58,    27,     7,    26,   102,   111,   113,
     113,     8,    14,    15,    18,    20,    22,    26,    27,    70,
      71,    73,    96,    97,    98,    99,   101,    50,    53,    53,
      33,    26,    59,     7,    61,    68,   102,    32,   103,    34,
      34,    99,    27,    42,    81,    82,    85,    86,    82,    99,
      26,    74,    34,    40,    38,    93,    40,    31,    98,   100,
       9,    51,    30,    30,    26,    27,    28,    42,    45,    69,
      79,    80,    33,    62,   102,    34,    26,    26,    76,    26,
      78,     9,    24,    26,    38,    70,    80,    89,    92,    96,
      16,    33,    41,    84,    19,    21,    40,    26,   101,    85,
      26,    85,    94,    96,    85,    26,    72,   100,    66,   109,
     110,    66,    27,    28,    29,    30,    25,    26,    75,    76,
      77,   106,   107,   108,    34,    76,   103,    30,   112,    92,
      85,    93,    23,    42,    87,    88,    43,    46,    47,    90,
      91,    98,    86,    98,    82,    83,    85,    32,    95,    39,
      95,    30,    30,    45,    26,    57,     9,   102,   104,    30,
     108,   112,     7,   102,    39,    89,    92,    17,    44,    85,
      96,    53,    53,    33,    34,     9,    30,    26,    60,    30,
     102,    34,    88,    91,    98,    83,    95,    95,    30,    30,
      79,   108,     9,    33,    63,   102,    34,    76,    19,    66,
      66,    30,    30,   105,   108,    34,    76,   112,    98,    57,
     102,    30,   108,   112,    34,    60,    30,   108,    63,   105
  };

  const unsigned char
   mlaskal_parser ::yyr1_[] =
  {
       0,    48,    49,    50,    51,    52,    53,    54,    54,    55,
      55,    56,    57,    57,    58,    58,    59,    60,    60,    61,
      61,    62,    63,    63,    64,    64,    65,    65,    66,    66,
      66,    67,    68,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    79,    79,    80,    80,    80,
      80,    81,    81,    82,    83,    84,    84,    85,    85,    86,
      87,    87,    88,    88,    89,    90,    90,    90,    91,    91,
      92,    92,    92,    92,    92,    93,    93,    94,    94,    95,
      95,    95,    96,    96,    97,    98,    98,    98,    98,    98,
      98,    98,    98,    98,    98,    99,   100,   100,   101,   101,
     102,   103,   103,   104,   105,   105,   106,   106,   107,   107,
     107,   108,   108,   108,   109,   110,   111,   111,   112,   112,
     112,   113,   113
  };

  const unsigned char
   mlaskal_parser ::yyr2_[] =
  {
       0,     2,     5,     4,     3,     3,     2,     0,     3,     0,
       2,     5,     0,     5,     0,     2,     5,     0,     5,     0,
       2,     5,     0,     5,     0,     1,     5,     5,     0,     5,
       5,     2,     0,     3,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     2,     2,     1,     1,     1,
       3,     0,     1,     1,     1,     1,     1,     1,     3,     3,
       1,     1,     0,     3,     2,     1,     1,     1,     0,     3,
       1,     1,     2,     3,     2,     0,     3,     2,     2,     0,
       3,     3,     1,     3,     1,     3,     1,     2,     2,     3,
       6,     4,     4,     4,     8,     2,     0,     2,     3,     3,
       2,     0,     3,     4,     0,     5,     1,     1,     4,     3,
       2,     1,     1,     1,     3,     5,     5,     4,     0,     6,
       5,     0,     1
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
  "FOR_DIRECTION", "\"'\"", "\"*\"", "\"/\"", "$accept", "mlaskal",
  "block_common", "body", "block_p", "block", "block_p1_opt",
  "block_p2_opt", "block_p2_list", "block_p2_next", "block_p3_opt",
  "block_p3_list", "block_p3_next", "block_p4_opt", "block_p4_list",
  "block_p4_next", "block_p5_opt", "block_p5_list", "block_p5_next",
  "uint_list_1", "uint_next", "constant_identifier", "function_identifier",
  "variable_identifier", "field_identifier", "procedure_identifier",
  "ordinal_type_variable_identifier", "structured_type_identifier",
  "type_identifier", "ordinal_type_identifier", "scalar_type_identifier",
  "constant", "unsigned_constant", "pm_opt", "boolean_expression",
  "ordinal_expression", "expression_cmp", "expression",
  "simple_expression", "simple_expression_bin_op",
  "simple_expression_next", "term", "term_bin_op", "term_next", "factor",
  "factor_parameters_opt", "real_parameters", "real_parameters_next",
  "variable", "record_variable", "statement", "statement_list_1",
  "statement_next", "stmt_1", "identifier_list_1", "identifier_next",
  "field_list", "field_list_next", "structured_type", "structured_type_3",
  "type", "procedure_header", "function_header", "method_parameters",
  "method_parameters_next", "method_parameters_opt", YY_NULLPTR
  };

#if YYDEBUG
  const unsigned short
   mlaskal_parser ::yyrline_[] =
  {
       0,   114,   114,   121,   126,   131,   136,   140,   141,   144,
     145,   147,   149,   150,   153,   154,   156,   158,   159,   162,
     163,   165,   167,   168,   171,   172,   174,   175,   177,   178,
     179,   182,   185,   186,   190,   194,   196,   198,   200,   202,
     204,   206,   208,   210,   213,   214,   215,   218,   219,   220,
     221,   224,   225,   233,   235,   238,   239,   242,   243,   246,
     250,   251,   253,   254,   259,   263,   264,   265,   268,   269,
     274,   275,   276,   277,   278,   281,   282,   285,   286,   289,
     290,   291,   294,   295,   299,   303,   304,   305,   306,   307,
     308,   309,   310,   311,   312,   315,   318,   319,   322,   323,
     326,   329,   330,   335,   337,   338,   341,   342,   344,   345,
     346,   349,   350,   351,   354,   356,   358,   359,   361,   362,
     363,   365,   366
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
#line 1258 "du3456g.cpp"

#line 370 "../private-src/du3456g.y"



namespace yy {

	void mlaskal_parser::error(const location_type& l, const std::string& m)
	{
		message(DUERR_SYNTAX, l, m);
	}

}

