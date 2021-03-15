-module(decimal).

%% Converters
-export([
         to_decimal/2,
         to_decimal/3,
         to_binary/1,
         to_binary/2
        ]).

%% Arith
-export([
         add/2,
         sub/2,
         mult/2,
         divide/3,
         sqrt/2
        ]).

%% Compare
-export([
         fast_cmp/2,
         cmp/3
        ]).

%% Utils
-export([
         abs/1,
         minus/1,
         is_zero/1,
         reduce/1,
         round/3
        ]).

-compile(inline).
-include("decimal.hrl").

-type decimal() :: #decimal{}.
-type old_decimal() :: {0|1, non_neg_integer(), integer()}.
-type rounding_algorithm() :: round_floor | round_ceiling |
                              round_half_up | round_half_down |
                              round_down.
-type opts() :: #{
              precision => non_neg_integer(),
              rounding => rounding_algorithm()
           }.

-export_type([
              decimal/0,
              opts/0,
              rounding_algorithm/0
             ]).

%% =============================================================================
%%% API
%% =============================================================================

%% = Converters ================================================================

-spec to_decimal(Value, Opts) -> decimal() when
      Value :: integer() | float() | binary() | list() |
               decimal() | old_decimal(),
      Opts :: opts().
to_decimal(#decimal{}=Decimal, _Opts)->
    Decimal;
to_decimal(Int, #{precision := Precision, rounding := Rounding}) when
      is_integer(Int) ->
    round(Rounding, #decimal{base = Int, exp = 0}, Precision);
to_decimal(Binary, #{precision := Precision, rounding := Rounding}) when
      is_binary(Binary) ->
    Decimal = decimal_conv:from_binary(Binary),
    round(Rounding, Decimal, Precision);
to_decimal(Float, #{precision := Precision, rounding := Rounding}) when
      is_float(Float) ->
    Decimal = decimal_conv:from_float(Float),
    round(Rounding, Decimal, Precision);
to_decimal(List, #{precision := Precision, rounding := Rounding}) when
      is_list(List) ->
    Decimal = decimal_conv:from_list(List),
    round(Rounding, Decimal, Precision);
%% Old decimal format support
to_decimal({Sign, Base0, Exp}, _Opts) when
      is_integer(Sign), is_integer(Base0), is_integer(Exp) ->
    Base = case Sign of 1 -> -Base0; 0 -> Base0 end,
    #decimal{base = Base, exp = Exp}.

-spec to_decimal(Base, Exp, Opts) -> decimal() when
      Base :: integer(),
      Exp :: integer(),
      Opts :: opts().
to_decimal(Base, Exp, _Opts) ->
    #decimal{base = Base, exp = Exp}.

-spec to_binary(decimal()) -> binary().
to_binary(Decimal) ->
    decimal_conv:to_binary(Decimal, #{pretty => true}).

-spec to_binary(decimal(), Opts) -> binary() when
      Opts :: decimal_conv:binary_opts().
to_binary(Decimal, Opts) ->
    decimal_conv:to_binary(Decimal, Opts).

%% = Arith =====================================================================

-spec add(decimal(), decimal()) -> decimal().
add(#decimal{base = Int1, exp = E1}, #decimal{base = Int2, exp = E2}) ->
    Emin = min(E1, E2),
    #decimal{base = Int1 * pow_of_ten(E1 - Emin) +
     Int2 * pow_of_ten(E2 - Emin),
     exp = Emin}.

-spec sub(decimal(), decimal()) -> decimal().
sub(A, B) ->
    add(A, minus(B)).

-spec mult(decimal(), decimal()) -> decimal().
mult(#decimal{base = Int1, exp = E1}, #decimal{base = Int2, exp = E2}) ->
    #decimal{base = Int1*Int2, exp = E1+E2}.

-spec divide(decimal(), decimal(), opts()) -> decimal().
divide(#decimal{base = M, exp = E}, #decimal{base = 2, exp = 0}, #{ precision := Precision, rounding := Rounding }) when (M band 1) == 0 ->
    round(Rounding, #decimal{base = M bsr 1, exp = E}, Precision);
divide(#decimal{base = M, exp = E}, #decimal{base = 2, exp = 0}, #{ precision := Precision, rounding := Rounding }) ->
    round(Rounding, #decimal{base = M * 5, exp = E-1}, Precision);
divide(#decimal{base = BaseA, exp = ExpA},#decimal{base = BaseB, exp = ExpB}, #{ precision := Precision0, rounding := Rounding }) ->
    Precision = max(0, -(ExpB - ExpA)) + Precision0 + 1,
    BaseRes = BaseA * pow_of_ten(Precision) div BaseB,
    round(Rounding, #decimal{base = BaseRes, exp = ExpA - ExpB - Precision}, Precision0).

-spec sqrt(decimal(), opts()) -> decimal() | no_return(). %% @throws error(badarith)
sqrt(#decimal{base = M, exp = _E}, _Context) when M < 0 ->
    error(badarith);
sqrt(#decimal{base = 0, exp = _E}, _Context) ->
    #decimal{base = 0, exp = 0};
sqrt(#decimal{base = M, exp = E}=Decimal, #{precision := Precision0} = Context) ->
    Precision = Precision0 + 1,
    CoefficientDigits = length(integer_to_list(M)),
    case E band 1 of
        0 ->
            Shift = Precision - ((CoefficientDigits + 1) bsr 1),
            sqrt(Decimal, Context, Shift, M);
        _ ->
            Shift = Precision - ((CoefficientDigits bsr 1) + 1),
            sqrt(Decimal, Context, Shift, M * 10)
    end.

sqrt(Decimal, Context, Shift, M) ->
    case Shift >= 0 of
        true ->
            sqrt(Decimal, Context, Shift, M * pow_of_ten(Shift bsl 1), true);
        false ->
            Operand = pow_of_ten((- Shift) bsl 1),
            sqrt(Decimal, Context, Shift, M div Operand, M rem Operand =:= 0)
    end.

sqrt(#decimal{exp = E0}, #{precision := Precision, rounding := Rounding}, Shift, M, Exact) ->
    E = E0 bsr 1,
    N = sqrt_loop(M, pow_of_ten(Precision + 1)),
    Result = case Exact and (N * N =:= M) of
        true ->
            case Shift >= 0 of
                true ->
                    #decimal{base = N div pow_of_ten(Shift), exp = E};
                false ->
                    #decimal{base = N * pow_of_ten(-Shift), exp = E}
            end;
        false ->
            #decimal{base = N, exp = E - Shift}
    end,
    round(Rounding, Result, Precision).

sqrt_loop(M, N) ->
    Q = M div N,
    case N =< Q of
        true ->
            N;
        false ->
            sqrt_loop(M, (N + Q) bsr 1)
    end.

%% = Compare ===================================================================

-spec cmp(decimal(), decimal(), opts()) -> -1 | 0 | 1.
cmp(#decimal{base = 0}, #decimal{base = 0}, _Opts) ->
    0;
cmp(#decimal{base = Int1}, #decimal{base = Int2}, _Opts) when Int1 >= 0, Int2 =< 0 ->
    1;
cmp(#decimal{base = Int1}, #decimal{base = Int2}, _Opts) when Int1 =< 0, Int2 >= 0 ->
    -1;
cmp(#decimal{base = Int, exp = E}, #decimal{base = Int, exp = E}, _Opts) ->
    0;
cmp(#decimal{base = Int1, exp = E}, #decimal{base = Int2, exp = E}, _Opts) when Int1 > Int2 ->
    1;
cmp(#decimal{base = Int1, exp = E}, #decimal{base = Int2, exp = E}, _Opts) when Int1 < Int2 ->
    -1;
cmp(A, B, #{ precision := Precision, rounding := Rounding }) ->
    #decimal{base = Int1, exp = E1} = round(Rounding, A, Precision),
    #decimal{base = Int2, exp = E2} = round(Rounding, B, Precision),
    Emin = min(E1, E2),
    B1 = Int1*pow_of_ten(E1-Emin),
    B2 = Int2*pow_of_ten(E2-Emin),
    if B1 < B2 -> -1;
       B1 > B2 -> 1;
       true -> 0
    end.

%% Fast compare without rounding
fast_cmp(#decimal{base = 0}, #decimal{base = 0}) ->
    0;
fast_cmp(#decimal{base = Int1}, #decimal{base = Int2}) when Int1 >= 0, Int2 =< 0 ->
    1;
fast_cmp(#decimal{base = Int1}, #decimal{base = Int2}) when Int1 =< 0, Int2 >= 0 ->
    -1;
fast_cmp(#decimal{base = Int, exp = E}, #decimal{base = Int, exp = E}) ->
    0;
fast_cmp(#decimal{base = Int1, exp = E}, #decimal{base = Int2, exp = E}) when Int1 > Int2 ->
    1;
fast_cmp(#decimal{base = Int1, exp = E}, #decimal{base = Int2, exp = E}) when Int1 < Int2 ->
    -1;
fast_cmp(#decimal{base = Int1, exp = E1}, #decimal{base = Int2, exp = E2}) ->
    Emin = min(E1, E2),
    B1 = Int1*pow_of_ten(E1-Emin),
    B2 = Int2*pow_of_ten(E2-Emin),
    if B1 < B2 -> -1;
       B1 > B2 -> 1;
       true -> 0
    end.

%% = Utils =====================================================================

-spec is_zero(decimal()) -> boolean().
is_zero(#decimal{base = 0}) -> true;
is_zero(_) -> false.

-spec minus(decimal()) -> decimal().
minus(#decimal{base = Int, exp = E}) ->
    #decimal{base = -Int, exp = E}.

-spec abs(decimal()) -> decimal().
abs(#decimal{base = Int, exp = E}) ->
    #decimal{base = erlang:abs(Int), exp = E}.

-spec reduce(decimal()) -> decimal().
reduce(#decimal{base = Int, exp = E}) ->
    reduce_(Int, E).
reduce_(0, _E) -> #decimal{base = 0, exp = 0};
reduce_(Int, E) ->
    case Int rem 10 of
        0 -> reduce_(Int div 10, E+1);
        _ -> #decimal{base = Int, exp = E}
    end.

-spec round(rounding_algorithm(), decimal(), non_neg_integer()) -> decimal().
round(Rounding, #decimal{base = Int, exp = E}=Decimal, Precision) ->
    reduce(
        case -Precision-E of
            Delta when Delta > 0 ->
                round_(Rounding, Int, E, Delta);
            _ ->
                Decimal
        end).

round_(round_down, Int, E, Delta) ->
    P = pow_of_ten(Delta),
    Base = Int div P,
    zero_exp_(Base, E+Delta);
round_(Rounding, Int, E, Delta) when
      Rounding =:= round_ceiling;
      Rounding =:= round_floor ->
    P = pow_of_ten(Delta),
    Base0 = Int div P,
    Diff = Int-Base0*P,
    Base =
        case Rounding of
            round_floor when Diff < 0 ->
                Base0 - 1;
            round_ceiling when Diff > 0 ->
                Base0 + 1;
            _ ->
                Base0
        end,
    zero_exp_(Base, E+Delta);
round_(Rounding, Int, E, Delta) ->
    P = pow_of_ten(Delta-1),
    Data = Int div P,
    Base0 = Data div 10,
    LastDigit = erlang:abs(Data-(Base0*10)),
    Base =
        case Rounding of
            round_half_up when LastDigit >= 5, Data > 0 ->
                Base0 + 1;
            round_half_up when LastDigit >= 5, Data < 0  ->
                Base0 - 1;
            round_half_down when LastDigit > 5, Data > 0 ->
                Base0 + 1;
            round_half_down when LastDigit > 5, Data < 0  ->
                Base0 - 1;
            _ ->
                Base0
        end,
    zero_exp_(Base, E+Delta).

zero_exp_(0, _Exp) -> #decimal{base = 0,exp = 0};
zero_exp_(Base, Exp) -> #decimal{base = Base, exp = Exp}.

%% =============================================================================
%%% Internal functions
%% =============================================================================

-spec pow_of_ten(non_neg_integer()) -> pos_integer().
pow_of_ten(N) ->
    if N > 0 -> int_pow(10, N, 1);
       true  -> 1
    end.

int_pow(X, N, R) when N < 2 ->
    R * X;
int_pow(X, N, R) ->
    int_pow(X * X, N bsr 1, case N band 1 of 1 -> R * X; 0 -> R end).
