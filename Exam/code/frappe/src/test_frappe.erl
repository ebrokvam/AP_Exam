-module(test_frappe).

-export([test_all/0, test_everything/0]).
-export([initial_state/0, command/1, precondition/2, postcondition/3, next_state/3]).
-export([mktrans/2, terminating_transformation/1, prop_cache_under_capacity/0, prop_capacity_invariant/0, prop_unique_keys/0, prop_stopped_server/0]).

-export([prop_cache/0, frappe_fresh/0]).

-include_lib("eqc/include/eqc.hrl").
-include("apqc_statem.hrl").

-behaviour(apqc_statem).

test_all() ->
  eqc:quickcheck(test_frappe:prop_cache_under_capacity()),
  eqc:quickcheck(test_frappe:prop_capacity_invariant()),
  eqc:quickcheck(test_frappe:prop_unique_keys()),
  eqc:quickcheck(test_frappe:prop_stopped_server()),
  test_eunit:test_all().

test_everything() ->
  test_all().

%% TODO: use limited versions since I haven't implemented stable/2

% Cache behaves normally under capacity
prop_cache_under_capacity() -> undefined.

% Capacity invariant is never broken
prop_capacity_invariant() ->
  ?FORALL(FS, cache(50),
    capacity_invariant(FS, 50)).

% No duplicate keys
prop_unique_keys() ->
  ?FORALL(FS, cache(),
    no_duplicates(frappe:all_items(FS))). %% TODO: convert to list of keys

% Stopped servers cannot receive calls
prop_stopped_server() -> undefined.

% Transformation generator
mktrans(Opr, Args) ->
  case Opr of
    new ->
      fun(new) -> {new_value, Args, 5} end;
    add_val ->
      fun({existing, Val}) -> {new_value, Val+Args, 5} end;
    set_cap ->
      fun({existing, Val}) -> {new_value, Val, Args} end;
    val_is_cap ->
      fun({existing, Val}) -> {new_value, Val, Val} end;
    throw ->
      fun({existing, _}) -> throw(Args) end
  end.

terminating_transformation(KeyGen) -> 
  {KeyGen, ?LET(Fun, list(trans_fun()), {call, ?MODULE, mktrans, Fun})}.

trans_fun() ->
  {oneof([new, add_val, set_cap, val_is_cap, throw]), value()}.

%% Attempt at cache generator #1:
cache() ->
  {ok, FS} = frappe:fresh(int()),
  eval(gen_cache(FS)),
  FS.
cache(Cap) ->
  {ok, FS} = frappe:fresh(Cap),
  eval(gen_cache(FS)),
  FS.

gen_cache(FS) ->
  ?LAZY(
      oneof([
        ?LET({K,V,C}, {key(), value(), cap()}, {call, frappe, set, [FS,K,V,C]})
      ])
    ).

% Attempt at cache generator #2: (aqpc)
prop_cache() ->
  ?FORALL(Cmds, commands(?MODULE),
    begin
      {_, FS, _} = Result = run_commands(?MODULE, Cmds),
      cleanup(FS),
      check_commands(Cmds, Result)
    end).

check_commands(Cmds, {_,_,Res} = HSRes) ->
  pretty_commands(?MODULE, Cmds, HSRes,
    aggregate(command_names(Cmds),
      equals(Res, ok))).

cleanup(FS) ->
  frappe:stop(FS).

-record(state,{fs}).

initial_state() ->
  #state{fs = none}.

command(#state{fs = none}) ->
  return({call, ?MODULE, frappe_fresh, [[]]});
command(FS) ->
  oneof([{call, frappe, set, [FS, key(), value(), cap()] }]).

next_state(S, FS, {call, frappe_fresh, _}) ->
  S#state{fs = FS};
next_state(S, _, _) ->
  S.

precondition(_, _) ->
  true.

postcondition(_, _, _) ->
  true.

frappe_fresh() ->
  {ok, FS} = frappe:fresh(int()),
  FS.

% Other generators
key() ->
  oneof([int(), char()]).
value() -> 
  frequency([{4, int()}, {1, char()}]). % minimise chance of error in transformation function (eg. char+int, setting char as cap)
cap() -> 
  int().

% Helper functions
no_duplicates(Lst) ->
  length(Lst) =:= length(lists:usort(Lst)). %% TODO: THIS IS WRONG, DONT USE USORT

capacity_invariant(FS, Cap) -> 
  sum_capacities(frappe:all_items(FS)) =< Cap.

sum_capacities([]) -> 0;
sum_capacities([{_, _, C} | Queue]) ->
	C + sum_capacities(Queue).