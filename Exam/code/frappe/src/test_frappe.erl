-module(test_frappe).

-export([test_all/0, test_everything/0]).
-export([prop_cache_under_capacity/0, prop_capacity_invariant/0]). % Remember to export the other functions from Q2.2

-include_lib("eqc/include/eqc.hrl").

% You are allowed to split your testing code in as many files as you
% think is appropriate, just remember that they should all start with
% 'test_'.
% But you MUST have a module (this file) called test_frappe.

test_all() ->
  ok.

test_everything() ->
  test_all().

mktrans(Ops, Args) -> undefined.

terminating_transformation(KeyGen) -> undefined.

prop_cache_under_capacity() -> undefined.

prop_capacity_invariant() -> undefined.

%% TODO: use limited versions if I don't implement stable/2