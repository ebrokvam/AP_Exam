-module(test_eunit).

-export([test_all/0]).

-include_lib("eunit/include/eunit.hrl").

test_all() -> eunit:test(testsuite(), [verbose]).

testsuite() ->
    [ {"Start/stop tests", spawn,
       [ 
         test_fresh(),
         test_fresh_multiple(),
         test_fresh_negative_cap(),
         test_stop(),
         test_stop_multiple()
       ]
      },
      {"Insert tests", spawn,
       [ 
         test_insert(),
         test_insert_over_cap(),
         test_insert_existing()
       ]
      },
      {"Read tests", spawn,
       [ 
         test_read_existing(),
         test_read_nonexisting()
       ]
      },
      {"Update tests", spawn,
       [ 
         test_update(),
         test_update_over_cap(),
         test_update_nonexisting()
       ]
      },
      {"Set tests", spawn,
       [ 
         test_set_nonexisting(),
         test_set_nonexisting_then_read(),
         test_set_nonexisting_over_cap(),
         test_set_existing_then_read(),
         test_set_existing_over_cap()
       ]
      },
      {"All_Items tests", spawn,
       [ 
         test_all_items(),
         test_all_items_empty()
       ]
      },
      {"Upsert tests", spawn,
       [ 
         test_upsert_insert(),
         test_upsert_insert_over_cap(),
         test_upsert_insert_existing(),
         test_upsert_insert_returns_wrong(),
         test_upsert_insert_function_throws_newval(),
         test_upsert_insert_function_throws_error(),

         test_upsert_update(),
         test_upsert_update_over_cap(),
         test_upsert_update_nonexisting(),
         test_upsert_update_returns_wrong(),
         test_upsert_update_function_throws_newval(),
         test_upsert_update_function_throws_error()
       ]
      },
      {"Stable tests", spawn,
       [ 
         % TODO
       ]
      },
      {"LRU tests", spawn,
       [ 
         test_LRU_set_removes_item(),
         test_LRU_insert_removes_item(),
         test_LRU_update_removes_item(),
         test_LRU_upsert_insert_removes_item(),
         test_LRU_upsert_update_removes_item(),

         test_LRU_read_changes_order(),
         test_LRU_update_changes_order(),
         test_LRU_upsert_update_changes_order_on_success(),
         test_LRU_upsert_no_update_does_not_change_order()
        %  test_LRU_stable_changes_order()
       ]
      },
      {"Key coherency tests", spawn,
       [
         test_key_coherency_set(),
         test_key_coherency_update(),
         test_key_coherency_upsert()
       ]
      },
      {"Key concurrency tests", spawn,
       [
         % TODO
       ]
      },
      {"Efficienty tests", spawn,
       [
         % TODO
       ]
      }
    ].

test_fresh() ->
  {"Start a frappe server",
    fun () ->
      ?assertMatch({ok, _}, frappe:fresh(5))
    end }.

test_fresh_multiple() ->
  {"Start multiple frappe servers",
    fun () ->
      ?assertMatch({ok, _}, frappe:fresh(5)),
      ?assertMatch({ok, _}, frappe:fresh(6)),
      ?assertMatch({ok, _}, frappe:fresh(12))
    end }.

test_fresh_negative_cap() ->
  {"Start frappe server with negative capacity",
    fun () ->
      ?assertMatch({error, _}, frappe:fresh(-5))
    end }.

test_stop() ->
  {"Start and stop a frappe server",
    fun () ->
      {ok, FS} = frappe:fresh(5),
      ?assertMatch(ok, frappe:stop(FS))
    end }.

test_stop_multiple() ->
  {"Start and stop multiple frappe servers",
    fun () ->
      {ok, FS} = frappe:fresh(5),
      {ok, FS2} = frappe:fresh(6),
      {ok, FS3} = frappe:fresh(12),
      ?assertMatch(ok, frappe:stop(FS)),
      ?assertMatch(ok, frappe:stop(FS2)),
      ?assertMatch(ok, frappe:stop(FS3))
    end }.

test_insert() ->
  {"Insert item",
    fun () ->
      {ok, FS} = frappe:fresh(5),
      ?assertEqual(ok, frappe:insert(FS, key, val, 3))
    end }.

test_insert_over_cap() ->
  {"Insert that breaks capacity invariant",
    fun () ->
      {ok, FS} = frappe:fresh(5),
      ?assertMatch({error, _}, frappe:insert(FS, key, val, 6))
    end }.

test_insert_existing() ->
  {"Insert item with key that exists",
    fun () ->
      {ok, FS} = frappe:fresh(5),
      frappe:insert(FS, key, val, 3),
      ?assertMatch({error, _}, frappe:insert(FS, key, val, 2))
    end }.

test_read_existing() ->
  {"Read existing item",
    fun () ->
      {ok, FS} = frappe:fresh(5),
      frappe:insert(FS, key, val, 3),
      ?assertEqual({ok, val}, frappe:read(FS, key))
    end }.

test_read_nonexisting() ->
  {"Read non-existing item",
    fun () ->
      {ok, FS} = frappe:fresh(5),
      ?assertEqual(nothing, frappe:read(FS, key))
    end }.

test_update() ->
  {"Update existing item",
    fun () ->
      {ok, FS} = frappe:fresh(5),
      frappe:insert(FS, key, val, 3),
      frappe:update(FS, key, newval, 3),
      ?assertEqual({ok, newval}, frappe:read(FS, key))
    end }.

test_update_over_cap() ->
  {"Update to break capacity invariant",
    fun () ->
      {ok, FS} = frappe:fresh(5),
      frappe:insert(FS, key, val, 3),
      ?assertMatch({error, _}, frappe:update(FS, key, val, 6))
    end }.

test_update_nonexisting() ->
  {"Update nonexisting item",
    fun () ->
      {ok, FS} = frappe:fresh(5),
      ?assertMatch({error, _}, frappe:update(FS, key, val, 3))
    end }.

test_set_nonexisting() ->
  {"Set new item that does not break capacity invariant",
    fun () ->
      {ok, FS} = frappe:fresh(5),
      ?assertEqual(ok, frappe:set(FS, key, val, 3))
    end }.

test_set_nonexisting_over_cap() ->
  {"Set new item that does break capacity invariant",
    fun () ->
      {ok, FS} = frappe:fresh(5),
      ?assertMatch({error, _}, frappe:set(FS, key, val, 6))
    end }.

test_set_nonexisting_then_read() ->
  {"Set new item that does not break capacity invariant and read",
    fun () ->
      {ok, FS} = frappe:fresh(5),
      frappe:set(FS, key, val, 3),
      ?assertEqual({ok, val}, frappe:read(FS, key))
    end }.

test_set_existing_then_read() ->
  {"Set existing item so that it does not break capacity invariant",
    fun () ->
      {ok, FS} = frappe:fresh(5),
      frappe:insert(FS, key, val, 3),
      frappe:set(FS, key, newval, 3),
      ?assertEqual({ok, newval}, frappe:read(FS, key))
    end }.

test_set_existing_over_cap() ->
  {"Set existing item so that it does break capacity invariant",
    fun () ->
      {ok, FS} = frappe:fresh(5),
      frappe:insert(FS, key, val, 3),
      ?assertMatch({error, _}, frappe:set(FS, key, val, 6))
    end }.

test_all_items() ->
  {"Insert three items then call all_items/1",
    fun () ->
      {ok, FS} = frappe:fresh(10),
      frappe:insert(FS, key, val, 3),
      frappe:insert(FS, key2, val, 3),
      frappe:insert(FS, key3, val, 3),
      ?assertEqual([{key,val,3},{key2,val,3},{key3,val,3}], frappe:all_items(FS))
    end }.

test_all_items_empty() ->
  {"Call all_items/1 on an empty cache",
    fun () ->
      {ok, FS} = frappe:fresh(5),
      ?assertEqual([], frappe:all_items(FS))
    end }.

test_upsert_insert() ->
  {"Call upsert to insert a new item",
    fun () ->
      {ok, FS} = frappe:fresh(5),
      ok = frappe:upsert(FS, key,
                fun(new) -> {new_value, val, 4} end),
      ?assertEqual({ok, val}, frappe:read(FS, key))
    end }.

test_upsert_insert_over_cap() ->
  {"Call upsert to insert a new item and break capacity invariant",
    fun () ->
      {ok, FS} = frappe:fresh(5),
      ?assertMatch({error, _}, frappe:upsert(FS, key,
                fun(new) -> {new_value, val, 6} end)),
      ?assertEqual(nothing, frappe:read(FS, key))
    end }.

test_upsert_insert_existing() ->
  {"Call upsert to insert an already existing key (bad_arg to function)",
    fun () ->
      {ok, FS} = frappe:fresh(5),
      ok = frappe:insert(FS, key, val, 3),
      ?assertEqual(ok, frappe:upsert(FS, key,
                fun(new) -> {new_value, 2, 3} end)),
      ?assertEqual({ok, val}, frappe:read(FS, key))
    end }.

test_upsert_insert_returns_wrong() ->
  {"Call upsert to insert new item with function that returns incorrect response",
    fun () ->
      {ok, FS} = frappe:fresh(5),
      ?assertEqual(ok, frappe:upsert(FS, key, fun({new}) -> no end))
    end }.

test_upsert_insert_function_throws_newval() ->
  {"Call upsert with function that throws a newvalue and then inserts",
    fun () ->
      {ok, FS} = frappe:fresh(5),
      ?assertMatch(ok, frappe:upsert(FS, key,
                fun(new) -> throw({new_value, 1, 4}) end)),
      ?assertEqual({ok, 1}, frappe:read(FS, key))
    end }.

test_upsert_insert_function_throws_error() ->
  {"Call upsert to insert new item with function that throws an error",
    fun () ->
      {ok, FS} = frappe:fresh(5),
      ?assertMatch(ok, frappe:upsert(FS, key,
                fun(new) -> throw("no") end))
    end }.

test_upsert_update() ->
  {"Call upsert to update an existing item",
    fun () ->
      {ok, FS} = frappe:fresh(5),
      ok = frappe:insert(FS, key, 23, 3),
      ok = frappe:upsert(FS, key,
                fun({existing, Val}) -> {new_value, Val+2, 4} end),
      ?assertEqual({ok, 25}, frappe:read(FS, key))
    end }.

test_upsert_update_over_cap() ->
  {"Call upsert to update an item and break capacity invariant",
    fun () ->
      {ok, FS} = frappe:fresh(5),     
      ok = frappe:insert(FS, key, 2, 3),
      ?assertMatch({error, _}, frappe:upsert(FS, key,
                fun({existing, Val}) -> {new_value, Val+2, 6} end)),
      ?assertEqual({ok, 2}, frappe:read(FS, key))
    end }.

test_upsert_update_nonexisting() ->
  {"Call upsert to update a nonexisting key (bad_arg to function)",
    fun () ->
      {ok, FS} = frappe:fresh(5),
      ?assertMatch(ok, frappe:upsert(FS, key,
                fun({existing, Val}) -> {new_value, Val+3, 3} end)),
      ?assertEqual(nothing, frappe:read(FS, key))
    end }.

test_upsert_update_returns_wrong() ->
  {"Call upsert to update with function that returns incorrect response",
    fun () ->
      {ok, FS} = frappe:fresh(5),
      ok = frappe:insert(FS, key, 23, 3),
      ?assertEqual(ok, frappe:upsert(FS, key,
                fun({existing, _Val}) -> no end))
    end }.

test_upsert_update_function_throws_newval() ->
  {"Call upsert with function that throws a newvalue and then updates",
    fun () ->
      {ok, FS} = frappe:fresh(5),
      ok = frappe:insert(FS, key, 1, 3),
      ?assertMatch(ok, frappe:upsert(FS, key,
                fun({existing, Val}) -> throw({new_value, Val+1, 4}) end)),
      ?assertEqual({ok, 2}, frappe:read(FS, key))
    end }.

test_upsert_update_function_throws_error() ->
  {"Call upsert to update with function that tries to add integer to string",
    fun () ->
      {ok, FS} = frappe:fresh(5),
      ok = frappe:insert(FS, key, val, 3),
      ?assertMatch(ok, frappe:upsert(FS, key,
                fun({existing, Val}) ->
                  New = Val+2,
                  {new_value, New, 4}
                end)),
      ?assertEqual({ok, val}, frappe:read(FS, key))
    end }.

test_LRU_set_removes_item() ->
  {"Test LRU holds for set function",
    fun () ->
      {ok, FS} = frappe:fresh(5),
      frappe:set(FS, key, val, 3),
      frappe:set(FS, key2, val, 3),
      ?assertEqual(nothing, frappe:read(FS, key)),
      ?assertEqual({ok, val}, frappe:read(FS, key2))
    end }.

test_LRU_insert_removes_item() ->
  {"Test LRU holds for insert function",
    fun () ->
      {ok, FS} = frappe:fresh(5),
      frappe:insert(FS, key, val, 3),
      frappe:insert(FS, key2, val, 3),
      ?assertEqual(nothing, frappe:read(FS, key)),
      ?assertEqual({ok, val}, frappe:read(FS, key2))
    end }.

test_LRU_update_removes_item() ->
  {"Test LRU holds for update function",
    fun () ->
      {ok, FS} = frappe:fresh(5),
      frappe:insert(FS, key, val, 3),
      frappe:insert(FS, key2, val, 2),
      frappe:update(FS, key2, newval, 3),
      ?assertEqual(nothing, frappe:read(FS, key)),
      ?assertEqual({ok, newval}, frappe:read(FS, key2))
    end }.

test_LRU_upsert_insert_removes_item() ->
  {"Test LRU holds for upsert function when inserting",
    fun () ->
      {ok, FS} = frappe:fresh(5),
      ok = frappe:insert(FS, key, newval, 2),
      ok = frappe:upsert(FS, key2, fun(new) -> {new_value, val, 4} end),
      ?assertEqual(nothing, frappe:read(FS, key)),
      ?assertEqual({ok, val}, frappe:read(FS, key2))
    end }.

test_LRU_upsert_update_removes_item() ->
  {"Test LRU holds for upsert function when updating",
    fun () ->
      {ok, FS} = frappe:fresh(5),
      ok = frappe:insert(FS, key, newval, 2),
      ok = frappe:insert(FS, key2, 4, 2),
      ok = frappe:upsert(FS, key2, fun({existing, Val}) -> {new_value, Val+1, 4} end),
      ?assertEqual(nothing, frappe:read(FS, key)),
      ?assertEqual({ok, 5}, frappe:read(FS, key2))
    end }.

test_LRU_read_changes_order() ->
  {"Test changing of order by adding two items, reading LRU, inserting another item to break capacity, then attempt to read popped item",
    fun () ->
      {ok, FS} = frappe:fresh(5),
      frappe:insert(FS, key, val, 3),
      frappe:insert(FS, key2, val, 2),
      frappe:read(FS, key),
      frappe:insert(FS, key3, val, 1),
      ?assertEqual(nothing, frappe:read(FS, key2))
    end }.

test_LRU_update_changes_order() ->
  {"Test changing of order by adding two items, updating LRU, inserting another item to break capacity, then attempt to read popped item",
    fun () ->
      {ok, FS} = frappe:fresh(5),
      frappe:insert(FS, key, val, 3),
      frappe:insert(FS, key2, val, 2),
      frappe:update(FS, key, newval, 3),
      frappe:insert(FS, key3, val, 1),
      ?assertEqual(nothing, frappe:read(FS, key2))
    end }.

test_LRU_upsert_update_changes_order_on_success() ->
  {"Test changing of order by adding two items, successfully upsert updating LRU, inserting another item to break capacity, then attempt to read popped item",
    fun () ->
      {ok, FS} = frappe:fresh(5),
      frappe:insert(FS, key, 3, 3),
      frappe:insert(FS, key2, val, 2),
      frappe:upsert(FS, key, fun({existing, Val}) -> {new_value, Val+2, 3} end),
      frappe:insert(FS, key3, val, 1),
      ?assertEqual(nothing, frappe:read(FS, key2))
    end }.

test_LRU_upsert_no_update_does_not_change_order() ->
  {"Test changing of order by adding two items, unsuccessfully upsert updating LRU, inserting another item to break capacity, then attempt to read popped item",
    fun () ->
      {ok, FS} = frappe:fresh(5),
      frappe:insert(FS, key, 3, 3),
      frappe:insert(FS, key2, val, 2),
      frappe:upsert(FS, key, fun({existing, _Val}) -> no end),
      frappe:insert(FS, key3, val, 1),
      ?assertEqual({ok, val}, frappe:read(FS, key2))
    end }.

test_key_coherency_set() ->
  {"Make two set calls to same key in sequencial order",
    fun () ->
      {ok, FS} = frappe:fresh(5),
      ok = frappe:insert(FS, key, 1, 3),
      ok = frappe:set(FS, key, 2, 3),
      ok = frappe:set(FS, key, 3, 3),
      ?assertEqual({ok, 3}, frappe:read(FS, key))
    end }.

test_key_coherency_update() ->
  {"Make two update calls to same key in sequencial order",
    fun () ->
      {ok, FS} = frappe:fresh(5),
      ok = frappe:insert(FS, key, 1, 3),
      ok = frappe:update(FS, key, 2, 3),
      ok = frappe:update(FS, key, 3, 3),
      ?assertEqual({ok, 3}, frappe:read(FS, key))
    end }.

test_key_coherency_upsert() ->
  {"Make two upsert calls to same key in sequencial order",
    fun () ->
      {ok, FS} = frappe:fresh(5),
      ok = frappe:insert(FS, key, 1, 3),
      ok = frappe:upsert(FS, key, fun({existing, Val}) -> {new_value, Val+1, 3} end),
      ok = frappe:upsert(FS, key, fun({existing, Val}) -> {new_value, Val+1, 3} end),
      ?assertEqual({ok, 3}, frappe:read(FS, key))
    end }.