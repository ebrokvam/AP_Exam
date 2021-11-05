-module(frappe).

% You are allowed to split your Erlang code in as many files as you
% find appropriate.
% However, you MUST have a module (this file) called frappe.

% Export at least the API:
-export([fresh/1,
         set/4,
         read/2,
         insert/4,
         update/4,
         upsert/3,
         stable/3,
         all_items/1,
         stop/1
        ]).

% You may have other exports as well
-export([]).

fresh(Cap) ->
  server:start(Cap).

set(FS, Key, Value, C) ->
  server:set(FS, Key, Value, C).

read(FS, Key) ->
  server:read(FS, Key).

insert(FS, Key, Value, C) ->
  server:insert(FS, Key, Value, C).

update(FS, Key, Value, C) ->
  server:update(FS, Key, Value, C).

upsert(FS, Key, Fun) ->
  not_implemented.

stable(FS, Key, Ref) ->
  not_implemented.

all_items(FS) ->
  server:all_items(FS).

stop(FS) ->
  server:stop(FS).
