-module(server).

-export([start/1, set/4, read/2, insert/4, update/4, all_items/1, stop/1]).

%% gen_server functions
-export([init/1, handle_call/3, handle_cast/2]).

-behaviour(gen_server).

% export functions

start(Cap) ->
	gen_server:start(?MODULE, Cap, []).

set(FS, Key, Value, C) ->
	gen_server:call(FS, {set, {Key, Value, C}}).

read(FS, Key) ->
	gen_server:call(FS, {read, Key}).

insert(FS, Key, Value, C) ->
	gen_server:call(FS, {insert, {Key, Value, C}}).

update(FS, Key, Value, C) ->
	gen_server:call(FS, {update, {Key, Value, C}}).

all_items(FS) ->
	gen_server:call(FS, {all_items}).

stop(FS) ->
	gen_server:stop(FS).

% server functions

init(Cap) -> 
	{ok, {Cap, []}}.

handle_call(Request, _From, {Cap, Queue} = State) -> 
	case Request of
		{insert, {Key, _Val, _C} = Item} ->
			case lists:keymember(Key, 1, Queue) of
				true ->
					{reply, {error, "Key already exists"}, State};
				false ->
					insert_item(Item, State)
			end;
		{read, Key} ->
			case lists:keyfind(Key, 1, Queue) of
				{Key, Val, _C} = Item ->
					% new state has reordered queue
					{reply, {ok, Val}, {Cap, lists:keydelete(Key, 1, Queue) ++ [Item]}};
				false ->
					{reply, nothing, State}
			end;
		{update, {Key, _Val, _C} = Item} ->
			case lists:keymember(Key, 1, Queue) of
				true ->
					update_item(Item, State);
				false ->
					{reply, {error, "Key does not exists"}, State}
			end;
		{set, {Key, _Val, _C} = Item} ->
			case lists:keymember(Key, 1, Queue) of
				true ->
					update_item(Item, State);
				false ->
					insert_item(Item, State)
			end;
		{all_items} ->
			{reply, Queue, State}
	end.

handle_cast(Request, State) -> undefined.

insert_item({_Key, _Val, C} = Item, {Cap, Queue} = State) ->
	case C =< Cap of
		true ->
			NewQueue = Queue ++ [Item],
			case is_exceeded_capacity(Cap, NewQueue) of
				true ->
					{_, PoppedQueue} = lists:split(1, NewQueue),
					{reply, ok, {Cap, PoppedQueue}};
				false ->
					{reply, ok, {Cap, NewQueue}}
			end;
		false ->
			{reply, {error, "Broken capacity invariant"}, State}
	end.

update_item({Key, _Val, C} = Item, {Cap, Queue} = State) ->
	case C =< Cap of
		true ->
			% update and reorder in queue
			NewQueue = lists:keydelete(Key, 1, Queue) ++ [Item],
			case is_exceeded_capacity(Cap, NewQueue) of
				true ->
					{_, PoppedQueue} = lists:split(1, NewQueue),
					{reply, ok, {Cap, PoppedQueue}};
				false ->
					{reply, ok, {Cap, NewQueue}}
			end;
		false ->
			{reply, {error, "Broken capacity invariant"}, State}
	end.

is_exceeded_capacity(Cap, Queue) ->
	sum_capacities(Queue) > Cap.

sum_capacities([]) -> 0;
sum_capacities([{_, _, C} | Queue]) ->
	C + sum_capacities(Queue).