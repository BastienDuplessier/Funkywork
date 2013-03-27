-module(database).

%% ====================================================================
%% API functions
%% ====================================================================
-export([database_create_schema/0, create_table/1]).
-export([insert/1, get_next_id/1]).
-export([select/2, erase/2, update/2]).

%% ====================================================================
%% Includes
%% ====================================================================
-include("configuration.hrl").
-include("struct.hrl").

%% ====================================================================
%% Internal functions
%% ====================================================================

%% Initialize Database
database_create_schema() ->
	mnesia:create_schema([node()]).

%% Create table
create_table(Record) ->
	case mnesia:create_table(
		   Record, 
		   [{attributes, record_info_var(Record)},
      		{type, ordered_set},
      		{disc_copies, [node()]}]
	) of
		{atomic, ok} 		-> io:format("Table ~w 's create~n", [Record]);
		{aborted, Reason} 	-> exit(Reason)
	end.

%% Field  Representation
record_info_var(series) ->
	record_info(fields, series);
record_info_var(paper) ->
	record_info(fields, paper);
record_info_var(author) ->
	record_info(fields, author).

%% Extract id
record_id(series, Record) ->
	Record#series.id;
record_id(paper, Record) ->
	Record#paper.id;
record_id(author, Record) ->
	Record#author.id.

%% Insert Row
insert(Record) ->
	Transaction = fun() -> mnesia:write(Record) end,
	{atomic, ok} = mnesia:transaction(Transaction),
	{ok, recorded}.


%% Get next ID 
get_next_id(Table) ->
	Transaction = fun() -> mnesia:last(Table) end,
   	case mnesia:transaction(Transaction) of 
		{aborted, Reason} -> exit(Reason);
      	{atomic, '$end_of_table'} -> 0;
      	{atomic, Id} -> Id + 1
   	end.

%% Select from ID 
select(Table, Id) ->
	Transaction = fun() ->  mnesia:read(Table, Id) end,
	{atomic, Result} = mnesia:transaction(Transaction),
	Result.

%% Erase
erase(Table, Id) ->
	Transaction = fun() -> mnesia:delete({Table, Id}) end,
	{atomic, _} = mnesia:transaction(Transaction).
	
%% Update 
update(Table, Record) ->
	Id = record_id(Table, Record),
	erase(Table, Id),
	insert(Record).


