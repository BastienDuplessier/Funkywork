-module(series).

%% ====================================================================
%% API functions
%% ====================================================================
-export([build/4, build/5, add/4, all/0]).

%% ====================================================================
%% Includes
%% ====================================================================
-include("configuration.hrl").
-include("struct.hrl").

%% ====================================================================
%% Internal functions
%% ====================================================================

%% Build a new record
build(Url, Parent, Title, Description) ->
	Id = database:get_next_id(series),
	build(Id, Url, Parent, Title, Description).
	
build(Id, Url, Parent, Title, Description) ->
	#series{
	 	id = Id,
		url = Url,
		parent = Parent,
		title = Title, 
		description = Description
	}.

%% Add series
add(Url, Parent, Title, Description) ->
	Record = build(Url, Parent, Title, Description),
	database:insert(Record),
	ok.

%% Get all Series
all() ->
	Transaction = fun() -> mnesia:match_object(#series{ _ = '_'}) end,
	{atomic, Result} = mnesia:transaction(Transaction),
	Result.
	