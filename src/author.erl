-module(author).

%% ====================================================================
%% API functions
%% ====================================================================
-export([build/2, build/3, add/2, all/0]).
-export([getById/1, getByNickname/1]).

%% ====================================================================
%% Includes
%% ====================================================================
-include("configuration.hrl").
-include("struct.hrl").

%% ====================================================================
%% Internal functions
%% ====================================================================

%% Build a new Record
build(Nickname, Mail) ->
	Id = database:get_next_id(author),
	build(Id, Nickname, Mail).

build(Id, Nickname, Mail) ->
	#author{
		id = Id,
		nickname = Nickname,
		mail = Mail
	}.

%% Add author
add(Nickname, Mail) ->
	Record = build(Nickname, Mail),
	database:insert(Record),
	ok.

%% Get all authors
all() ->
	Transaction = fun() -> mnesia:match_object(#author{ _ = '_'}) end,
	{atomic, Result} = mnesia:transaction(Transaction),
	Result.
	
%% Get By Id
getById(Id) ->
	[Result] = database:select(author, Id), 
	Result.

%% Get By NickName
getByNickname(Nick) ->
	Filter = fun(X) -> X#author.nickname == Nick end,
	[Result] = lists:filter(Filter, all()),
	Result.
						  