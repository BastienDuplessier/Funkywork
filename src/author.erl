-module(author).

%% ====================================================================
%% API functions
%% ====================================================================
-export([build/2, build/3, add/2]).

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