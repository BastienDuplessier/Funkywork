-module(text_util).

%% ====================================================================
%% API functions
%% ====================================================================
-export([getParam/1]).

%% ====================================================================
%% Includes
%% ====================================================================
-include("configuration.hrl").
-include("struct.hrl").

%% ====================================================================
%% Internal functions
%% ====================================================================

%% Get File Parameters
getParam(File) ->getParam(File, [], []).
getParam([], Acc, HTML) -> {ok, Acc, HTML};
getParam([Head|Tail], Acc, HTML) ->
	Regex = "\<\:(.+)=(.+)\:\>", 
	case re:split(Head, Regex, [{return, list}]) of
		[_,Id,Value,_] ->
			Param = list_to_atom(Id),
			Args = [{Param, Value}|Acc],
			getParam(Tail, Args, HTML);
		_ -> {ok, Acc, [Head|Tail]}
	end.
	