-module(text_util).

%% ====================================================================
%% API functions
%% ====================================================================
-export([getParam/1, getParamValue/2]).

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

%% Extract Param Value
getParamValue(Name, List) when Name == keywords ; Name == authors ->
	Result = getParamValue(Name, List),
	string:tokens(Result, ",");
getParamValue(Name, List) ->
	Fun = fun(X) -> case X of {Name, _} -> true; _ -> false end end,
	case lists:filter(Fun, List) of
		[{_,Value}] -> Value;
		_ -> undefined
	end.

%% Build Paper from raw
build(Raw) -> 
	File = file_util:get_file_lines(Raw),
	Params = getParam(File),
	
	
	

			