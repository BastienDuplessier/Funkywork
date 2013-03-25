-module(debug_module).

%% ====================================================================
%% API functions
%% ====================================================================
-export([print_list/1, web_page/1, web_project_page/1, move_to_root/0]).

%% ====================================================================
%% Includes
%% ====================================================================
-include("configuration.hrl").
-include("struct.hrl").

%% ====================================================================
%% Internal functions
%% ====================================================================

%% Print List 
print_list([]) -> ok;
print_list([H|T]) -> 
	io:format("~p~n", [H]),
	print_list(T).

%% Launch Web Page 
web_page(Url) ->
	Command = string:concat("start chrome ", Url),
	os:cmd(Command),
	ok.

%% Launch a Project's Page
web_project_page(File) ->
	Command = string:concat(?ROOT_PROJECT, File),
	web_page(Command).

%% Move to FW project
move_to_root() ->
	c:cd(?ROOT_PROJECT).

