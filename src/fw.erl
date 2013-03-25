-module(fw).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0, refresh_layout/0]).

%% ====================================================================
%% Includes
%% ====================================================================
-include("configuration.hrl").
-include("struct.hrl").

%% ====================================================================
%% Internal functions
%% ====================================================================

%% Start Session
start() ->
	debug_module:move_to_root(),
	mnesia:start().

refresh_layout() ->
	file_util:cp("Layout/style.css", "Out/style.css"),
	file_util:cp("Layout/front.html", "Out/index.html"),
	ok.
