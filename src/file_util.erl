-module(file_util).

%% ====================================================================
%% API functions
%% ====================================================================
-export([get_file_lines/1, get_file_info/1, cp/2]).

%% ====================================================================
%% Includes
%% ====================================================================
-include_lib("kernel/include/file.hrl").
-include("configuration.hrl").
-include("struct.hrl").

%% ====================================================================
%% Internal functions
%% ====================================================================

%% Open file
open_file(File) ->
	case file:open(File, [read]) of
		{ok, Device} -> Device;
		{error, Reason} -> exit(Reason)
	end.

%% Get file's info
get_file_info(File) ->
	case file:read_file_info(File) of
		{ok, Infos} -> Infos;
		{error, Reason} -> exit(Reason)
	end.

%% Get file's line : API
get_file_lines(File) ->
	get_file_lines(open_file(File), []).

%% Get file's line function
get_file_lines(FileData, Acc) ->
	case io:get_line(FileData, "") of
		eof -> 
			file:close(FileData),
			lists:reverse(Acc);
		Line -> get_file_lines(FileData, [Line|Acc])
	end.

%% Move file
cp(Source, Destination) ->
	file:copy(Source, Destination).

