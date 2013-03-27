-module(paper).

%% ====================================================================
%% API functions
%% ====================================================================
-export([build/11, build/12, add/11, all/0]).

%% ====================================================================
%% Includes
%% ====================================================================
-include("configuration.hrl").
-include("struct.hrl").

%% ====================================================================
%% Internal functions
%% ====================================================================

%% Build a new Record
build(Url, Title, Authors, Description, Date, Parent, Draft, 
	  Series_id, Keywords, Comment, Content) ->
	Id = database:get_next_id(paper),
	build(Id ,Url, Title, Authors, Description, Date, Parent, Draft, 
		  Series_id, Keywords, Comment, Content).

build(Id ,Url, Title, Authors, Description, Date, Parent, Draft, 
	  Series_id, Keywords, Comment, Content) ->
		#paper{
			id = Id,
			url = Url,
			title = Title,
			authors = Authors,
			description = Description,
			date = Date,
			parent = Parent,
			draft = Draft,
			series_id = Series_id,
			keywords = Keywords,
			comment = Comment,
			content = Content
		}.
  
%% Add paper
add(Url, Title, Authors, Description, Date, Parent, Draft, 
	  Series_id, Keywords, Comment, Content) ->
	Record = build(Url, Title, Authors, Description, Date, Parent, Draft, 
	  Series_id, Keywords, Comment, Content),
	database:insert(Record),
	ok.

%% Get all papers
all() ->
	Transaction = fun() -> mnesia:match_object(#paper{ _ = '_'}) end,
	{atomic, Result} = mnesia:transaction(Transaction),
	Result.
	