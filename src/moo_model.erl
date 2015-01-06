-module(moo_model).
-export([parse_bindings/4, parse_qs/4, parse_proplist/4, parse_json/4,
         parse_body/5]).
-export_type([content_type/0]).

-type content_type() :: {binary(), binary(), '*' | [{binary(), binary()}]}.
-type parsing_result(T) :: {ok, T, cowboy_req:req()}
                         | {error, Reason :: term(), cowboy_req:req()}.
-type attributes() :: [atom() | binary()].

-callback parse_attribute(Name, Value, State) -> {ok, State} | {error, Reason} when
	  Name :: binary(),
	  Value :: term(),
	  State :: term(),
	  Reason :: term().

-spec parse_bindings(cowboy_req:req(), module(), T, attributes()) -> parsing_result(T).
parse_bindings(Req, Module, State, Attributes) ->
	{Bindings, Req2} = cowboy_req:bindings(Req),
	return(Req2, parse_proplist(Bindings, Module, State, Attributes)).

-spec parse_qs(cowboy_req:req(), module(), T, attributes()) -> parsing_result(T).
parse_qs(Req, Module, State, Attributes) ->
	{Qs, Req2} = cowboy_req:qs_vals(Req),
	return(Req2, parse_proplist(Qs, Module, State, Attributes)).

-spec parse_body(content_type(), cowboy_req:req(), module(), T, attributes()) -> parsing_result(T).
parse_body({<<"application">>, <<"x-www-form-urlencoded">>, _},
           Req, Module, State, Attributes) ->
	case cowboy_req:body_qs(Req) of
		{ok, BodyQs, Req2} ->
			return(Req2, parse_proplist(BodyQs, Module, State, Attributes));
		{error, Reason} ->
			{error, Reason, Req}
	end;
parse_body({<<"application">>, <<"json">>, _},
           Req, Module, State, Attributes) ->
	case cowboy_req:body(Req) of
		{ok, Body, Req2} ->
			return(Req2, parse_json(Body, Module, State, Attributes));
		{error, Reason} ->
			{error, Reason, Req}
	end;
parse_body(_, Req, _, _, _) ->
	{error, unsupported, Req}.

-spec parse_json(binary(), module(), T, attributes()) -> {ok, T} | {error, term()}.
parse_json(Json, Module, State, Attributes) ->
	try jsx:decode(Json) of
		[{}] ->
			{ok, State};
		Proplist when is_list(Proplist) ->
			parse_proplist(Proplist, Module, State, Attributes);
		_ ->
			{error, malformed_json}
	catch
		error:badarg ->
			{error, malformed_json}
	end.

-spec parse_proplist([{atom() | binary(), term()}], module(), T, attributes()) -> {ok, T} | {error, term()}.
parse_proplist(Proplist, Module, State, Attributes) ->
	parse_proplist1(Proplist, Module, State, normalize_attributes(Attributes)).

parse_proplist1([], _, State, _) ->
	{ok, State};
parse_proplist1([{K, V}|Rest], Module, State, Attributes) ->
	case parse_attribute(K, V, Module, State, Attributes) of
		{ok, NewState} -> parse_proplist1(Rest, Module, NewState, Attributes);
		{error, _} = Err -> Err
	end.

parse_attribute(K, V, Module, State, Attributes) when is_atom(K) ->
	parse_attribute(atom_to_binary(K, latin1), V, Module, State, Attributes);
parse_attribute(K, V, Module, State, Attributes) ->
	case lists:member(K, Attributes) of
		true -> Module:parse_attribute(K, V, State);
		false -> {error, forbidden_attribute}
	end.

normalize_attributes(Attributes) ->
	lists:map(
	  fun(Attr) when is_atom(Attr)   -> atom_to_binary(Attr, latin1);
	     (Attr) when is_binary(Attr) -> Attr end,
	  Attributes
	).

return(Req, {ok, Result}) -> {ok, Result, Req};
return(Req, {error, Reason}) -> {error, Reason, Req}.
