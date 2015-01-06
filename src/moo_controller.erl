-module(moo_controller).
-export([rest_init/5, parse_body/5]).

rest_init(Req, Module, State, BindingAttributes, QsAttributes) ->
	case parse_bindings(Req, Module, State, BindingAttributes) of
		{ok, State2, Req2} ->
			case parse_qs(Req2, Module, State2, QsAttributes) of
				{ok, State3, Req3} ->
					{ok, Req3, State3};
				{error, Reason, Req3} ->
					{{error, Reason}, Req3, State2}
			end;
		{error, Reason, Req2} ->
			{{error, Reason}, Req2, State}
	end.

parse_body(ContentType, Req, Module, State, Attributes) ->
	case moo_model:parse_body(ContentType, Req, Module, State, Attributes) of
		{ok, State2, Req2} -> {ok, Req2, State2};
		{error, unsupported, Req2} -> {unsupported, Req2, State};
		{error, Reason, Req2} -> {{error, Reason}, Req2, State}
	end.

parse_bindings(Req, _, State, []) -> {ok, State, Req};
parse_bindings(Req, Module, State, Attrs) ->
	moo_model:parse_bindings(Req, Module, State, Attrs).

parse_qs(Req, _, State, []) -> {ok, State, Req};
parse_qs(Req, Module, State, Attrs) ->
	moo_model:parse_qs(Req, Module, State, Attrs).
