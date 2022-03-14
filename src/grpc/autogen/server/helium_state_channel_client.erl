%%%-------------------------------------------------------------------
%% @doc Client module for grpc service helium.state_channel.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated on 2022-03-14T14:52:48+00:00 and should not be modified manually

-module(helium_state_channel_client).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("grpcbox/include/grpcbox.hrl").

-define(is_ctx(Ctx), is_tuple(Ctx) andalso element(1, Ctx) =:= ctx).

-define(SERVICE, 'helium.state_channel').
-define(PROTO_MODULE, 'state_channel_pb').
-define(MARSHAL_FUN(T), fun(I) -> ?PROTO_MODULE:encode_msg(I, T) end).
-define(UNMARSHAL_FUN(T), fun(I) -> ?PROTO_MODULE:decode_msg(I, T) end).
-define(DEF(Input, Output, MessageType), #grpcbox_def{service=?SERVICE,
                                                      message_type=MessageType,
                                                      marshal_fun=?MARSHAL_FUN(Input),
                                                      unmarshal_fun=?UNMARSHAL_FUN(Output)}).

-spec msg() ->
    {ok, grpcbox_client:stream()} | grpcbox_stream:grpc_error_response() | {error, any()}.
msg() ->
    msg(ctx:new(), #{}).

-spec msg(ctx:t() | grpcbox_client:options()) ->
    {ok, grpcbox_client:stream()} | grpcbox_stream:grpc_error_response() | {error, any()}.
msg(Ctx) when ?is_ctx(Ctx) ->
    msg(Ctx, #{});
msg(Options) ->
    msg(ctx:new(), Options).

-spec msg(ctx:t(), grpcbox_client:options()) ->
    {ok, grpcbox_client:stream()} | grpcbox_stream:grpc_error_response() | {error, any()}.
msg(Ctx, Options) ->
    grpcbox_client:stream(Ctx, <<"/helium.state_channel/msg">>, ?DEF(blockchain_state_channel_message_v1_pb, blockchain_state_channel_message_v1_pb, <<"helium.blockchain_state_channel_message_v1">>), Options).

