%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for grpc service helium.state_channel.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated on 2022-03-14T14:52:48+00:00 and should not be modified manually

-module(helium_state_channel_bhvr).

-callback msg(state_channel_pb:blockchain_state_channel_message_v1_pb(), grpcbox_stream:t()) ->
    ok | {ok, grpcbox_stream:t()} | {ok, state_channel_pb:blockchain_state_channel_message_v1_pb(), grpcbox_stream:t()} | {stop, grpcbox_stream:t()} | {stop, state_channel_pb:blockchain_state_channel_message_v1_pb(), grpcbox_stream:t()} | grpcbox_stream:grpc_error_response().

-callback init(atom(), grpcbox_stream:t()) -> grpcbox_stream:t().
-callback handle_info(any(), grpcbox_stream:t()) -> grpcbox_stream:t().
-optional_callbacks([init/2, handle_info/2]).
