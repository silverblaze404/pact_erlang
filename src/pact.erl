-module(pact).

-export([v4/2, create_interaction/2, verify_interaction/1]).

v4(Consumer, Producer) ->
    {ok, PactPid} = pact_handler:start_pact(Consumer, Producer),
    PactPid.

create_interaction(PactPid, Interaction) ->
    {Consumer, Producer} = pact_handler:get_consumer_producer(PactPid),
    PactRef = pact_ffi_helper:create_new_pact(Consumer, Producer),
    ok = pact_handler:set_pact_ref(PactPid, PactRef),
    GivenState = maps:get(upon_receiving, Interaction, <<"">>),
    InteractionRef = pact_ffi_helper:create_new_interaction(PactRef, GivenState),
    ok = pact_handler:create_interaction(PactPid, InteractionRef, Interaction),
    RequestDetails = maps:get(with_request, Interaction, #{}),
    ok = insert_request_details(InteractionRef, RequestDetails),
    ResponseDetails = maps:get(will_respond_with, Interaction ,#{}),
    ok = insert_response_details(InteractionRef, ResponseDetails),
    MockServerPort = pact_ffi_helper:create_mock_server_for_transport(
        PactRef, <<"127.0.0.1">>, 0, <<"http">>
    ),
    ok = pact_handler:set_mock_server_port(PactPid, MockServerPort),
    {ok, MockServerPort}.

verify_interaction(PactPid) ->
    PactRef = pact_handler:get_pact_ref(PactPid),
    MockServerPort = pact_handler:get_mock_server_port(PactPid),
    {ok, matched} = pact_ffi_helper:verify(MockServerPort),
    pact_ffi_helper:cleanup_pact(PactRef),
    ok = pact_ffi_helper:cleanup_mock_server(MockServerPort).

insert_request_details(InteractionRef, RequestDetails) ->
    ReqMethod = maps:get(method, RequestDetails),
    ReqPath = maps:get(path, RequestDetails),
    pact_ffi_helper:with_request(InteractionRef, ReqMethod, ReqPath),
    ReqHeaders = maps:get(headers, RequestDetails, undefined),
    case ReqHeaders of
        undefined -> ok;
        _ ->
            maps:fold(
                fun(Key, Value, _Acc) ->
                    pact_ffi_helper:with_request_header(InteractionRef, Key, 0, Value)
                end,
                ok,
                ReqHeaders
            )
    end,
    ReqBody = maps:get(body, RequestDetails, undefined),
    case ReqBody of
        undefined -> ok;
        _ ->
            pact_ffi_helper:with_request_body(InteractionRef, <<"application/json">>, ReqBody)
    end,
    ReqQueryParams = maps:get(query_params, RequestDetails, undefined),
    case ReqQueryParams of
        undefined -> ok;
        _ ->
            maps:fold(
                fun(Key, Value, _Acc) ->
                    pact_ffi_helper:with_query_parameter(InteractionRef, Key, 0, Value)
                end,
                ok,
                ReqQueryParams
            )
    end,
    ok.

insert_response_details(InteractionRef, ResponseDetails) ->
    ResponseStatusCode = maps:get(status, ResponseDetails, undefined),
    case ResponseStatusCode of
        undefined -> ok;
        _ ->
            pact_ffi_helper:with_response_status(InteractionRef, ResponseStatusCode)
    end,
    ResHeaders = maps:get(headers, ResponseDetails, undefined),
    case ResHeaders of
        undefined -> ok;
        _ ->
            maps:fold(
                fun(Key, Value, _Acc) ->
                    pact_ffi_helper:with_response_header(InteractionRef, Key, 0, Value)
                end,
                ok,
                ResHeaders
            )
    end,
    ResBody = maps:get(body, ResponseDetails, undefined),
    case ResBody of
        undefined -> ok;
        _ ->
            pact_ffi_helper:with_response_body(InteractionRef, <<"application/json">>, ResBody)
    end,
    ok.
