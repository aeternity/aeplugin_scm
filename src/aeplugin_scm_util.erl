-module(aeplugin_scm_util).

-export([ abbrev_key/1,
          split_words/1 ]).

-export([ channel_opts/3 ]).

abbrev_key(K) when is_binary(K) ->
    <<(binary:part(K, {0,8}))/binary,
      "...",
      (binary:part(K, {byte_size(K),-4}))/binary>>.

split_words(Str) ->
    re:split(Str, <<"\\s*,\\s*">>, []).

channel_opts(Role, #{pubkey := PubKey} = KeyPair, Peer) ->
    Opts0 = channel_opts_(KeyPair),
    case Role of
        initiator -> Opts0#{ initiator => PubKey
                           , responder => Peer };
        responder -> Opts0#{ responder => PubKey
                           , initiator => Peer }
    end.

channel_opts_(KeyPair) ->
    #{ sign => KeyPair
     , channel_reserve => 100000000000000
     , initiator_amount => 1000000000000000
     , lock_period => 10
     , minimum_depth => 0
     , noise => [{noise,<<"Noise_NN_25519_ChaChaPoly_BLAKE2b">>}]
     , push_amount => 0
     , responder_amount => 1000000000000000
     , msg_forwarding => true }.
