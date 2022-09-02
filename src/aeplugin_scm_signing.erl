-module(aeplugin_scm_signing).

-export([ verify_signing_config/1
	, sign_tx/2 ]).

sign_tx(SignedTx, #{sign := Sign}) ->
    Tx = aetx_sign:innermost_tx(SignedTx),
    [NewSignature] = aetx_sign:signatures(do_sign_tx(Tx, Sign)),
    add_signature_to_innermost_signed_tx(SignedTx, NewSignature).

do_sign_tx(Tx, Sign) ->
    Bin0 = aetx:serialize_to_binary(Tx),
    BinForNetwork = aec_governance:add_network_id(Bin0),
    Signatures = [Sign(BinForNetwork)],
    aetx_sign:new(Tx, Signatures).

add_signature_to_innermost_signed_tx(TopSignedTx, Signature) ->
    modify_innermost_tx(TopSignedTx,
        fun(SignedTx) ->
            aetx_sign:add_signatures(SignedTx, [Signature])
        end).

modify_innermost_tx(SignedTx, ModFun) ->
    case aetx:specialize_callback(aetx_sign:tx(SignedTx)) of % topmost tx
        {aega_meta_tx, Meta} ->
            NewInnerTx =
                % recursively go to inner layers until the innermost tx is
                % reached so its signed transaction is updated
                % note: not tail recursive
                modify_innermost_tx(aega_meta_tx:tx(Meta), ModFun),
            UpdatedTx = aega_meta_tx:set_tx(NewInnerTx, Meta),
            aetx_sign:new(aetx:new(aega_meta_tx, UpdatedTx), []);
        {_, _InnerMostTx} ->
            ModFun(SignedTx)
    end.

verify_signing_config(#{privkey := PrivKey}) ->
    fun(Bin) ->
            enacl:sign_detached(Bin, PrivKey)
    end.
