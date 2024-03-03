defmodule Anoma.Types do
  use SerialisedStruct
  import Bitwise

  deftypes do
    proto_prim Bytes, binary(), :bytes, fn x -> x end, fn x -> x end
    proto_prim Bool, boolean(), :bool, fn x -> x end, fn x -> x end
    proto_prim U64, non_neg_integer(), :fixed64, fn x -> x end, fn x -> x end
    proto_prim U32, non_neg_integer(), :fixed32, fn x -> x end, fn x -> x end
    # no native 8/16 bit types
    proto_prim U16, non_neg_integer(), :uint32, fn x -> x end, fn x -> x end
    proto_prim U8,  non_neg_integer(), :uint32,  fn x -> x end, fn x -> x end
    # ad-hoc encoding of nock nouns pending a better one
    proto_prim SNoun, Noun.t(), :bytes, &:erlang.term_to_binary/1, &:erlang.binary_to_term/1
    proto Nat, non_neg_integer(), fn n -> %Nat{digits: Enum.reverse(Integer.digits(n, 1 <<< 32))} end, fn n -> Integer.undigits(Enum.reverse(n.digits), 1 <<< 32) end do
      field(:digits, :fixed32, :repeated)
    end
    proto Int, integer(), fn n -> %Int{sign: n < 0, magnitude: Nat.Helper.serialise(abs(n))} end, fn n -> Nat.Helper.deserialise(n.magnitude) * if n.sign do -1 else 1 end end do
      field(:sign, :bool)
      field(:magnitude, Nat)
    end

    # begin RM-related types not in specs yet
    proto DeltaEntry, DeltaEntry.t(), fn x -> x end, fn x -> x end do
      field(:kind, :bytes) # can't make this fixed size
      field(:value, :int64) # not sure why Int doesn't work here? todo
    end
    proto Delta, %{binary() => integer()}, fn map -> %Delta{entries: Enum.map(map, fn {kind, value} -> %DeltaEntry{kind: kind, value: value} end)} end, fn delta -> Map.new(Enum.map(delta.entries, fn %DeltaEntry{kind: kind, value: value} -> {kind, value} end)) end do
      field(:entries, DeltaEntry, :repeated)
    end

    struct MerkleAnchor do
      field(:hash, Bytes) # the hash of a merkle anchor
    end

    struct Resource do
      field(:logic, SNoun, default: [[1 | 0], 0 | 0]) # resource logic.  should be actual contents of the logic or a hash?
      field(:label, Bytes, default: <<>>) # fungibility label
      field(:quantity, Nat, default: 0)
      field(:data, Bytes, default: <<>>) # fungible data
      field(:eph, Bool, default: false) # ephemerality flag
      field(:nonce, Bytes) # 256 bits.  ensures uniqueness
      field(:npk, Bytes) # nullifier public key.  256 bits ed25519
      field(:rseed, Bytes) # 256 bits random key.  why can't this be the same as the nonce?
    end

    struct Proof do
      field(:resource, Resource) # in the transparent case, a proof is just a resource
    end

    struct ProofRecord do
      field(:proof, Proof)
    end

    struct Transaction do
      # https:#research.anoma.net/t/resource-consumption-ownership-and-authorization/422/2
      # perhaps we need to store a list of signed partial txes here?
      field(:roots, list(MerkleAnchor), default: []) # the anchors used by commitment proofs in this transaction
      field(:commitments, list(Bytes), default: []) # commitments to be created (currently just erlang binary encoded)
      field(:nullifiers, list(Bytes), default: []) # (ditto)
      field(:proofs, list(ProofRecord), default: []) # resource proofs
      field(:delta, Delta, default: %{})
      field(:extra, list(Bytes), default: [])
      # todo preference
    end

    # request all intents from the intent pool
    struct AllIntentsRequest do end
    struct AllIntentsResponse do
      field(:intents, list(Transaction))
    end

    # request to the intent pool to add an intent
    struct AddIntent do field(:intent, Transaction) end
    # " remove "
    struct RemoveIntent do field(:intent, Transaction) end
    # notification from the intent pool that an intent has been added
    struct IntentAdded do field(:intent, Transaction) end
    # " removed
    struct IntentRemoved do field(:intent, Transaction) end
    # new solution
    struct IntentSolution do field(:transaction, Transaction) end
    # end RM-related types not in specs yet

    struct Signature do
      field(:sig, Bytes)
    end

    struct Time do
      field(:unix_ns, Nat)
    end
    # ed25519 signing key; enacl 'box' (https://nacl.cr.yp.to/box.html) encryption
    # short name for concision
    struct BoxId do
      field(:verify, Bytes)
      field(:encrypt, Bytes)
    end
    union ExternalId do
      field(:box, BoxId)
    end
    struct EngineId do field(:id, ExternalId) end
    struct NodeId do field(:id, ExternalId) end
    struct TopicId do field(:id, ExternalId) end
    struct DomainId do field(:id, ExternalId) end
    union DestinationId do
      field(:engine, EngineId)
      field(:node, NodeId)
      field(:topic, TopicId)
    end
    struct BoxInternalId do
      field(:commit, Bytes)
      field(:decrypt, Bytes)
    end
    union InternalId do
      field(:box, BoxInternalId)
    end
    struct Id do
      field(:external, ExternalId)
      field(:internal, InternalId)
    end

    struct RoutingScope.Local do end
    struct RoutingScope.Domain do field(:id, DomainId) end
    struct RoutingScope.Any do end
    union RoutingScope do
      field(:local, RoutingScope.Local)
      field(:domain, RoutingScope.Domain)
      field(:any, RoutingScope.Any)
    end
    struct RoutingPrefs do
      field(:scope, RoutingScope)
    end

    struct TransportPrefs.Ordering.Ordered do end
    struct TransportPrefs.Ordering.Unordered do end
    union TransportPrefs.Ordering do
      field(:ordered, TransportPrefs.Ordering.Ordered)
      field(:unordered, TransportPrefs.Ordering.Unordered)
    end
    struct TransportPrefs.Reliability.Reliable do end
    struct TransportPrefs.Reliability.Unreliable do end
    union TransportPrefs.Reliability do
      field(:reliable, TransportPrefs.Reliability.Reliable)
      field(:unreliable, TransportPrefs.Reliability.Unreliable)
    end
    struct TransportPrefs.Security.Direct do end
    union TransportPrefs.Security do field(:direct, TransportPrefs.Security.Direct) end
    struct TransportPrefs do
      field(:ordering, TransportPrefs.Ordering)
      field(:reliability, TransportPrefs.Reliability)
      field(:security, TransportPrefs.Security)
    end

    struct ConnectionPrefs.Ephemeral do end
    struct ConnectionPrefs.Permanent do end
    union ConnectionPrefs do
      field(:ephemeral, ConnectionPrefs.Ephemeral)
      field(:permanent, ConnectionPrefs.Permanent)
    end

    struct QUICAddr do
      field(:ip, Bytes) # could have a more structured representation...
      field(:port, Nat)
      # cert issuer--not bothering with this for now...
    end
    union TransportAddress do
      field(:quic, QUICAddr)
    end
    struct EngineMessage.Signed do
      field(:msg, Bytes) # encoded EngineMessage; defined below because we have order-dependence--todo consider toposort in serialisedstruct to deal with that?
      field(:sig, optional(Signature))
    end
    struct RelayMessage.Signed do
      field(:msg, Bytes)
      field(:sig, Signature)
    end


    union P2PMessage.Contents do
      field(:engine, EngineMessage.Signed)
      field(:relay, RelayMessage.Signed)
    end
    struct P2PMessage do
      field(:src, NodeId)
      field(:dst, NodeId)
      field(:msg, P2PMessage.Contents)
    end
    struct P2PMessage.Signed do
      field(:msg, Bytes)
      field(:sig, Signature)
    end

    struct NodeTransportAddress do
      field(:address, TransportAddress)
      field(:node, NodeId)
    end
    struct TransportMessage do
      field(:addr, TransportAddress)
      field(:tprefs, optional(TransportPrefs))
      field(:expiry, optional(Time))
      field(:msg, P2PMessage)
    end
    struct ConnectedNodesRequest do end
    struct ConnectedNodesResponse do
      field(:nodes, list(NodeTransportAddress))
    end

    struct TopicAdvert do
      field(:topic, TopicId)
      field(:tags, list(Bytes))
      field(:publisher, EngineId)
      field(:created, Time)
    end
    struct TopicAdvert.Signed do
      field(:advert, Bytes)
      field(:sig, Signature)
    end
    struct TopicCreateRequest do
      field(:topic, TopicId)
      field(:scope, RoutingScope)
      field(:advert, optional(TopicAdvert))
    end
    struct TopicCreateResponse.Ok do end
    struct TopicCreateResponse.Error do end
    union TopicCreateResponse do
      field(:ok, TopicCreateResponse.Ok)
      field(:error, TopicCreateResponse.Error)
    end
    struct TopicDeleteRequest do
      field(:topic, TopicId)
      field(:scope, RoutingScope)
    end
    struct TopicDeleteResponse.Ok do end
    struct TopicDeleteResponse.Error do end
    union TopicDeleteResponse do
      field(:ok, TopicDeleteResponse.Ok)
      field(:error, TopicDeleteResponse.Error)
    end

    struct TopicSubRequest do
      field(:topic, TopicId)
      field(:scope, RoutingScope)
    end
    struct TopicSubResponse.Ok do end
    struct TopicSubResponse.Error do end
    union TopicSubResponse do
      field(:ok, TopicSubResponse.Ok)
      field(:error, TopicSubResponse.Error)
    end

    struct TopicUnsubRequest do
      field(:topic, TopicId)
      field(:scope, RoutingScope)
    end
    struct TopicUnsubResponse.Ok do end
    struct TopicUnsubResponse.Error do end
    union TopicUnsubResponse do
      field(:ok, TopicUnsubResponse.Ok)
      field(:error, TopicUnsubResponse.Error)
    end

    struct TrustValue do field(:trust, U8) end
    struct ReputationValue do field(:reputation, U8) end
    struct LookupIdentityRequest do
      field(:id, ExternalId)
    end
    struct NodeAdvert do
      field(:id, NodeId)
      field(:addrs, list(TransportAddress)) # no preference--implicit in order
      field(:version, U32)
      field(:created, Time)
    end
    struct NodeIdentityRecord do
      field(:id, NodeId)
      field(:advert, optional(NodeAdvert))
      field(:trust, optional(TrustValue))
      field(:reputation, optional(ReputationValue))
      field(:tprefs, optional(TransportPrefs))
      field(:cprefs, optional(ConnectionPrefs))
    end
    struct TopicIdentityRecord do
      field(:id, TopicId)
      field(:advert, TopicAdvert.Signed)
    end
    struct DomainIdentityRecord do
      field(:id, DomainId)
      #advert
    end
    struct EngineIdentityRecord do
      field(:id, EngineId)
      #engine address
    end
    union LookupIdentityResponse do
      field(:engine, EngineIdentityRecord)
      field(:node, NodeIdentityRecord)
      field(:domain, DomainIdentityRecord)
      field(:topic, TopicIdentityRecord)
    end

    struct NodeConnected do field(:node, NodeTransportAddress) end
    struct NodeConnectFailed do field(:node, NodeTransportAddress) end
    struct NodeDisconnected do field(:node, NodeTransportAddress) end


    # big ol' union of all Messages...
    # (should abstract so we just say 'message Foo do ...')
    union Message do
      field(:AllIntentsRequest, AllIntentsRequest)
      field(:AllIntentsResponse, AllIntentsResponse)
      field(:AddIntent, AddIntent)
      field(:RemoveIntent, RemoveIntent)
      field(:IntentAdded, IntentAdded)
      field(:IntentRemoved, IntentRemoved)
      field(:IntentSolution, IntentSolution)
      field(:NodeConnected, NodeConnected)
      field(:NodeConnectFailed, NodeConnectFailed)
      field(:NodeDisconnected, NodeDisconnected)
      field(:LookupIdentityRequest, LookupIdentityRequest)
      field(:LookupIdentityResponse, LookupIdentityResponse)
    end
    struct RelayMessage do
      field(:src, NodeId)
      field(:dst, NodeId)
      field(:tprefs, optional(TransportPrefs))
      field(:expiry, optional(Time))
      field(:msg, Bytes)
    end
    struct EngineMessage do
      field(:src, EngineId)
      field(:dst, DestinationId)
      field(:id, U64)
      field(:reply_to, optional(U64))
      field(:expiry, optional(Time))
      field(:rprefs, optional(RoutingPrefs))
      field(:tprefs, optional(TransportPrefs))
      # protocol
      field(:body, Message) # not currently encoded at this level
    end
  end
end
