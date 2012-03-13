unit uNetworkConst;

interface

const
  NETWORK_VERB = $FF00;
  NETWORK_VERB_HANDSHAKE = NETWORK_VERB + $01;              // {s:ProtocolName}
  NETWORK_VERB_HSERROR = NETWORK_VERB + $02;                // {s:ExpectedProtocolName}
  NETWORK_VERB_STATEERROR = NETWORK_VERB + $03;             //
  // Server -> Client
  NETWORK_VERB_ENTITY_ADD = NETWORK_VERB + $11;             // {c:EntityID} {c:EntityClass}
  NETWORK_VERB_ENTITY_REMOVE = NETWORK_VERB + $12;          // {c:EntityID}
  NETWORK_VERB_ENTITY_MESSAGE = NETWORK_VERB + $13;         // {c:EntityID} {w:Method} {...}
  NETWORK_VERB_STATE_TRANSITION = NETWORK_VERB + $14;       // {b:NewState}
  // Client -> Server
  NETWORK_VERB_ENTITY_CALL = NETWORK_VERB + $22;            // {c:EntityID} {w:Method} {...}

implementation

end.
 
