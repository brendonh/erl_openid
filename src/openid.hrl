%%%-------------------------------------------------------------------
%%% File    : openid.hrl
%%% Author  : Brendon Hogger <brendonh@dev.brendonh.org>
%%% Description : OpenID-related record definitions
%%%
%%% Created : 18 Sep 2009 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------

-record(xrdService, {
  types,
  uris,
  localID
}).

-record(xrds, {
  origID, 
  claimedID, 
  canonicalID, 
  isXRI, 
  services
}).

-record(authReq, {
   opURLs,
   version,
   claimedID=none,
   localID=none,
   assoc=none
}).

-record(assoc, {
  handle,
  created,
  expiresIn,
  servPublic,
  mac
}).
