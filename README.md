# interview

## The main challenge

Currently, losing connectivity with the cluster is not recognized.
This information is needed for peers to know when to try to re-join
once more.  How this can be solved (it has to be solved prior to final
grading of the solution):

1. We could assume that if a peer has not sent a message during the
   previous 500 ms (needs experimental tuning), then the connection
   has been dropped. (Taking into account the characteristics of this
   traffic; normally, a keep-alive message would be necessary).

1. More important: **how should I be testing such a scenario locally,
   on my notebook?**

## Decisions

* This is 0th iteration. Apart from the main problem above, I’ve
  written down [what could/should be improved at the very
  end](#possible-improvements). What else should be improved?

* I’ve rejected the master/slave model from Control.Distributed,
  because I don’t have any information at all on what will fail. It
  might be safest to assume that failure of each node is equally
  probable. And then, having nodes with equal responsibilities seems
  more reliable. (This needs testing).

* A joining node has to know at least one neighbor initially:

  * for now, I’ve chosen UDP multicast with relatively low discovery
    timeout,

  * alternatively, the first IP might be hardcoded (or even a /24
    network and the node might try to open 254 connections at once,
    when joining?),

  * there could be some central tracking server(s) (like in BT), but
    that’s a SPOF.

* I’ve decided that each peer will be brodcasting their *peer lists*
  to their *peers*. We’re already broadcasting all real messages in
  this way, and this is way more chatty (and suggests that the whole
  network is not huge).

* Unfortunately, we’re sorting messages on their sent time (and not
  received time), so—apart from the payload—we have to remember
  sent/creation time of each real message.

* Because this initial version is using UDP multicast, it needs to be
  unblocked on system’s firewall. E.g., on my machine (NixOS):

  ```nix
  {
    networking.firewall.extraCommands = ''
      # Enable multicast.
      iptables -A nixos-fw -d 224.0.0.0/4 -j nixos-fw-accept
    '';
  }
  ```

## Flow

1. New node is turned on. It starts by sending a UDP multicast
   packet. Hopefully, some neighbors receive the packet and respond
   with their addresses. (Alternatively, the list of nodes can be
   provided in [Node configuration](#node-configuration)).

1. This new node’s initial list of peers is populated with neighbors
   found through multicast.  A message containing this list is sent to
   each node on this list.

1. Neighbors reply with lists of their peers.

  * When processing a received list of peers, the receiving node adds
    them to its list (no duplicates).

  * If its knowledge of surroundings increased thanks to that
    operation, the node notifies all of its peers (including new
    peers), by sending them the knowledge (the merged list of old and
    new peers).

1. Regular/real messages are being constantly sent to all peers from
   the list.

## Node configuration

1. Sample configuration is in `lib/Nodes.hs`.

1. You have to provide `runOnNodes :: [[NodeId] -> Process ()] -> IO
   ()` to `Runner.run`. This function takes a (potentially) infinite
   list of functions from an *initial peer list* to *processes* and
   runs them on `Nodes.thisManyNodes` nodes. Of course, `runOnNodes`
   could also use a hardcoded list of nodes, as mentioned in the spec.

1. With current setting of `Nodes.initialPeerDiscoveryTimeout` at 50
   µs, not all peers get discovered initially (when running on
   loopback interfaces). This is positive → the discovery algorithm
   can be tested.

1. Currently, there’s an artificial logger node to counteract
   interleaving of the log messages printed to stderr. More about that
   in `lib/Nodes.hs`.

## Example

Printing all the log messages takes considerably more time (at least
to my terminal emulator), so here is the output with final tuples and
grace-period-related messages only:

```
% alias tss='ts "%Y-%m-%d %H:%M:%.S"$'\''\t'\'

% ( date ; \
    ./interview-exe --send-for 6 --wait-for 10 --with-seed 10503 2>&1 | grep -E 'grace|^\(' ; \
    date ; ) | tss

2016-10-21 21:44:12.962408       Fri Oct 21 21:44:12 CEST 2016
2016-10-21 21:44:19.372643       Fri Oct 21 19:44:18 UTC 2016 pid://127.0.0.8:10501:0:10: pre-grace: I’ve got 3712 messages so far.
2016-10-21 21:44:19.372763       Fri Oct 21 19:44:18 UTC 2016 pid://127.0.0.8:10501:0:10: peri-grace: I’ve got 3712 messages in the final state.
2016-10-21 21:44:19.372809       Fri Oct 21 19:44:18 UTC 2016 pid://127.0.0.4:10501:0:10: pre-grace: I’ve got 17598 messages so far.
2016-10-21 21:44:19.372852       Fri Oct 21 19:44:18 UTC 2016 pid://127.0.0.4:10501:0:10: peri-grace: I’ve got 17601 messages in the final state.
2016-10-21 21:44:19.372895       Fri Oct 21 19:44:18 UTC 2016 pid://127.0.0.9:10501:0:10: pre-grace: I’ve got 11073 messages so far.
2016-10-21 21:44:19.372941       Fri Oct 21 19:44:18 UTC 2016 pid://127.0.0.10:10501:0:10: pre-grace: I’ve got 2868 messages so far.
2016-10-21 21:44:19.372983       Fri Oct 21 19:44:18 UTC 2016 pid://127.0.0.10:10501:0:10: peri-grace: I’ve got 2868 messages in the final state.
2016-10-21 21:44:19.373026       Fri Oct 21 19:44:18 UTC 2016 pid://127.0.0.5:10501:0:10: pre-grace: I’ve got 27780 messages so far.
2016-10-21 21:44:19.373067       Fri Oct 21 19:44:18 UTC 2016 pid://127.0.0.5:10501:0:10: peri-grace: I’ve got 27784 messages in the final state.
2016-10-21 21:44:19.373109       Fri Oct 21 19:44:18 UTC 2016 pid://127.0.0.1:10501:0:10: pre-grace: I’ve got 1563 messages so far.
2016-10-21 21:44:19.373151       Fri Oct 21 19:44:18 UTC 2016 pid://127.0.0.1:10501:0:10: peri-grace: I’ve got 1570 messages in the final state.
2016-10-21 21:44:19.373198       Fri Oct 21 19:44:18 UTC 2016 pid://127.0.0.3:10501:0:10: pre-grace: I’ve got 8419 messages so far.
2016-10-21 21:44:19.373261       Fri Oct 21 19:44:18 UTC 2016 pid://127.0.0.3:10501:0:10: peri-grace: I’ve got 8419 messages in the final state.
2016-10-21 21:44:19.373299       Fri Oct 21 19:44:18 UTC 2016 pid://127.0.0.9:10501:0:10: peri-grace: I’ve got 11082 messages in the final state.
2016-10-21 21:44:19.373337       Fri Oct 21 19:44:19 UTC 2016 pid://127.0.0.6:10501:0:10: pre-grace: I’ve got 11708 messages so far.
2016-10-21 21:44:19.373373       Fri Oct 21 19:44:19 UTC 2016 pid://127.0.0.6:10501:0:10: peri-grace: I’ve got 11969 messages in the final state.
2016-10-21 21:44:19.373412       (8419,1.771552671510441e7)
2016-10-21 21:44:19.373449       (17601,7.778375861769518e7)
2016-10-21 21:44:19.373487       (2868,2055313.1252860774)
2016-10-21 21:44:19.373523       (11082,3.083165491805066e7)
2016-10-21 21:44:19.373554       (1570,598713.076066068)
2016-10-21 21:44:19.373585       (27784,1.9284630421839693e8)
2016-10-21 21:44:19.373618       (11969,3.590190847309941e7)
2016-10-21 21:44:19.373651       (3312,2739929.116742483)
2016-10-21 21:44:19.373684       (18351,8.431254444565432e7)
2016-10-21 21:44:19.374864       Fri Oct 21 21:44:19 CEST 2016

```

As can be seen in the first column, the timing is quite correct.

---

## Possible improvements

* It’s very tempting to calculate the tuples on the fly. Unfortunately,
  they have to be sorted on time sent and not received. What could be
  done, though, is keeping only last 500 ms in memory and rejecting
  any latecomers in the final result.

* Instead of sorting the *m* list at the very end, insertions could be
  used.  (However, it’s worth noting, that the *m* list is more/less
  well sorted already—however, reversed).

* Typed channels should be used for communication. Related: using
  `nsendRemote` (to treat a node more/less like a process) seems
  smelly.

* Regarding node discovery:

  * [the first paragraph of this README](#the-main-challenge),

  * read through the code of– and test `distributed-process-p2p` for
    this application,

  * the network is pretty talkative, during topology changes — could
    we use some kind of a flooding algorithm?

  * experimental tuning of `Nodes.initialPeerDiscoveryTimeout` for the
    UDP multicast. For this application, a function more like
    `findOnePeer :: Int → IO (Maybe NodeId)` might turn out
    better. Perhaps `findPeers` should return a Stream and not a List?

  * maybe we could be sending diffs of peer lists? This would
    complicate the algorithm, though;

  * maybe sent time could somehow be used when broadcasting the peer
    lists,

  * current discovery algorithm is solely additive—disconnected nodes
    are not removed form peer lists, and that loses CPU time on
    sending messages to them — related to [the first paragraph of this
    README](#the-main-challenge),

  * research different cluster node discovery algorithms in published
    papers; I haven’t done that, wanting to give you an initial
    version of the solution as soon as possible; most of the time was
    spent on learning about Cloud Haskell, instead of on researching,
    thinking and implementation; I’ve assumed that the current naïve
    algorithm might suffice as a first version of this solution;

  * perhaps check if parallelization of message sending (within a
    peer, to its peers) is more beneficial… but I doubt that, as
    sending messages is asynchronous anyway,

  * check if the UDP multicat is a reliable one,

  * maybe add another multicast during the clusters normal lifetime
    (when topology is not changing), just in case?

* Cosmetic changes:

  * add pretty pictures to this README.md from Wikipedia? =)

  * various `FIXME`s in the code, using the State monad, Data.Set
    instead of List etc.

  * add code auto-formatter and Unicode symbols,

  * keep real time types, not `Int`s in `Runner.Configuration`, I’d
    have to dive into `optparse-applicative` deeper than just scanning
    the docs,

  * the artificial shared logger process and node are sometimes dying
    before they flush all of their messages to stderr,

  * somehow get µs in logger timestamps.

* Probably, a Prelude should be used that is saner than the default
  one. `ClassyPrelude`, perhaps?

* Potential `Exception`s in processes are not handled. However, I’m
  not using them for flow control myself, they could happen during
  setting up transports etc.

* Automatic tests, but I need more specs to do that — related to [the
  first paragraph of this README](#the-main-challenge),

* check how costly it is to ask for system time… `Runner.childSender`
  does it for every message it sends.  Maybe it could be checked every
  n-th message? Also: how costly it is to do calculations on
  `UTCTime`?

* we’re relying on sent times. In this case it’s not that important
  (see the formula for the final tuples), but if it were, we’d have to
  make sure that nodes have their system times nicely in sync.

## Summary

I hope you’ll forgive me that his initial version took so long. As I
said, I had to familiarize myself with Cloud Haskell, at the same time
helping around the house. Before, I’ve only used Akka, but CH is
**way** better and more intuitive. :heart:
