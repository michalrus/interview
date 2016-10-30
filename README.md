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

## Assumptions

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

Printing all the log messages takes a considerable amount of time (at
least to my terminal emulator), so the provided `launch-script`
filters most of those:

```
% ./launch-script
Missing: --send-for SECONDS --wait-for SECONDS --with-seed SEED

Usage: interview-exe --send-for SECONDS --wait-for SECONDS --with-seed SEED

% ./launch-script --send-for 5 --wait-for 1 --with-seed 1
(6104,9269702.655930057)
(6108,9273478.13697705)
Fri Oct 28 20:01:18 UTC 2016 pid://127.0.0.6:10501:0:10: got StopSending!
Fri Oct 28 20:01:18 UTC 2016 pid://127.0.0.6:10501:0:10: pre-grace: I’ve got 6104 messages so far.
Fri Oct 28 20:01:18 UTC 2016 pid://127.0.0.6:10501:0:10: peri-grace: I’ve got 6108 messages in the final state.
Fri Oct 28 20:01:18 UTC 2016 pid://127.0.0.7:10501:0:10: got StopSending!
Fri Oct 28 20:01:18 UTC 2016 pid://127.0.0.7:10501:0:10: pre-grace: I’ve got 6103 messages so far.
Fri Oct 28 20:01:18 UTC 2016 pid://127.0.0.7:10501:0:10: peri-grace: I’ve got 6104 messages in the final state.
Fri Oct 28 20:01:18 UTC 2016 pid://127.0.0.4:10501:0:10: got StopSending!
Fri Oct 28 20:01:18 UTC 2016 pid://127.0.0.4:10501:0:10: pre-grace: I’ve got 119308 messages so far.
(6199,9573308.058435662)
(13266,4.370715770854487e7)
Fri Oct 28 20:01:18 UTC 2016 pid://127.0.0.2:10501:0:10: got StopSending!
Fri Oct 28 20:01:18 UTC 2016 pid://127.0.0.2:10501:0:10: pre-grace: I’ve got 6191 messages so far.
Fri Oct 28 20:01:18 UTC 2016 pid://127.0.0.2:10501:0:10: peri-grace: I’ve got 6199 messages in the final state.
Fri Oct 28 20:01:18 UTC 2016 pid://127.0.0.4:10501:0:10: peri-grace: I’ve got 119318 messages in the final state.
Fri Oct 28 20:01:18 UTC 2016 pid://127.0.0.10:10501:0:10: got StopSending!
(4055,4097041.4055770617)
(6227,9635836.800797215)
(8099,1.6305006353892066e7)
(119318,3.5657791386828623e9)
(10482,2.7491442352444764e7)
Fri Oct 28 20:01:18 UTC 2016 pid://127.0.0.10:10501:0:10: pre-grace: I’ve got 13263 messages so far.
Fri Oct 28 20:01:18 UTC 2016 pid://127.0.0.10:10501:0:10: peri-grace: I’ve got 13266 messages in the final state.
(23924,1.4224226456514654e8)
```

Some of the `stderr` was clearly lost, however all 10 expected tuples
were written to `stdout`.

The `launch-script` can be passed a `--debug` flag as its first
argument. Then, it filters the `stderr` less, runs tests and adds some
additional timestamps.

```
% ./launch-script --debug --send-for 5 --wait-for 1 --with-seed 1
2016-10-28T22:17:15,446468806+02:00 running ‘stack test’…

interview-0.1.0.0: test (suite: interview-test)

[…]

2016-10-28T22:17:18,179079612+02:00 running ‘stack exec’…

Fri Oct 28 20:17:19 UTC 2016 pid://127.0.0.3:10501:0:10: my initialPeers (of length 2) = [nid://127.0.0.1:10501:0,nid://127.0.0.3:10501:0]
Fri Oct 28 20:17:19 UTC 2016 pid://127.0.0.1:10501:0:10: my initialPeers (of length 2) = [nid://127.0.0.1:10501:0,nid://127.0.0.3:10501:0]
Fri Oct 28 20:17:19 UTC 2016 pid://127.0.0.10:10501:0:10: my initialPeers (of length 2) = [nid://127.0.0.1:10501:0,nid://127.0.0.3:10501:0]
Fri Oct 28 20:17:19 UTC 2016 pid://127.0.0.8:10501:0:10: my initialPeers (of length 6) = [nid://127.0.0.1:10501:0,nid://127.0.0.2:10501:0,nid://127.0.0.3:10501:0,nid://127.0.0.6:10501:0,nid://127.0.0.7:10501:0,nid://127.0.0.8:10501:0]
Fri Oct 28 20:17:19 UTC 2016 pid://127.0.0.2:10501:0:10: my initialPeers (of length 7) = [nid://127.0.0.1:10501:0,nid://127.0.0.2:10501:0,nid://127.0.0.3:10501:0,nid://127.0.0.5:10501:0,nid://127.0.0.6:10501:0,nid://127.0.0.7:10501:0,nid://127.0.0.8:10501:0]
Fri Oct 28 20:17:19 UTC 2016 pid://127.0.0.7:10501:0:10: my initialPeers (of length 5) = [nid://127.0.0.1:10501:0,nid://127.0.0.2:10501:0,nid://127.0.0.3:10501:0,nid://127.0.0.7:10501:0,nid://127.0.0.8:10501:0]
Fri Oct 28 20:17:19 UTC 2016 pid://127.0.0.6:10501:0:10: my initialPeers (of length 6) = [nid://127.0.0.1:10501:0,nid://127.0.0.2:10501:0,nid://127.0.0.3:10501:0,nid://127.0.0.6:10501:0,nid://127.0.0.7:10501:0,nid://127.0.0.8:10501:0]
Fri Oct 28 20:17:19 UTC 2016 pid://127.0.0.5:10501:0:10: my initialPeers (of length 7) = [nid://127.0.0.1:10501:0,nid://127.0.0.2:10501:0,nid://127.0.0.3:10501:0,nid://127.0.0.5:10501:0,nid://127.0.0.6:10501:0,nid://127.0.0.7:10501:0,nid://127.0.0.8:10501:0]
Fri Oct 28 20:17:19 UTC 2016 pid://127.0.0.4:10501:0:10: my initialPeers (of length 10) = [nid://127.0.0.10:10501:0,nid://127.0.0.1:10501:0,nid://127.0.0.2:10501:0,nid://127.0.0.3:10501:0,nid://127.0.0.4:10501:0,nid://127.0.0.5:10501:0,nid://127.0.0.6:10501:0,nid://127.0.0.7:10501:0,nid://127.0.0.8:10501:0,nid://127.0.0.9:10501:0]
Fri Oct 28 20:17:19 UTC 2016 pid://127.0.0.9:10501:0:10: my initialPeers (of length 9) = [nid://127.0.0.10:10501:0,nid://127.0.0.1:10501:0,nid://127.0.0.2:10501:0,nid://127.0.0.3:10501:0,nid://127.0.0.5:10501:0,nid://127.0.0.6:10501:0,nid://127.0.0.7:10501:0,nid://127.0.0.8:10501:0,nid://127.0.0.9:10501:0]
(5885,8530184.51326529)
(9509,2.2505299591975145e7)
(4396,4871215.0594848925)
(13878,4.8439581768553756e7)
Fri Oct 28 20:17:24 UTC 2016 pid://127.0.0.10:10501:0:10: got StopSending!
Fri Oct 28 20:17:24 UTC 2016 pid://127.0.0.10:10501:0:10: pre-grace: I’ve got 9509 messages so far.
Fri Oct 28 20:17:24 UTC 2016 pid://127.0.0.10:10501:0:10: peri-grace: I’ve got 9509 messages in the final state.
Fri Oct 28 20:17:24 UTC 2016 pid://127.0.0.1:10501:0:10: got StopSending!
Fri Oct 28 20:17:24 UTC 2016 pid://127.0.0.1:10501:0:10: pre-grace: I’ve got 28783 messages so far.
Fri Oct 28 20:17:24 UTC 2016 pid://127.0.0.1:10501:0:10: peri-grace: I’ve got 28784 messages in the final state.
Fri Oct 28 20:17:24 UTC 2016 pid://127.0.0.6:10501:0:10: got StopSending!
Fri Oct 28 20:17:24 UTC 2016 pid://127.0.0.6:10501:0:10: pre-grace: I’ve got 13876 messages so far.
Fri Oct 28 20:17:24 UTC 2016 pid://127.0.0.6:10501:0:10: peri-grace: I’ve got 13878 messages in the final state.
Fri Oct 28 20:17:24 UTC 2016 pid://127.0.0.4:10501:0:10: got StopSending!
Fri Oct 28 20:17:24 UTC 2016 pid://127.0.0.4:10501:0:10: pre-grace: I’ve got 5885 messages so far.
(28784,2.069931099859176e8)
Fri Oct 28 20:17:24 UTC 2016 pid://127.0.0.4:10501:0:10: peri-grace: I’ve got 5885 messages in the final state.
Fri Oct 28 20:17:24 UTC 2016 pid://127.0.0.5:10501:0:10: got StopSending!
(9537,2.264057402684156e7)
Fri Oct 28 20:17:24 UTC 2016 pid://127.0.0.2:10501:0:10: got StopSending!
Fri Oct 28 20:17:24 UTC 2016 pid://127.0.0.2:10501:0:10: pre-grace: I’ve got 4376 messages so far.
(14503,5.24668661509739e7)
(9589,2.312385528184515e7)
Fri Oct 28 20:17:24 UTC 2016 pid://127.0.0.2:10501:0:10: peri-grace: I’ve got 4396 messages in the final state.
Fri Oct 28 20:17:24 UTC 2016 pid://127.0.0.3:10501:0:10: got StopSending!
Fri Oct 28 20:17:24 UTC 2016 pid://127.0.0.3:10501:0:10: pre-grace: I’ve got 14486 messages so far.
Fri Oct 28 20:17:24 UTC 2016 pid://127.0.0.3:10501:0:10: peri-grace: I’ve got 14503 messages in the final state.
Fri Oct 28 20:17:24 UTC 2016 pid://127.0.0.9:10501:0:10: got StopSending!
(23360,1.36752700898935e8)
Fri Oct 28 20:17:24 UTC 2016 pid://127.0.0.9:10501:0:10: pre-grace: I’ve got 9504 messages so far.
Fri Oct 28 20:17:24 UTC 2016 pid://127.0.0.9:10501:0:10: peri-grace: I’ve got 9537 messages in the final state.
Fri Oct 28 20:17:24 UTC 2016 pid://127.0.0.5:10501:0:10: pre-grace: I’ve got 23357 messages so far.
Fri Oct 28 20:17:24 UTC 2016 pid://127.0.0.5:10501:0:10: peri-grace: I’ve got 23360 messages in the final state.
Fri Oct 28 20:17:24 UTC 2016 pid://127.0.0.7:10501:0:10: got StopSending!
Fri Oct 28 20:17:24 UTC 2016 pid://127.0.0.7:10501:0:10: pre-grace: I’ve got 9572 messages so far.
Fri Oct 28 20:17:24 UTC 2016 pid://127.0.0.7:10501:0:10: peri-grace: I’ve got 9589 messages in the final state.
Fri Oct 28 20:17:24 UTC 2016 pid://127.0.0.8:10501:0:10: got StopSending!
Fri Oct 28 20:17:24 UTC 2016 pid://127.0.0.8:10501:0:10: pre-grace: I’ve got 17783 messages so far.
Fri Oct 28 20:17:24 UTC 2016 pid://127.0.0.8:10501:0:10: peri-grace: I’ve got 17787 messages in the final state.
(17787,7.913895842149054e7)

2016-10-28T22:17:25,031743287+02:00 process exited with 0
```

The timing seems pretty correct (starting at `my initialPeers` and
ending at `got StopSending`—5 seconds that were requested).

**All in all, it’s best to run `stack exec interview-exe` and grep the
output to your liking.** Even better: run the executable directly,
bypassing stack’s startup overhead. Then, `/bin/time` could even be
used to get some useful information.

---

## Possible improvements

* It’s very tempting to calculate the tuples on the
  fly. Unfortunately, they have to be sorted on time sent and not
  received. What could be done, though, is keeping only last 500 ms
  (tuneable) in memory and rejecting any latecomers in the final
  result.

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

  * check if the UDP multicast is a reliable one.

* Cosmetic changes:

  * use **real** time types, not `Int`s in `Configuration`,
    `MainProcessConfig` and everywhere else. Using `Int`s is asking
    for trouble. Intly-typed programming is as bad as stringly-typed…
    (I’d have to dive into `optparse-applicative` deeper than just
    scanning the docs),

  * turn off the `say` logger in tests,

  * various `FIXME`s in the code, using the State monad, Data.Set
    instead of List etc.

  * add code auto-formatter and Unicode symbols,

  * the artificial shared logger process and node are sometimes dying
    before they flush all of their messages to stderr,

  * somehow get µs in logger timestamps,

  * maybe making `Runner.killer` not a Process, but an IO thread,
    would be more correct (as it’s meant to kill not just a
    process/node, but the whole application),

  * add pretty pictures to this README.md from Wikipedia? =)

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
