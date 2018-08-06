### Developer notes

Most of this information should be available on the tz docs site somewhere.
Ideally, when one builds the tezos documentation locally, all of this information
appears so we dont have to maintain another set ourselves.


### Sandbox testing

Testing rpc api needs a sandbox. Sandbox scripts are located here
  ```
  <tezos_install>/tezos/src/bin_node$ ls tezos-sandboxed-node.sh 
  ```

### Sandboxed node
This script as per the documentation here allows to launch a local closed test network with a maximum of 9 nodes.
```
<tezos_install>/tezos/src/bin_node$ ./tezos-sandboxed-node.sh 
Small script to launch local and closed test network with a maximum of 9 nodes.

Usage: ./tezos-sandboxed-node.sh <id>
  where <id> should be an integer between 1 and 9.
```

### Sandbox client node. 
Corresponding to the sandboxed node, one needs to setup a sandboxed client node and that script is here :
```
<tezos_install>/tezos/src/bin_client$ less tezos-init-sandboxed-client.sh
```
When we have completed setting up the sandboxed nodes, we can then run our client api applications to interact with the node.

