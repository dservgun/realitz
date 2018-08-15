### Build instructions
To build the project
  * Install [haxe](http://haxe.org)
  * Setup haxelib to install dependencies for the build.
  * Run
    ** ``` haxe build.hxml ``` to build the js/html5 target.
    ** ``` haxe build.java.hxml ``` to build java/android target. 



#### Dependencies
``` haxelib install json2object ```

#### Expert tips
Tips from the core dev team to help us navigate the api.

#### Some tips from an outsider looking into the code.

Code walkthrough process: it is simpler and probably intended that 
as implementers of client api, we assume types specified in the .mli (ml interface files). This ensures that during client implementations we inadvertently peek into server implementation. Files
that are a good place to start: 
  * ProtocolAlpha.hx -- The core protocol is the center of the diagram inside the shell.
  * BlockPrevalidator.hx -- 

Of course, everything has to work together therefore all of the files are important. However, reading these files helped me in getting to essence of the rpc api.
