![alt text](https://github.com/stephb9959/mqttsim/images/owls.jpg?raw=true)
# OWLS (Open WiFi Load Simulator)
Erlang based MQTT simulator

## Getting started
### Pre-requisites
You must install Erlang OTP 22 or newer.
#### Linux 
##### Ubuntu
```
sudo apt install erlang
```
##### Linux downloads
Please visit https://www.erlang-solutions.com/resources/download.html in order to get instructions for 
other Linux distributions and operating systems.

#### OS X
```
brew install erlang 
```

#### From source
Please visit https://erlang.org/doc/installation_guide/INSTALL.html to build Erlang from scratch.

#### Windows
The systeme currently does not support Windows

### Verifying if Erlang is available
From the command line, simply type 
```
prompt > erl
```
Your should see something like this
```
Erlang/OTP 23 [erts-11.1.1] [source] [64-bit] [smp:16:16] [ds:16:16:10] [async-threads:1] [hipe] [dtrace]

Eshell V11.1.1  (abort with ^G)
1>
```
 To exit, enter `q().`, like this
 ```
 Erlang/OTP 23 [erts-11.1.1] [source] [64-bit] [smp:16:16] [ds:16:16:10] [async-threads:1] [hipe] [dtrace]

Eshell V11.1.1  (abort with ^G)
1> q().
ok
2>                                                                                
prompt >
```
### Compiling the code
You need to clone the repository, run a configuration command, and start doing the simulation

```
git clone https://github.com/stephb9959/mqttsim
cd mqttsim
```

### Choosing a node type
In a simulation, you have 2 types of nodes. 

#### simmanager node
There is only one 'simmanager' node. This node is responsible for directing all the other nodes in the simulation. It is a 
central management point and gathers all the data about the simulation. You will be interacting with the 'simmanager' through a 
command line interface or a web UI. If you wish to start a 'simmanager' node, you should do the following:

```
./simmanager_config `pwd`
./simnanager
```

#### simnode node
You can have multiple 'simnode' nodes. Each of these nodes can be started on the same host, or a number of other virtual or physical machines.
Once a `simnode` is running, you will be able to monitor it trough a command line interface or a local web UI. If you wish to start a `simnode`, please 
follow these instructions. 

```
./simnode_config `pwd`
./simnode
```

#### On running multiple node types on a single machine
If you wish to run multiple nodes on a single host, you should run this from multiple copies of the repository code. 

```
mkdir ~/owls
cd ~/owls
mkdir simnode1
cd simnode1
git clone https://github.com/stephb9959/mqttsim
cd mqttsim
./simnode_config `pwd`
./simnode
```

in another terminal window

```
cd ~/owls
mkdir simnode2
cd simnode2
git clone https://github.com/stephb9959/mqttsim
cd mqttsim
./simnode_config `pwd`
./simnode
```

Before starting the node, you must go and change the hostname and port for each node. On a `simnode`, you must change
the `-name` parameter in the `config/simnode.args` file. Each node must have a unique name. You must also change the `web_ui` port 
in the `config/simnode.config` file to a unique port.  

### Basic configuration
#### Hostname
The file `config/simmanager.args` or `config/simnode.args` contains the only value you must change. Near the op pf the 
file, you will find the following commands:
```
-name simmanager@renegademac.local
```
You must change that value. This is NOT an email address. The first part is used to locate the VM on a given host. `sim1` or 
something like that is good. Next, change the hostname portion of that entry to the hostname where you are running the simulator. 
This must be an FQDN (Fully Qualified Domain Name). This just means that the hostname has to contain at least 1 period. Do not use `localhost`. 
Usually you can use your PC name followed by `.local`. You can try to `ping` that name to see if your PC can find it. This step is critical. 
If all fails, enter something in your `/etc/hosts` file.
#### Network cookie
```
-setcookie oreo
```
You can change the cookie for erlang distribution. Whatever value you pick, you will need to enter the same value on all the additional hosts 
that will participate in this simulation. In the case, replace `oreo` with your favorite password. Please note that this simulation is not meant 
to run accross the internet and is expected to run behind firewalls. Security is beyond the scope of this project.
#### Custom CA configuration
Once you run `simmanager_config` or `simnode_config`, you will get a customized configuration file located in the config directory.

 ## Starting the simulator
 A quick run file has been added in order to provide a playground for you. From the command line simply enter
 ```
 prompt> ./r
heart_beat_kill_pid = 44603
Erlang/OTP 23 [erts-11.1.1] [source] [64-bit] [smp:16:16] [ds:16:16:10] [async-threads:5] [hipe] [dtrace]

Eshell V11.1.1  (abort with ^G)
(mqttsim@renegademac.local)1> 
```
This is what you should be able to see. If you do not see something similar, something went wrong and get in touch
with the community to help you troubleshoot the problem. 

## API
This project uses OpenAPI specification 3.0, and you can use Swagger (https://editor.swagger.io/) in order to look at the API located in the `api` directory. 
This API also follows the best practices for RESTful APi discussed in https://github.com/stephb9959/REST-API-Design-Guide. 
