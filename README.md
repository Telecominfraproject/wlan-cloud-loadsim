![OWLS](images/owls.jpg?raw=true "OWLS")
# OWLS (Open WiFi Load Simulator)
This is a distrubuted system for creating load on a OpenWIFI system. The goal is to generate load over time, keep statistics
on time-outs, response times, and number of concurrent users. The system maybe distributed over several nodes to provide multiple 
realtime numbers.

## Getting started
### Hardware and software
This simulator requires multiple machines to run. Now, it can be bare-metal or VMs. It does not really matter. Since the goal is to stress server resourcess, we 
would suggest to run the TIP on a much larger machine and these simulation node on VMs. As for support, this was developed on Debian based Linux distributions (Ubuntun and Debian). We have also done extensive testing on Mac OS X. Windows is not supported currently and there is no plans on supporting it.

### Security
This simulator should run behind your firewalls. There is no security between the management UI and the simulation manager. You could run the simulation entirely behind a firewall and run the TIP controller in the cloud. 

### Pre-requisites
This simulator uses Erlang. This language is designed to support thousands of processesand very suitable for this task. You must install Erlang OTP 22 or newer in order to run this application.
#### Linux 
##### Ubuntu
```
sudo apt install erlang
```
##### Other Linux distributions
Please visit https://www.erlang-solutions.com/resources/download.html in order to get instructions for 
other Linux distributions and operating systems.

#### OS X
```
brew install erlang 
```

#### From source
Please visit https://erlang.org/doc/installation_guide/INSTALL.html to build Erlang from scratch.

#### Windows
Their is currently no plan to support Windows based hosts.

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
git clone https://github.com/telecominfroproject/wlan-cloud-loadsim
cd wlan-cloud-loadsim
```

### Choosing a node type
In a simulation, you have 3 types of nodes. 

#### simmanager node
There is only one `simmanager` node. This node is responsible for directing all the other nodes in the simulation. It is a 
central management point and gathers all the data about the simulation. You will be interacting with the `simmanager` through a 
command line interface or a web UI. If you wish to start a `simmanager` node, you should do the following and answer the questions
for the initial configuration.

```
./simmanager_config
./simnanager
```

#### simnode node
You can have multiple `simnode` nodes. Each of these nodes can be started on the same host, or a number of other virtual or physical machines.
Once a `simnode` is running, you will be able to monitor it trough a command line interface or a local web UI. If you wish to start a `simnode`, please 
follow these instructions and answer the questions for the initial configuration.

```
./simnode_config
./simnode
```

#### simmonitor node
You should have a single 'simmonitor' node. This node is intended to run on the actual TIP controller server. It's only purpose in life is to report OS details back into the 'simmanager' node. This is then displayed in the UI to monitor the laod experienced on the TIP controller server.

```
./simmonitor_config
./simmonitor
```

#### On running multiple node types on a single machine
If you wish to run multiple nodes on a single host, you should run this from multiple copies of the repository code. 

```
mkdir ~/projects
cd ~/projects
mkdir simnode1
cd simnode1
git clone https://github.com/stephb9959/owls
cd owls
./simnode_config
./simnode
```

in another terminal window

```
cd ~/projects
mkdir simnode2
cd simnode2
git clone https://github.com/stephb9959/owls
cd owls
./simnode_config
./simnode
```

Before starting the node, you must go and change the hostname and port for each node. On a `simnode`, you must change
the `-name` parameter in the `config/simnode.args` file. Each node must have a unique name. You must also change the `web_ui` port 
in the `config/simnode.config` file to a unique port.  

### Basic configuration
#### Hostname
The file `config/simmanager.args` or `config/simnode.args` contains the only value you must change. Near the top of the 
file, you will find the following commands:
```
-name simmanager@renegademac.local
```
You must change that value. This is NOT an email address. The first part is used to locate the VM on a given host. `simnode1` or 
something like that is good. Next, change the hostname portion of that entry to the hostname where you are running the simulator. 
This must be an FQDN (Fully Qualified Domain Name). This just means that the hostname has to contain at least 1 period. Do not use `localhost`. 
Usually you can use your PC name followed by `.local`. You can try to `ping` that name to see if your PC can find it. This step is critical. 
If all fails, enter something in your `/etc/hosts` file.

#### Network cookie
For nodes to accept communication between eachother, they must share the same `cookie`. You an change this in the `config/simmanager.args` or the `config/simnode.args`. 
```
-setcookie oreo
```
Whatever value you pick, you will need to enter the same value on all the additional hosts (simmanager and simnodes)
that will participate in this simulation. In the case, replace `oreo` with your favorite password. Please note that this simulation is not meant 
to run accross the internet and is expected to run behind firewalls. Security is beyond the scope of this project.
#### Custom CA configuration
Once you run `simmanager_config` or `simnode_config`, you will get a customized configuration file located in the config directory.

## Planning the simulation
In order to create a successful simualtion, a bit of planning is necessary. Here is what you will need:
- 1 `simmanager` node
- 1 or more `simnode`
Wether the node is a `simmanager` or `simnode`, you will need to have a copy of this repo. Therefore, if you use different physical hosts,
you just need to clone this repo. If you plan on running multiple nodes on a single host, you should clone this repo in a separate directory 
for each node.

### Creating the `simmanager`
In order to create the `simmanager` you need to clone the repo and launch the `simmanager_config` command. The command will ask you 
for several questions. In many cases the default values are just fine. Here's an example:

```
cd ~
github clone https://github.com/stephb9959/owls
cd owls
./simmanager_config
Please enter a node name [simmanager@renegademac.arilia.com] :
Please enter a network cookie [oreo] :
Please enter a directory name [/Users/stephb/Desktop/Dropbox/dhcp/test_repos3/owls] :
Please enter the WEB UI port [9091] :
```
All the values between brackets are the default values. The most important value is the host part of the node name. You must be able to `ping` any host used as a node for this simulation. 

Once the `simmanager` is started, you should be able to start it like this:
```
./simmanager
heart_beat_kill_pid = 17839
Erlang/OTP 23 [erts-11.1.1] [source] [64-bit] [smp:16:16] [ds:16:16:10] [async-threads:5] [hipe] [dtrace]

Eshell V11.1.1  (abort with ^G)
(simmanager@renegademac.arilia.com)1>
```
The prompt should show the node name you entered when you configured the node initially.

### Creating the `simnodes`
In order to create the `simnodes` you need to clone the repo and launch the `simnode_config` command. The command will ask you 
for several questions. In many cases the default values are just fine. Here's an example:

```
cd ~
github clone https://github.com/stephb9959/owls
cd owls
./simnode_config
Please enter a node number(1..99) [1] :
Please enter a node name [simnode1@renegademac.arilia.com] :
Please enter a network cookie [oreo] :
Please enter a directory name [/Users/stephb/Desktop/Dropbox/dhcp/test_repos3/owls] :
Please enter the WEB UI port(9096..9196) [9096] :
Please enter the OVSDB reflector port [6643] :
Please enter the OVSDB port [6640] :
```
All the values between brackets are the default values. The most important value is the host part of the node name. You must be able to `ping` any host used as a node for this simulation. 

Once the `simnode` is started, you should be able to start it like this:
```
./simnode
heart_beat_kill_pid = 17839
Erlang/OTP 23 [erts-11.1.1] [source] [64-bit] [smp:16:16] [ds:16:16:10] [async-threads:5] [hipe] [dtrace]

Eshell V11.1.1  (abort with ^G)
(simnode1@renegademac.arilia.com)1>
```

## API
This project uses OpenAPI specification 3.0, and you can use Swagger (https://editor.swagger.io/) in order to look at the API located in the `api` directory. 
This API also follows the best practices for RESTful APi discussed in https://github.com/NationalBankBelgium/REST-API-Design-Guide/wiki. 
