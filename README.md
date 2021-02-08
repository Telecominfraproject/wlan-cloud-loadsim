![OWLS](images/owls.jpg?raw=true "OWLS")
# OWLS (Open WiFi Load Simulator)
This is a distrubuted system for creating load on a OpenWIFI system. The goal is to generate load over time, keep statistics
on time-outs, response times, and number of concurrent users. The system maybe distributed over several nodes to provide multiple 
realtime numbers.

## Getting started
### Hardware and software
This simulator requires multiple machines to run. Now, it can be bare-metal or VMs or docker. It does not really matter. Since the goal 
is to stress server resourcess, we would suggest to run the TIP controller on a much larger machine and these simulation node on VMs. 
As for support, this was developed on Debian based Linux distributions (Ubuntun and Debian). We have also done extensive 
testing on Mac OS X. Windows is not supported currently and there is no plans on supporting it.

### Security
This simulator should run behind your firewalls. There is no security between the management UI and the simulation manager. 
You could run the simulation entirely behind a firewall and run the TIP controller in the cloud. 

### Pre-requisites
This simulator uses Erlang. This language is designed to support thousands of processesand very suitable for this task. 
You must install Erlang OTP 22 or newer in order to run this application - unless you are using Docker.

#### Docker
If you intend to simply run the Docker version, please go to the [Docker](#running-docker) section. You do not need to install anything else. Simple cloning this repository
will give you all you need. 

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
make
```

### Choosing a node type
In a simulation, you have 3 types of nodes. 

#### `simmanager` node
There is only one `simmanager` node. This node is responsible for directing all the other nodes in the simulation. It is a 
central management point and gathers all the data about the simulation. You will be interacting with the `simmanager` through a 
command line interface or a web UI. If you wish to start a `simmanager` node, you should do the following and answer the questions
for the initial configuration.

```
./simmanager_config
./start
```

#### `simnode` node
You can have multiple `simnode` nodes. Each of these nodes can be started on the same host, or a number of other virtual or physical machines.
Once a `simnode` is running, you will be able to monitor it trough a command line interface or a local web UI. If you wish to start a `simnode`, please 
follow these instructions and answer the questions for the initial configuration.

```
./simnode_config
./start
```

#### `simmonitor` node
You should have a single 'simmonitor' node. This node is intended to run on the actual TIP controller server. It's only purpose in life is to report OS details back into the 'simmanager' node. This is then displayed in the UI to monitor the laod experienced on the TIP controller server.

```
./simmonitor_config
./start
```

#### On running multiple node types on a single machine
If you wish to run multiple nodes on a single host, you should run this from multiple copies of the repository code. 

```
mkdir ~/projects
cd ~/projects
mkdir simnode1
cd simnode1
git clone https://github.com/telecominfraproject/wlan-cloud-loadsim
cd wlan-cloud-loadsim
make
./simnode_config
./start
```

in another terminal window

```
cd ~/projects
mkdir simnode2
cd simnode2
git clone https://github.com/telecominfraproject/wlan-cloud-loadsim
cd wlan-cloud-loadsim
make
./simnode_config
./start
```

If you want to run multiple nodes on a single machine, you need to make sure that your number numbers are all different. 
If you run multiple on the same machine within different VMs, the node number is not as important. 

#### Simple check on each node
In order for all the nodes to participate in the simulation, you should start all the nodes with the proper node 
type for the local node by running either 'simmanager', 'simnode', or 'simmonitor'. Now from the prompt, 
you should be able to type the following:

```erlang
(simmonitor10@host1.local)1> nodes().
['simmanager@host1.local',
'simnode1@host2.local']
(simmonitor10@host1.local)2>
```

It will take about 10-30 seconds for all the nodes to be visible. If you see all the nodes, you should now 
proceed on planning your simulation. If some nodes are missing, you should use `ping hostname` to see if the nodes 
are visible and able to reach other by name. You must be using names. You should modify `/etc/hosts` file
in order to make sure that you can reach each host participating in the simulation by name. 

#### About the Network cookie
For nodes to accept communication between each other, they must share the same `cookie`. You can change this 
in the `config/simmanager.args` or the `config/simnode.args`. The `cookie` provides light security for
each nodes behind the firewall.

```erlang
-setcookie oreo
```

Whatever value you pick, you will need to enter the same value on all the additional hosts (`simmanager`,
`simmonitor`, and `simnode`) that will participate in this simulation. In the case, replace `oreo` with your favorite 
password. Please note that this simulation is not meant to run across the internet and is expected to run behind 
firewalls. Security is beyond the scope of this project.

## Your CA (Certificate Authority)
When you configured your TIP Controller, you created a number of keys and certificates. In order for this simulator to
create keys and certificates that are compatible with your installation of the TIP controller, you will need to import
the CA key and the certificate. Make sure you have the password you used during that configuration. Usually, that password 
has been set to 'mypassword'. You will need this information to import your CA in the UI.

## Planning the simulation
In order to create a successful simulation, a bit of planning is necessary. Here is what you will need:
- 1 `simmanager` node
- 1 or more `simnode`
- 1 `simmonitor` node

Whether the node is a `simmanager`,`simnode`, or `simmonitor`, you will need to have a copy of this repository. Therefore, 
if you use different physical hosts, you just need to clone this repository. If you plan on running multiple nodes on 
a single host, you should clone this repo in a separate directories for each node.

### Creating the `simmanager` node
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
Please enter the WEB UI port [9090] :
```
All the values between brackets are the default values. The most important value is the host part of the node name. You must be able 
to `ping` any host used as a node for this simulation. 

Once the `simmanager` is started, you should be able to start it like this:
```
./start
heart_beat_kill_pid = 17839
Erlang/OTP 23 [erts-11.1.1] [source] [64-bit] [smp:16:16] [ds:16:16:10] [async-threads:5] [hipe] [dtrace]

Eshell V11.1.1  (abort with ^G)
(simmanager@renegademac.arilia.com)1>
```
The prompt should show the node name you entered when you configured the node initially.

### Creating the `simnode` nodes
In order to create the `simnode` nodes you need to clone the repo and launch the `simnode_config` command. 
The command will ask you for several questions. In many cases the default values are just fine. Here's an example:

```
cd ~
github clone https://github.com/stephb9959/owls
cd owls
make
./simnode_config
Please enter a node number(1..99) [1] :
Please enter a node name [simnode1@renegademac.arilia.com] :
Please enter a network cookie [oreo] :
Please enter a directory name [/Users/stephb/Desktop/Dropbox/dhcp/test_repos3/owls] :
```
All the values between brackets are the default values. The most important value is the host part of the node name. 
You must be able to `ping` any host used as a node for this simulation. 

Once the `simnode` is started, you should be able to start it like this:
```
./simnode
heart_beat_kill_pid = 17839
Erlang/OTP 23 [erts-11.1.1] [source] [64-bit] [smp:16:16] [ds:16:16:10] [async-threads:5] [hipe] [dtrace]

Eshell V11.1.1  (abort with ^G)
(simnode1@renegademac.arilia.com)1>
```

### Creating the `simmonitor` node
The `simmonitor` node does nothing really. It is useful to report the amount of memory and additional resources are
available on the TIP controller. To configure the `simmonitor` node, simply do the following:

```
user/home > ./simmonitor_config
```

Please answer the simple questions.

## How to run a simulation
You should start the UI by entering `http://<host of the simmanager node>:9090`. You should get something similar to the 
following screen (some slight changes may have occurred since the release of the document).

## The steps

### Import you CA first
Using the dialog, please use your `cakey.pem` and `cacert.pem` files and import the CA. Let's give the CA the name of 
`tip1`. 

### Create the simulation
A simulation must have a name, like '`sim`. No spaces are allowed. Enter the name of the CA you created in the previous step.
Enter the number of APs you want to simulate. Depending on the size of your TIP controller, you should select the proper number.
Let's choose 100 for this setup.

- Name: the simulation name.
- CA: the name of the CA your created in the previous step
- Number of devices: the number of Access Points you want to simulate (100 to start).
- Server name: the name of your TIP controller server. You must be able to ping that name from each node in your simulation.
- Port: the port to use for the TIP controller. 6643 is usually the default. 

Once the simulation record is created, you are one step closer. 

### Prepare the assets
Onec the simulation parameters have been established (previous step), now you need to create the actual Access Points. 
You do this with `prepare assets`. Just make sure you select the simulation name from the previous step. This step may take 
some time, depending on how many APs you are creating. (200 may take 1-2 minutes).

### Push the simulation 
Once all the assets are created and exist, you need to push them to the `simnode` nodes. Just press the `push assets` 
buton and select your simulation name. This step is very quick usually. (less than 5 seconds for 2,000 devices)

### Start the simulation
This will tell all the `simnode` nodes to start their set of APs. And this is where the magic happens. At this point,
the simulation nodes will start chatting with your TIP controller. You should start to see devices and access points 
appear when you select the `network` menu choice. Be mindful that the TIP controller may take several seconds or maybe minutes 
to display all the data the load simulator produces. 

## Running Docker
Running is the sinmplest and fastest way to run this application. We tried to make this a simple as possible. You should be familiar enough with Docker first.
No need to be an expert. You will need to tailor one script and you should be off to the races. Any cons in running Docker? Of course. You will be losing the 
interactive CLI provided by the Erlang emulator. You may also lose some flexibility. These are small prices to pay for the ease of use. 

### Getting Docker
You should first install Docker for you platform. This document will no go into details on how to install Docker. Simply follow the instructions for your platform. 

### The main script
The script you will need to tailor is called `docker_run_net.sh`. Here is the content of this script so we can go over what you need to modify:

```
#!/bin/sh

HUBNAME=tip-tip-wlan-cloud-loadsim.jfrog.io
IMAGE_NAME=tip-owls-1
DOCKER_NAME=$HUBNAME/$IMAGE_NAME

NET_NAME=owls_net
# You must set this to the resolvable name of your TIP controller
TIP_CONTROLLER_HOST=debfarm1-node-a.arilia.com
TIP_CONTROLLER_IP=10.3.11.1
# This is the name of the host providing API access. In some cases, it may be the same as the TIP controller host name
TIP_API_HOST=debfarm1-node-a.arilia.com
TIP_API_IP=10.3.11.1

SIM_SCRIPT=/scripts/simulation.yaml

# clean networks and create the testing network
docker network prune --force
docker network create \
  --driver=bridge \
  --subnet=172.21.0.0/16 \
  --ip-range=172.21.10.0/24 \
  --gateway=172.21.0.1 \
  $NET_NAME

#stop previously running images
docker container stop manager node1
docker container rm manager node1 --force

#create directories for logs
rm -rf docker_logs_manager
rm -rf docker_logs_node1

mkdir docker_logs_manager
mkdir docker_logs_node1

HOSTNAMES="--add-host mgr.owls.net:172.21.10.2 --add-host node1.owls.net:172.21.10.3 --add-host $TIP_CONTROLLER_HOST:$TIP_CONTROLLER_IP --add-host $TIP_API_HOST:$TIP_API_IP"

#
# A simulation file called sim1.yaml is used to describe how a simulation should run. Here is the content...
#
# simulation:
#   name: sim1
#   ca:
#     name: tip1
#     cert: /etc/ssl/tipcert.pem   (this file should be in the $PWD/ssl dir)
#     key: /etc/ssl/tipkey.pem     (this file should be in the $PWD/ssl dir)
#     password: mypassword
#   server: (should be the same name as TIP_CONTROLLER_NAME
#   port: 6643
#   devices: 10
#
# To run a script, please replace the host line with this one in your manager section
#
# -e ERL_NODE_NAME="mgr@mgr.owls.net" -e ERL_OPTIONS="-noshell -noinput -tipauth 1 -tipapi $TIP_API_HOST -sim $SIM_SCRIPT " -e ERL_NODE_TYPE="manager" \

#start manager node
docker run  -d -p 9091:9090 --init \
            --network=owls_net \
            --volume="$PWD/ssl:/etc/ssl/certs" \
            --volume="$PWD/docker_logs_manager:/app_data/logs" \
            --volume="$PWD/scripts:/scripts" \
            -e ERL_NODE_NAME="mgr@mgr.owls.net" -e ERL_OPTIONS="-noshell -noinput -tipauth 1 -tipapi $TIP_API_HOST -sim $SIM_SCRIPT" -e ERL_NODE_TYPE="manager" \
            --ip="172.21.10.2" $HOSTNAMES \
            --name="manager" $DOCKER_NAME

#start simulation node
docker run  -d --init \
            --network=owls_net \
            --volume="$PWD/ssl:/etc/ssl/certs" \
            --volume="$PWD/docker_logs_node1:/app_data/logs" \
            -e ERL_NODE_NAME="node1@mgr.owls.net" -e ERL_OPTIONS="-noshell -noinput -tipauth 1 -tipapi $TIP_API_HOST" -e ERL_NODE_TYPE="node" \
            --ip="172.21.10.3" $HOSTNAMES \
            --name="node1" $DOCKER_NAME

```

#### NET_NAME
The is the name of the network created to contain all the nodes participating in the simulation. You may change this to anything you like. 

#### DOCKER_NAME
This is the name of the docker image on dockerhub. You should not change this image unless you are an expert or have beene asked by one of us to do it.

#### TIP_CONTROLLER_HOST
The tip FQDN. The FQDN is only used in the creation of the simulation. This name should resolve on your network. The docker image does not have or need to resolve this.

#### TIP_CONTROLLER_IP
The IPv4 of the TIP Controller. This is the IP the simulation nodes will try to reach.

#### TIP_API_HOST
The hostname of the TIP API Endpoint. This _could_ be the same as the TIP_CONTROLLER_HOST, depending on the deployment style you are using.

#### TIP_API_IP
The IPv4 of the TIP API Endpoint.

#### TIP_AUTH
The first version of this software was built for a pre-release of the TIP controller which used an older form of authentication. You should set this to "1" if you use the community software, or "2" if you use the proprietary version.

#### SIM_SCRIPT
If you want to run an automated script, you should use this option and put the file name where this script is. On docker, your should put this 
file in your $PWD/script directory, in your `simulation.yaml` file, the name then becomes `/scripts/simulation.yaml`. If you dono want to run a script, you must remove the following from your docker line in the manager section: `-sim $SIM_SCRIPT`.

### What this script does...
This script first removes all unneeded networks. It then creates the docker network that this simulation will be using. After this, the manager and node1 container will be stopped if they are running (from a aprevious run for example or an older version). The old containers are then removed. The log directory for each node is then created. `HOSTNAME` simply declares the hosts in the simulation. After which, the manager node and the simulation node are created. The script wil launch 2 containers: manager and node1. 

### Is Docker running?
If everything is running, you should see something like this with you enter the `docker ps` command.

```
stephb@debfarm1-node-c:~$ docker ps
CONTAINER ID   IMAGE                   COMMAND                  CREATED         STATUS         PORTS                              NAMES
d5654262b17a   stephb9959/tip-owls-1   "/bin/sh -c /owls/do…"   6 seconds ago   Up 5 seconds   4369/tcp, 9090/tcp                 node1
a00f770c4fc4   stephb9959/tip-owls-1   "/bin/sh -c /owls/do…"   8 seconds ago   Up 5 seconds   4369/tcp, 0.0.0.0:9091->9090/tcp   manager
stephb@debfarm1-node-c:~$
```

### Accessing the UI
To access the UI, simply go to http://hostname-where-docker-is-running:9091. In our case, `http://debfarm1-node-c.arilia.com:9091` and follow the instructions. 

### Accessing the API with Docker
The API is available at the same address as the UI and the same port.

### Important note on running Docker
The system needs to downlaod and parse the OUI DB stored ate Linux.net. This usually takes about 1 minute. So please wait 1 minutes after starting the simulation for this to happen. Once this has happened, you can run your simulation. We are working on a small improvement that will render this unneccessary. 

### Running an automated script from Docker
You can specify an automated script to run from docker by using the `-e SIM_SCRIPT="scriptname.yaml"` in your docker_run_net.sh file for your manager node. The `scriptname.yaml` file should contains the following:

```
simulation:
  name: sim1
  ca:
    name: tip1
    cert: /etc/ssl/certs/tip-cacert.pem
    key: /etc/ssl/certs/tip-cakey.pem
    password: mypassword
  server: debfarm1-node-a.arilia.com
  port: 6643
  devices: 10
```
The files in `cert` and `key` are from the perspective of the running container. Since the certificate information must be placed in
the `$PWD/ssl` directory and this is mapped to `/etc/ssl/certs` in the container, this must appear in the `sim1.yaml` file. 

The scripts should also be put in `$PWD/scripts`, which is mapped to `/scripts` in the container. So the filename you must on on the `docker run` 
line must be something like `/scripts/simulation.yaml`.

Your docker_run_net.sh file should have the following in your manager node section:
```
SIM_SCRIPT=/scripts/simulation.yaml

docker run  -d -p 9091:9090 --init \
            --network=owls_net \
            --volume="$PWD/ssl:/etc/ssl/certs" \
            --volume="$PWD/docker_logs_manager:/app_data/logs" \
            --volume="$PWD/scripts:/scripts" \
            -e ERL_NODE_NAME="mgr@mgr.owls.net" -e ERL_OPTIONS="-noshell -noinput -tipauth 1 -tipapi $TIP_API_HOST -sim $SIM_SCRIPT" -e ERL_NODE_TYPE="manager" \
            --ip="172.21.10.2" $HOSTNAMES \
            --name="manager" $DOCKER_NAME
```

## API
This project uses OpenAPI specification 3.0.03, and you can use Swagger (https://editor.swagger.io/) in order to 
look at the API located in the `api` directory. This API also follows the best practices for REST APi discussed in
https://github.com/NationalBankBelgium/REST-API-Design-Guide/wiki. 
