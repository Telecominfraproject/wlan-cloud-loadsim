# mqttsim
Erlang based MQTT simulator

## Getting started
### Pre-requisites
You must install Erlang OTP 22 or newer. 
#### Ubuntu
```
sudo apt install erlang
```
#### OS X
```
brew install erlang 
```
#### Windows
The systeme currently does not support Windows

### Verifying Erlang is available
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
./mqtt_conf `pwd`
```

### Basic configuration
#### Hostname
The file `config/vm.args` contains the only value you must change. Near the op pf the file, you will find the following
```
-name mqttsim@renegademac.local
```
You must change that value. This is NOT an email address. The first part is used to locate the VM on a given host. `sim1` or something like that is good. Next, change the hostname portion of that entry to the hostname where you are running the simulator. This must be an FQDN (Fully Qualified Domain Name). This just means that the hostname has to contain at least 1 period. Do not use `localhost`. Usually you can use your PC name followed by `.local`. You can try to `ping` that name to see if your PC can find it. This step is critical. If all fails, enter something in your `/etc/hosts` file.
#### Network cookie
```
-setcookie oreo
```
You can change the cookie for erlang distribution. Whatever value you pick, you will need to enter the same value on all the additional hosts 
that will participate in this simulation. In the case, replace `oreo` with your favorite password. Please note that this simulation is not meant 
to run accross the internet and is expected to run behind firewalls. Security is beyond the scope of this project.
#### Custom CA configuration
Once you run `mqtt_conf`, you will get a customized configuration file located 
in the config directory. The file `sys.config` contains most of the settings. You can change the `ca_name` line to reflect the name
of the ca to use. The default is `sim_ca`. This value is cosmertic only.

 
