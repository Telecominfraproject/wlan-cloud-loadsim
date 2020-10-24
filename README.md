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


 
