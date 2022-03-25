# /bin/bash
wget https://packages.erlang-solutions.com/erlang-solutions_2.0_all.deb
sudo dpkg -i erlang-solutions_2.0_all.deb
sudo apt-get update
sudo apt install esl-erlang=1:22.3.4.1-1 cmake libsodium-dev libssl-dev build-essential cargo -y
./rebar3 compile && ./rebar3 release