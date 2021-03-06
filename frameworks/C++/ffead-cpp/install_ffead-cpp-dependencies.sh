#!/bin/bash

apt update -yqq && apt install --no-install-recommends -yqq autoconf-archive unzip uuid-dev odbc-postgresql unixodbc unixodbc-dev \
	apache2 apache2-dev libapr1-dev libaprutil1-dev memcached libmemcached-dev redis-server libssl-dev \
	zlib1g-dev cmake make clang-format-9 ninja-build libhiredis-dev libcurl4-openssl-dev libpq-dev

#redis will not start correctly on bionic with this config
sed -i "s/bind .*/bind 127.0.0.1/g" /etc/redis/redis.conf

service apache2 stop
service memcached stop
service redis-server stop

cd $IROOT
git clone https://github.com/efficient/libcuckoo.git
cd libcuckoo
git checkout 8785773896d74f72b6224e59d37f5f8c3c1e022a -b works
cmake -DCMAKE_INSTALL_PREFIX=/usr .
make install
cd $IROOT
rm -rf libcuckoo

mkdir -p /usr/lib/x86_64-linux-gnu/odbc
wget -q https://downloads.mysql.com/archives/get/p/10/file/mysql-connector-odbc-8.0.19-linux-ubuntu18.04-x86-64bit.tar.gz
tar xf mysql-connector-odbc-8.0.19-linux-ubuntu18.04-x86-64bit.tar.gz
mv mysql-connector-odbc-8.0.19-linux-ubuntu18.04-x86-64bit/lib/libmyodbc8* /usr/lib/x86_64-linux-gnu/odbc/
mysql-connector-odbc-8.0.19-linux-ubuntu18.04-x86-64bit/bin/myodbc-installer -d -a -n "MySQL" -t "DRIVER=/usr/lib/x86_64-linux-gnu/odbc/libmyodbc8w.so;"
rm -f mysql-connector-odbc-8.0.19-linux-ubuntu18.04-x86-64bit.tar.gz
rm -rf mysql-connector-odbc-8.0.19-linux-ubuntu18.04-x86-64bit

#wget -q https://cdn.mysql.com/archives/mysql-connector-odbc-5.3/mysql-connector-odbc-5.3.11-linux-ubuntu16.04-x86-64bit.tar.gz
#tar xf mysql-connector-odbc-5.3.11-linux-ubuntu16.04-x86-64bit.tar.gz
#mv mysql-connector-odbc-5.3.11-linux-ubuntu16.04-x86-64bit/lib/libmyodbc5* /usr/lib/x86_64-linux-gnu/odbc/
#mysql-connector-odbc-5.3.11-linux-ubuntu16.04-x86-64bit/bin/myodbc-installer -d -a -n "MySQL" -t "DRIVER=/usr/lib/x86_64-linux-gnu/odbc/libmyodbc5w.so;"
#rm -f mysql-connector-odbc-5.3.11-linux-ubuntu16.04-x86-64bit.tar.gz
#rm -rf mysql-connector-odbc-5.3.11-linux-ubuntu16.04-x86-64bit

wget -q https://github.com/mongodb/mongo-c-driver/releases/download/1.4.2/mongo-c-driver-1.4.2.tar.gz
tar xf mongo-c-driver-1.4.2.tar.gz
rm -f mongo-c-driver-1.4.2.tar.gz
cd mongo-c-driver-1.4.2/ && \
    ./configure --disable-automatic-init-and-cleanup && \
    make && make install
cd $IROOT
rm -rf mongo-c-driver-1.4.2 

#wget -q https://github.com/redis/hiredis/archive/v0.13.3.tar.gz
#tar xf v0.13.3.tar.gz
#rm -f v0.13.3.tar.gz
#cd hiredis-0.13.3/
#make
#PREFIX=/usr make install
#cd $IROOT
#rm -rf hiredis-0.13.3

cd $IROOT
wget -q https://github.com/microsoft/mimalloc/archive/v1.6.3.tar.gz
tar xf v1.6.3.tar.gz
rm -f v1.6.3.tar.gz
cd mimalloc-1.6.3
mkdir -p out/release
cd out/release
cmake ../.. -DCMAKE_BUILD_TYPE=Release
make && make install
cd $IROOT
rm -rf mimalloc-1.6.3

wget -q https://github.com/microsoft/snmalloc/archive/0.4.2.tar.gz
tar xf 0.4.2.tar.gz
rm -f 0.4.2.tar.gz
cd snmalloc-0.4.2
mkdir build
cd build
cmake -G Ninja .. -DCMAKE_BUILD_TYPE=Release
ninja
cd $IROOT
