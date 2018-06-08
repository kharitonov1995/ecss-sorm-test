FROM ubuntu:xenial
LABEL author="Alexander Demidenko"
LABEL description="ECSS-10 Toolchain with Erlang"
LABEL maintainer="Eltex"

ENV \
    USER=eltex \
    TERM=xterm \
    TZ=Asia/Novosibirsk \
    BU_LOCAL=true \
    BU_GENERATION=201805031201 \
    ECSS_UID=33339 \
    ECSS_GID=33339

#ADD /home/${USER}/.gitconfig /home/${USER}/
#ADD /home/${USER}//.gitignore /home/${USER}/

RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone
RUN adduser --disabled-password --gecos '' ${USER}
RUN \
    addgroup --quiet --system --gid "${ECSS_GID}" ssw &&\
    adduser --quiet --system --home "/var/lib/ecss" --shell /bin/false --uid "${ECSS_UID}" --gid "${ECSS_GID}" --disabled-password --disabled-login --gecos "Eltex ECSS-10 system account" ssw &&\
    adduser ${USER} ssw &&\
    chown -Rc ${USER}:${USER} "/home/${USER}/" &&\
    echo "GPFOOPBJQGHXVEZZUOPG" > "/home/${USER}/.erlang.cookie" &&\
    echo "[{debug_info, des3_cbc, [], \"Eltex_ECSS-10_m/kltHhB8S0l//Wd+E42oMIvH4ULZWgMCuVNNWNMIpM=\"}]." > "/home/${USER}/.erlang.crypt" &&\
    chmod a-rwx,u+r "/home/${USER}/.erlang.crypt" "/home/${USER}/.erlang.cookie" &&\
    echo 'Acquire::http::Proxy "http://ubuntu.resource.eltex.loc:3142";' > /etc/apt/apt.conf.d/00aptproxy &&\
    sed -i -e "s/^deb-src/#deb-src/" /etc/apt/sources.list

RUN \
    apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 33CB2B750F8BB6A5 &&\
    echo "deb [arch=amd64] http://rep.resource.eltex.loc dev main" > /etc/apt/sources.list.d/eltex-dev-tools.list &&\
    mkdir -p /etc/sudoers.d/ &&\
    echo "${USER} ALL=NOPASSWD: ALL" > /etc/sudoers.d/${USER} &&\
    DEBIAN_FRONTEND=noninteractive \
        apt-get update &&\
    DEBIAN_FRONTEND=noninteractive \
        apt-get --quiet --yes --no-install-recommends install \
            apt-utils bash-completion chrpath curl debconf debhelper devscripts dialog dirmngr dh-systemd dpkg dput-ng \
            fakeroot gcc git libexpat1-dev libp11-dev libpam0g-dev libsystemd-dev lintian locales tzdata make ssh \
            dh-make build-essential automake gnupg python-paramiko sudo \
            inotify-tools iputils-ping libxml2-utils libgraph-easy-perl libpcsclite1 mc \
            erlang-base-hipe erlang-dev erlang-src erlang-eunit erlang-parsetools \
            erlang-syntax-tools erlang-asn1 erlang-snmp erlang-inets erlang-eldap erlang-tools erlang-ssh erlang-os-mon erlang-snmp erlang-megaco erlang-xmerl &&\
    DEBIAN_FRONTEND=noninteractive \
        apt-get --quiet --yes upgrade &&\
    DEBIAN_FRONTEND=noninteractive \
        apt-get clean &&\
    locale-gen en_US.UTF-8 ru_RU.UTF-8 &&\
    update-locale LANG=en_US.UTF-8 &&\
    mkdir -p /usr/lib/erlang/lib &&\
    curl http://files.resource.eltex.loc/erlang-utils/dev-utils-3.11.tar.gz --silent | tar -xzC /usr/bin &&\
    curl http://files.resource.eltex.loc/erlang-lib/lib-3.11.tar.gz --silent | tar -xzC /usr/lib/erlang/lib &&\
    curl http://files.resource.eltex.loc/ecss-toolchain-lib/ecss-toolchain-lib.tar.gz --silent | tar -xzC / &&\
    curl http://files.resource.eltex.loc/pkcs11/libpkcs11-proxy.so --silent --output /usr/lib/libpkcs11-proxy.so &&\
    rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

WORKDIR /home/${USER}/project
VOLUME ["/home/${USER}/project"]
CMD ["/bin/run.sh /bin/bash"]

LABEL version="3.11"
