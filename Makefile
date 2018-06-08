#!make
.PHONY: all local cli toolchain build deb publish down export import raw-toolchain raw-build raw-deb raw-publish

# Внимание, скрипт мантирует ~/.ssh для выполнения публикации на сервер репозитория.
# соответственно он будет это делать с правами пользователя, от имени которого был запущен процесс Make
# Текущий каталог с проектом монитруется в контейнер по пути /home/eltex/project

# Парсим имя пакета и номер версии из внешнего файла package-vsn
PACKAGE = $(shell cat "./package-vsn")
PACKAGE_NAME = $(firstword $(subst " ", ,${PACKAGE}))
MAJOR_VSN = $(or $(word 2,$(subst " ", ,${PACKAGE})),$(value 2))
# Получаем полный путь к клиенту докера
DOCKER = $(shell which docker)
# При работе в контексте Jenkins, нам не нужен интерактивный режим и эмуляция TTY
ifndef JOB_BASE_NAME
	USE_TTY = -ti
endif
# Формируем внутренние переменные
TOOLCHAIN="eltex/${PACKAGE_NAME}-toolchain:${MAJOR_VSN}"
TOOLCHAIN-FILE="eltex-${PACKAGE_NAME}-${MAJOR_VSN}-toolchain.tar.gz"

all: toolchain build deb

local: import build deb

tc: toolchain

docker-req:
	@echo ""

cli: docker-req
	@${DOCKER} run \
		--volume "${PWD}":/home/eltex/project \
		--volume "${HOME}"/.ssh:/home/eltex/.ssh \
		--tmpfs /tmp:exec,size=2G \
		--env UID=$(shell id -u) \
		--env GID=$(shell id -g) \
		--privileged \
		--rm \
		${USE_TTY} ${TOOLCHAIN} \
		/bin/run.sh /bin/bash

toolchain: docker-req
	@${DOCKER} build \
		--tag=${TOOLCHAIN} \
		--rm \
		./

build: docker-req
	@${DOCKER} run \
		--volume "${PWD}":/home/eltex/project \
		--volume "${HOME}"/.ssh:/home/eltex/.ssh \
		--tmpfs /tmp:exec,size=2G \
		--env UID=$(shell id -u) \
		--env GID=$(shell id -g) \
		--privileged \
		--rm \
		${USE_TTY} ${TOOLCHAIN} \
		/bin/run.sh /usr/bin/make raw-build

deb: docker-req
	@${DOCKER} run \
		--volume "${PWD}":/home/eltex/project \
		--volume "${HOME}"/.ssh:/home/eltex/.ssh \
		--tmpfs /tmp:exec,size=2G \
		--env UID=$(shell id -u) \
		--env GID=$(shell id -g) \
		--privileged \
		--rm \
		${USE_TTY} ${TOOLCHAIN} \
		/bin/run.sh /usr/bin/make raw-deb

publish: docker-req
	@${DOCKER} run \
		--volume "${PWD}":/home/eltex/project \
		--volume "${HOME}"/.ssh:/home/eltex/.ssh \
		--tmpfs /tmp:exec,size=2G \
		--env UID=$(shell id -u) \
		--env GID=$(shell id -g) \
		--privileged \
		--rm \
		${USE_TTY} ${TOOLCHAIN} \
		/bin/run.sh /usr/bin/make raw-publish

clean:
	@rm -rf "./${PACKAGE_NAME}" ./release ./ebin ./deps ./*.deb ./*.changes ./*.build ./*.buildinfo ./*.upload ./*.db || true

down:
	@${DOCKER} rm ${TOOLCHAIN} || true
	@${DOCKER} rmi -f ${TOOLCHAIN} || true

export:
	@echo "Exporting toolchain..."
	@${DOCKER} save --output ${TOOLCHAIN-FILE} ${TOOLCHAIN}

import:
	@echo "Importing toolchain..."
	@${DOCKER} load --input ${TOOLCHAIN-FILE}

up:
	@${DOCKER} run \
		--volume "${PWD}":/home/eltex/project \
		--volume "${HOME}"/.ssh:/home/eltex/.ssh \
		--tmpfs /tmp:exec,size=2G \
		--env UID=$(shell id -u) \
		--env GID=$(shell id -g) \
		--privileged \
		--rm \
		${USE_TTY} ${TOOLCHAIN} \
		/bin/run.sh /usr/bin/make raw-up

##########################################################################
raw-deb:
	@echo "Creating deb-package"
	@/bin/create-deb.sh

raw-build:
	@echo "Updating node modules..."
	rm -rf release
	rebar compile
	relx release
	@chmod +x release/${PACKAGE_NAME}/bin/install_upgrade.escript
	@chmod +x release/${PACKAGE_NAME}/bin/nodetool
	@echo "Done"

raw-publish:
	@echo "Publishing deb-package to unstable repo"
	@dput -U unstable *.changes

raw-up:
	@rebar delete-deps
	@rebar get-deps