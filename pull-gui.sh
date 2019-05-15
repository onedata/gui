#!/usr/bin/env bash

#####################################################################
# @author Lukasz Opiola
# @copyright (C) 2016 ACK CYFRONET AGH
# This software is released under the MIT license
# cited in 'LICENSE.txt'.
#####################################################################
# usage:
# ./pull-gui.sh <path to gui-config>
#
# This script copies static GUI package from a docker image to specified
# location. Requires configuration file that defines the target path and the
# docker image that should be used.
#
# Requires a gui config file - sh script exporting following envs:
#   TARGET_PATH
#   PRIMARY_IMAGE
#   SECONDARY_IMAGE
#####################################################################

PACKAGE_PATH_IN_DOCKER='/var/www/html/gui_static.tar.gz'

# If docker command is not present, just skip gui pull and continue.
command -v docker >/dev/null 2>&1 || {
    echo "WARNING: docker client not found, continuing without pulling gui." >&2;
    exit 0;
}

# Check if the config file is given
if [[ ! -f "${1}" ]]; then
    echo "Usage:"
    echo "    ./pull-gui.sh <path-to-gui-config>"
    exit 1
fi

# Source gui config which should contain following exports:
TARGET_PATH=''
PRIMARY_IMAGE=''
SECONDARY_IMAGE=''
source ${1}

if [[ -z ${TARGET_PATH} ]]; then
    echo "TARGET_PATH not defined in ${1}, aborting"
    exit 1
fi
if [[ -z ${PRIMARY_IMAGE} ]]; then
    echo "PRIMARY_IMAGE not defined in ${1}, aborting"
    exit 1
fi

STATIC_FILES_IMAGE=${PRIMARY_IMAGE}
docker pull ${STATIC_FILES_IMAGE} 2>/dev/null
if [ $? -ne 0 ]; then
    echo "Cannot pull primary docker image for static GUI files - falling back to secondary"
    if [[ -z ${SECONDARY_IMAGE} ]]; then
        echo "SECONDARY_IMAGE not defined in ${1}, aborting"
        exit 1
    fi
    STATIC_FILES_IMAGE=${SECONDARY_IMAGE}
    docker pull ${STATIC_FILES_IMAGE} 2>/dev/null
    if [ $? -ne 0 ]; then
        echo "Cannot pull primary nor secondary docker image for static GUI files. Exiting."
        exit 1
    fi
fi

set -e

echo "Copying static GUI package"
echo "    from image: ${STATIC_FILES_IMAGE}"
echo "    under path: ${TARGET_PATH}"

CONTAINER_ID=`docker run --detach ${STATIC_FILES_IMAGE} /bin/true`

docker cp -L ${CONTAINER_ID}:${PACKAGE_PATH_IN_DOCKER} ${TARGET_PATH}

docker rm -f ${CONTAINER_ID}
